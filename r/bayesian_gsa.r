#================================
# Global sensitivity analysis using Sobols method in R using sensitivity package
#================================

library(sensitivity)
library(boot)
library(doParallel)
library(parallel)
library(foreach)
library(here)

library(ggplot2)

source(here::here("r/ipcct2_run.r"))
source(here::here("r/bayesian_loglike.r"))

gsa <- function(site_data,
								climate_data,
								initial_c,
								parameter_bounds,
								sample_size = 10,
								method = "soboljansen") {
	# methods include either "soboljansen" or "fast99"
	
	# site_data can either be a data.frame, or a list of data.frames
	if(!inherits(site_data, "list")) {
		site_data <- list(site_data)
	}
	#initial_c should always be a list of lists
	if(!any(sapply(initial_c, is.list))) {
		initial_c <- list(initial_c)
	}
	if(length(site_data) != length(initial_c)) {
		stop("site_data and initial_c cannot be different lengths")
	}
	
	
	
	# read prior distribution from a csv file
	# (Required columns: Parameter, value, lower, upper)
	paramBounds <- parameter_bounds
	
	# names of parameters that are allowed to vary
	varSI       <- paramBounds$Parameter
	nParams     <- length(varSI)
	
	# sample size (10 used for illustration purposes)
	# (1024 used in Gurung et al., 2020)
	N <- sample_size
	
	# Sobols method required 2 matrix
	m1 = matrix(runif(nParams*N), nrow=N);
	m2 = matrix(runif(nParams*N), nrow=N);
	M1 <- matrix(0, nrow = N, ncol = nParams)
	M2 <- matrix(0, nrow = N, ncol = nParams)
	
	# transform standard uniform to prior distribution 
	for(i in 1:nParams){
		pos <- which(paramBounds$Parameter == varSI[i])
		lower <- paramBounds[pos, "lower"]
		upper <- paramBounds[pos, "upper"]
		M1[, i] <- qunif(m1[, i], min = lower, max = upper)
		M2[, i] <- qunif(m2[, i], min = lower, max = upper)
	}
	X1 = data.frame(M1)
	X2 = data.frame(M2)
	names(X1) <- varSI
	names(X2) <- varSI
	
	# choose a sensitivity method of your choice from the sensitivity package in R.
	# see documentation for available options.
	if(method == "fast99") {
		fast99_qargs <- paramBounds %>%
			group_by(Parameter) %>%
			select(Parameter, min = lower, max = upper) %>%
			group_split %>%
			map(function(x) {y <- x %>%
				select(-Parameter) %>%
				as.list()
			})
	}
	
	
	si_obj2 <- switch(method,
										soboljansen = sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2, nboot = 100),
										fast99 = sensitivity::fast99(model = NULL,
																								 factors = paramBounds$Parameter,
																								 n = N, M = 4,
																								 q="qunif",
																								 q.arg = fast99_qargs))
	
	X <- si_obj2$X
	n <- nrow(X)
	X <- cbind("SampleID" = 1:nrow(X), X)
	
	params_list_sorted_names <- c("SampleID",varSI)
	params_list <- X %>%
		rowwise %>%
		group_split %>%
		map(function(x) {
			y <- x %>%
				pivot_longer(everything()) %>%
				deframe()
			z <- split(unname(y),names(y))
			return(z)
		}) %>%
		map(~ .[params_list_sorted_names])
	
	# Run the model and calculate log-likelihood
	# likelihoods were calculated assuming that the error (modeled - mesasured) are iid 
	
	Lkhood <- NULL
	Lkhood_list <- list()
	
	for(site_n in 1:length(site_data)) {
		ncores=parallel::detectCores()-2
		cl=parallel::makeCluster(ncores)
		doParallel::registerDoParallel(cl)
		
		Lkhood=foreach(i=1:nrow(X), 
									 .combine = rbind, 
									 .packages = c("parallel", 
									 							"doParallel", 
									 							"tidyverse"),
									 .export = c("run_ipcct2",
									 						"IPCCTier2SOMmodel",
									 						"loglik",
									 						"run_ipcct2_calculate_loglik")) %dopar%
			
			run_ipcct2_calculate_loglik(site_data = site_data[[site_n]],
																	climate_data = climate_data,
																	init_active = initial_c[[site_n]]$init_active,
																	init_slow = initial_c[[site_n]]$init_slow,
																	init_passive = initial_c[[site_n]]$init_passive,
																	i)
		stopCluster(cl)
		Lkhood_list[[site_n]] <- Lkhood
		print(paste0("gsa: site ", site_n, "/", length(site_data), " (sample_size = ",sample_size,")"))
	}
	
	
	# # code for non-paralleled run (defunct)
	# Lkhood_list <- list()
	# for(i in 1:length(params_list)){
	# 	Lkhood_list[[i]] <- run_ipcct2_calculate_loglik(params_list[[i]])
	# }
	
	Lkhood1 <- Lkhood_list %>%
		bind_rows %>%
		mutate(loglik = ifelse(loglik == -Inf, NA, loglik)) %>%
		group_by(id) %>%
		summarise(loglik = mean(loglik, na.rm=T))
	
	si_obj2_llkhd <- sensitivity::tell(x = si_obj2, y = Lkhood1$loglik)
	
	if(method == "soboljansen")
	{
		# Calculate First-order and Total global sensitivity indices
		singleSI <- si_obj2_llkhd$S %>%
			select(singsi = original, singsi.lci = `min. c.i.`, singsi.uci = `max. c.i.`) %>%
			rownames_to_column("params") %>%
			as_tibble()
		totalSI <- si_obj2_llkhd$T %>%
			select(totsi = original, totsi.lci = `min. c.i.`, totsi.uci = `max. c.i.`) %>%
			rownames_to_column("params") %>%
			as_tibble()
		
		combined_si <- totalSI %>%
			full_join(singleSI, by=c("params"))
		
		return_si <- combined_si
	}
	if(method == "fast99")
	{
		return_si <- tibble(main = si_obj2_llkhd$D1 / si_obj2_llkhd$V,
												interactions = 1 - si_obj2_llkhd$Dt / si_obj2_llkhd$V) %>%
			mutate(params =  varSI, .before=main)
	}
	
	return(return_si)
	
}
