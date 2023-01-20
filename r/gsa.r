#================================
# Global sensitivity analysis using Sobols method in R using sensitivity package
#================================

library(sensitivity)
library(boot)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(here)

source(here::here("r/ipcct2_run.r"))
source(here::here("r/gsa_loglike.r"))

gsa <- function(site_data,
								climate_data,
								parameter_bounds,
								sample_size = 10) {
	# site_data can either be a data.frame, or a list of data.frames
	if(!inherits(site_data, "list")) {
		site_data <- list(site_data)
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
	#    - soboljansen is used for illustration here
	
	si_obj2 <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2, nboot = 100)
	
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
	
	
	# End of sample generation for GSA
	
	# Run the model and calculate log-likelihood
	#    - likelihoods were calculated assuming that the error (modeled - mesasured)
	#      are iid 
	
	
	run_ipcct2_calculate_loglik <- function(site_data, parameters) 
	{
		id <- parameters[[1]] #gets id, hopefully (I think foreach::foreach coerces rows into unnamed vectors)
		modelled <- do.call("run_ipcct2",
												append(list(site_data, climate_data,
																		init_active = 0,
																		init_slow = 0,
																		init_passive = 0),
															 parameters))
		actuals <-  site_data %>%
			mutate(POLYID = as.character(POLYID)) %>%
			select(site = POLYID, year = year_name,  actual = soc_tha_30cm)
		
		model_actual <- modelled %>%
			full_join(actuals, by=c("site", "year")) %>%
			filter(!is.na(actual))
		
		loglike <- loglik(model_actual$soc_total, model_actual$actual)
		
		output <- tibble(id, loglik = loglike)
		
		return(output)
	}
	
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
									 						"loglik")) %dopar%
			
			run_ipcct2_calculate_loglik(site_data[[site_n]], i)
		stopCluster(cl)
		Lkhood_list[[site_n]] <- Lkhood
	}
	
	
	# # code for non-paralleled run
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
	
	#==============================
	# Calculate First-order and Total global sensitivity indices
	#==============================
	singleSI <-si_obj2_llkhd$S # individual sensitivity indices
	singleSI$parmas <- row.names(singleSI)
	names(singleSI) <- c("singsi", "bias", "std.error", "singsi.lci", "singsi.uci", "params")
	singleSI <- singleSI[order(-singleSI$singsi), ]
	rownames(singleSI) <- 1:nrow(singleSI)
	singleSI <- singleSI[, c("params", "singsi", "singsi.lci", "singsi.uci")]
	SingPlot <- ggplot(singleSI, aes(x = reorder(params, -singsi), y = singsi, ymax=singsi.uci, ymin=singsi.lci)) + 
		xlab("Parameters") +
		ylab("First Order Sensitivity Index") +
		geom_errorbar(width=0.2, size=1, color="black") +
		geom_bar(stat='identity', fill="grey", alpha=0.70 ) +
		coord_flip() +
		theme_bw()
	SingPlot
	
	si_obj2_llkhd$S
	
	
	
	#==============================
	# Total-Order sensitivity indices
	#==============================
	totalSI <- si_obj2_llkhd$T # total sensitivity indices
	totalSI$parmas <- row.names(totalSI)
	names(totalSI) <- c("totsi", "bias", "std.error", "totsi.lci", "totsi.uci", "params")
	totalSI <- totalSI[order(-totalSI$totsi), ]
	rownames(totalSI) <- 1:nrow(totalSI)
	totalSI <- totalSI[, c("params", "totsi", "totsi.lci", "totsi.uci")]
	
	TotSIPlot <- ggplot(totalSI, aes(x = reorder(params, -totsi), y = totsi, ymax=totsi.uci, ymin=totsi.lci)) + 
		xlab("Parameters") + ylab("Total Sensitivity Index") + 
		geom_errorbar(width=0.2, size=1, color="black") +
		geom_bar(stat='identity', fill="grey", alpha=0.70) +
		coord_flip() + theme_bw()
	TotSIPlot
	
	combined_si <- totalSI %>%
		full_join(singleSI, by=c("params"))
	
	return(combined_si)
	
}
        