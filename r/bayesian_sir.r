#=================================================================================
# Sampling Importance Resampling algorithm with Latin hypercube sampling in R
#=================================================================================
library(lhs)
library(doParallel)
library(parallel)
library(foreach)
library(here)

library(ggplot2)

source(here::here("r/ipcct2_run.r"))
source(here::here("r/bayesian_loglike.r"))

sir <- function(site_data,
								climate_data,
								parameter_bounds,
								sample_size = 100,
								resample_size = 10) {
	#=================================================================================
	# read prior distribution
	# (Required columns: Parameter, value, lower, upper)
	paramBounds <- parameter_bounds
	#=================================================================================
	# names of parameters that are allowed to vary
	varSI       <- paramBounds$Parameter
	nParams     <- length(varSI)
	#=================================================================================
	# LHS sampling for SIR
	#=================================================================================
	# sample size (1000000 samples were used in Gurung et al., 2020)
	n <- sample_size
	X1 <- randomLHS(n = n, k = nParams)
	# transform standard uniform LHS to prior distribution 
	Y1 <- matrix(0, nrow = nrow(X1), ncol = nParams)
	for(i in 1:nParams){
		pos <- which(paramBounds$Parameter == varSI[i])
		lower <- paramBounds[pos, "lower"]
		upper <- paramBounds[pos, "upper"]
		Y1[, i] <- qunif(X1[, i], min = lower, max = upper)
	}
	X <- as.data.frame(Y1)
	names(X) <- varSI
	X <- cbind("SampleID" = 1:nrow(X), X)
	
	#=================================================================================
	# Run the model and calculate log-likelihood
	#    - likelihoods were calculated assuming that the error (modeled - mseasured) are iid 
	#=================================================================================
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
			
			run_ipcct2_calculate_loglik(site_data[[site_n]], climate_data, i)
		stopCluster(cl)
		Lkhood_list[[site_n]] <- Lkhood
	}
	
	Lkhood1 <- Lkhood_list %>%
		bind_rows %>%
		mutate(loglik = ifelse(loglik == -Inf, NA, loglik)) %>%
		group_by(id) %>%
		summarise(loglik = mean(loglik, na.rm=T)) %>%
		mutate(weights = exp(loglik)/sum(exp(loglik)))
	
	#=================================================================================
	# sample without replacement (a resampling of 1000 were used in Gurung et al., 2020)
	nsamp <- resample_size 
	sampIndx <- sample(1:nrow(Lkhood1), 
										 size = nsamp, replace = FALSE,
										 prob = Lkhood1$weights)
	PostTheta <- as.data.frame(X[sampIndx,])
	
	return(list(prior = X, posterior = PostTheta))
}

#================================================================================================================
# Graphing
# prior <- X %>%
# 	pivot_longer(cols = !`SampleID`)
# posterior <- PostTheta %>%
# 	pivot_longer(cols = !`SampleID`)
# 
# p2<-ggplot() +
# 	geom_density(data = posterior, aes(value), col = NA, fill = "red", alpha = 0.2) +
# 	geom_density(data = prior, aes(value), col = NA, fill = "blue", alpha = 0.2) +
# 	facet_wrap(~name, scales = "free", ncol = 3) +
# 	theme_bw() +
# 	theme(panel.border = element_rect(colour = "black", fill = NA), panel.grid.major = element_blank(),
# 				axis.title.x=element_blank(),panel.grid.minor = element_blank()) +
# 	scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
# p2