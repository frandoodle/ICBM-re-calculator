
mc <- function(site_data,
							 climate_data,
							 distribution,
							 sample_size = 10) {
	# site_data can either be a data.frame, or a list of data.frames
	if(!inherits(site_data, "list")) {
		site_data <- list(site_data)
	}
	# Sample from distribution
	parameters_sample <- slice_sample(distribution, n = sample_size)
	
	# Run model on samples (Monte Carlo)
	model_results_list <- list()
	for(site_n in 1:length(site_data)) {
		ncores=parallel::detectCores()-2
		cl=parallel::makeCluster(ncores)
		doParallel::registerDoParallel(cl)
		
		model_results=foreach(parameters=1:nrow(parameters_sample),
													.packages = c("parallel", 
																				"doParallel", 
																				"tidyverse"),
													.export = c("run_ipcct2",
																			"IPCCTier2SOMmodel")) %dopar%
			
			do.call("run_ipcct2",
							append(list(site_data[[site_n]],
													climate_data,
													init_active = 0,
													init_slow = 0,
													init_passive = 0,
													include_site_inputs = TRUE),
										 parameters))
		stopCluster(cl)
		model_results_list[[site_n]] <- model_results
	}
	
	# Return results
	results <- model_results_list %>%
		bind_rows
	return(results)
}