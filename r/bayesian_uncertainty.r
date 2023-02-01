
montecarlo <- function(site_data,
							 climate_data,
							 initial_c,
							 distribution,
							 sample_size = 10) {
	# site_data can either be a data.frame, or a list of data.frames
	if(!inherits(site_data, "list")) {
		site_data <- list(site_data)
	}
	if(!any(sapply(initial_c, is.list))) {
		initial_c <- list(initial_c)
	}
	if(length(site_data) != length(initial_c)) {
		stop("site_data and initial_c cannot be different lengths")
	}
	# Sample from distribution
	parameters_sample <- slice_sample(distribution, n = sample_size)
	
	# Median from distribution
	parameters_median <- distribution %>%
		summarise_all(median)
	
	# Run model on samples (Monte Carlo)
	model_results_list <- list()
	median_results_list <- list()
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
													init_active = initial_c[[site_n]]$init_active,
													init_slow = initial_c[[site_n]]$init_slow,
													init_passive = initial_c[[site_n]]$init_passive,
													return_site_inputs = TRUE),
										 parameters))
		stopCluster(cl)
		model_results_list[[site_n]] <- model_results
		# Run model on median
		median_results <- do.call("run_ipcct2",
						append(list(site_data[[site_n]],
												climate_data,
												init_active = initial_c[[site_n]]$init_active,
												init_slow = initial_c[[site_n]]$init_slow,
												init_passive = initial_c[[site_n]]$init_passive,
												return_site_inputs = TRUE),
									 parameters_median))
		
		median_results_list[[site_n]] <- median_results
	}
	
	
	
	
	# Return results
	
	model_results_list_bind <- model_results_list %>%
		bind_rows
	median_results_list_bind <- median_results_list %>%
		bind_rows
	return(list(montecarlo = model_results_list_bind,
							median = median_results_list_bind))
}