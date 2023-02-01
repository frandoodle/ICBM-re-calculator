source(here::here("r/ipcct2_run.r"))

ipcct2_run_spinup <- function(site_data,
															climate_data,
															...) {
	parameters <- list(...)
	model_results <- do.call(run_ipcct2,
													 append(list(site_data = site_data,
													 						climate_data = climate_data),
													 			 parameters))
	
	ss <- c(init_active = model_results$SS_active,
					init_slow = model_results$SS_slow,
					init_passive = model_results$SS_passive)
	
	return(ss)
}