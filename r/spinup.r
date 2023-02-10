
source(here::here("r/spinup_wrappers.r"))

spinup <- function(site_data,
									 climate_data,
									 initial_c,
									 model,
									 ...)
{
	parameters <- list(...)
	model_function <- switch(model,
													 ipcct2 = ipcct2_run_spinup,
													 icbm = icbm_run_spinup)
	
	# Get mean of all numeric columns
	site_data_numeric <- site_data %>%
		ungroup() %>%
		summarise(across(where(is.numeric), mean, na.rm=TRUE))
	
	# Select first row of non-numeric columns
	site_data_nonnumeric <- site_data %>%
		select(where(negate(is.numeric))) %>%
		slice(1)
	
	for(col in colnames(site_data_nonnumeric)) {
		if(length(unique(site_data[[col]])) > 1) {
			warning(paste0("Two or more values found in column ",col,". Using the first value from year_name = ",site_data[["year_name"]][[1]]))
		}
	}
	
	# Combine into average values for this site, and set the year as the first year of the input data
	first_year <- first(site_data$year_name)
	site_data_average <- bind_cols(site_data_nonnumeric, site_data_numeric) %>%
		mutate(year_name = first_year)
	
	# Calculate steady state
	ss <- do.call(model_function,
					append(list(site_data = site_data_average,
											climate_data = climate_data),
								 parameters))
	ss_proportion <- ss/sum(ss)
	ss_initial <- ss_proportion*initial_c
	
	return(as.list(ss_initial))
}