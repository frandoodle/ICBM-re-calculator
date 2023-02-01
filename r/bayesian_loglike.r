loglik=function(m,o){
  if(length(m)!=length(o)){
     print("Inequal number of modeled and observed values, cannot proceed")
     return()
   }
  
  res=log(m)-log(o)
  sigma=sqrt(mean(res^2))
  n=length(m)
  lk=-n*log(sigma)-(1/(2*sigma^2))*sum(res^2)
  return(lk)
  
}

run_ipcct2_calculate_loglik <- function(site_data,
																				climate_data,
																				init_active,
																				init_slow,
																				init_passive,
																				parameters) 
{
	id <- parameters[[1]] #gets id, hopefully (I think foreach::foreach coerces rows into unnamed vectors)
	modelled <- do.call("run_ipcct2",
											append(list(site_data = site_data,
																	climate_data = climate_data,
																	init_active = init_active,
																	init_slow = init_slow,
																	init_passive = init_passive),
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