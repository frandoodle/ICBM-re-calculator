library(here)
library(purrr)
library(readr)
library(dplyr)

getClimateData <- function(climate_data_folder,
													 master_data_folder,
													 site_name)
	# climate_data_folder is the name of the folder containing all climate data folders
	# master_data_folder is the name of the folder containing .csv files with
	# 		daily climate parameters from the NASA Power database
	# site_name is the name of the experimental site, which is used
	# 		to filter the NASA climate data files
	{
	files <- list.files(here::here(climate_data_folder,master_data_folder), full.names = TRUE, pattern = "csv$")
	
	climate_data <- files %>%
		purrr::map(~readr::read_csv(., col_types = "ciiiiddddddddd")) %>%
		purrr::map(~filter(., POLYID==site_name)) %>%
		bind_rows() %>%
		mutate(PET = calculatePETHolos(meanDailyTemperature=.$Tmean,
																			solarRadiation=.$Rad,
																			relativeHumidity=.$RH))%>%
		rename(Tavg = Tmean,
					 PREC = Precip,
					 JulianDay = Julian)
	
	return(climate_data)
}


calculatePETHolos <- function(meanDailyTemperature,
																solarRadiation,
																relativeHumidity){
	# This is a vectorized implementation of the Holos reference PET calculator.
	# https://github.com/holos-aafc/Holos/blob/main/H.Core/Calculators/Climate/EvapotranspirationCalculator.cs
	
	term1 = 0.013
	term2 = meanDailyTemperature / (meanDailyTemperature + 15)
	term3 = (23.8856 * solarRadiation) + 50
	term4 = 1 + ((50 - relativeHumidity) / 70)
	
	result <- ifelse(relativeHumidity >= 50,
				 term1 * term2 * term3,
				 term1 * term2 * term3 * term4)
	result <- ifelse(result < 0, 0, result)
	result <- ifelse(meanDailyTemperature <= 0, 0, result)
	
	return(result)
}
