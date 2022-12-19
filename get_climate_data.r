library(here)
library(purrr)
library(readr)
library(dplyr)
source(here('calculate_pet_HOLOS.r'))

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


