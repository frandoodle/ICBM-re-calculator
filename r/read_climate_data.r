# This is a function that returns a daily climate data table when given the
# climate data location and (unique) site name

library(here)
library(purrr)
library(readr)
library(dplyr)
source(here('r/icbm_holos_calculate_pet.r'))

read_climate_data <- function(climate_data_directory)
	# climate_data_directory is the full directory (generated using here()) containing all .csv files with
	# 		daily climate parameters from the NASA Power database
{
	files <- list.files(climate_data_directory, full.names = TRUE, pattern = "csv$")
	
	climate_data <- files %>%
		purrr::map(~readr::read_csv(., col_types = "ciiiiddddddddd")) %>%
		bind_rows() %>%
		mutate(PET = holos_calculate_pet(meanDailyTemperature=.$Tmean,
																		 solarRadiation=.$Rad,
																		 relativeHumidity=.$RH))%>%
		rename(Tavg = Tmean,
					 PREC = Precip,
					 JulianDay = Julian)
	
	return(climate_data)
}


