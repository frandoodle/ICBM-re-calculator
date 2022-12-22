# This is a function that returns a daily climate data table when given the
# climate data location and (unique) site name

library(here)
library(purrr)
library(readr)
library(dplyr)
source(here('calculate_pet_HOLOS.r'))

getClimateData <- function(climate_data_directory,
													 polyid)
	# climate_data_directory is the full directory (generated using here()) containing all .csv files with
	# 		daily climate parameters from the NASA Power database
	# polyid is the name of the experimental site, which is used
	# 		to filter the NASA climate data files
	{
	files <- list.files(climate_data_directory, full.names = TRUE, pattern = "csv$")
	
	climate_data <- files %>%
		purrr::map(~readr::read_csv(., col_types = "ciiiiddddddddd")) %>%
		purrr::map(~filter(., POLYID==polyid)) %>%
		bind_rows() %>%
		mutate(PET = calculatePETHolos(meanDailyTemperature=.$Tmean,
																			solarRadiation=.$Rad,
																			relativeHumidity=.$RH))%>%
		rename(Tavg = Tmean,
					 PREC = Precip,
					 JulianDay = Julian)
	
	return(climate_data)
}


