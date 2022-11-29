
dir.create(tempdir()) # This fixes a bug if the temporary directory is not found

here::i_am("run_icbm_and_re.r")
source(here::here("re_functions.r"))
source(here::here("re_script.r"))
source(here::here("ICBM_Sarah_TimeSeries_Tested_Manure_V1.r"))

# Overall Inputs ------------------------------------------------------------

# Yield - the annual crop yield for each year
# iag - annual carbon input to aboveground young C pool for each year
# ibg - annual carbon input to belowground young C pool for each year
# iman - annual carbon input to manure young C pool for each year.
# iag_init - initial aboveground young C pool
# ibg_init - initial belowground young C pool
# io_init - initial old C pool
# DailyClimateTable - a table with daily climate data for every year you
# 										intend to simulate
# 										Required columns:
# 											- Year
# 											- MeanDailyAirTemperature (celsius)
# 											- MeanDailyPrecipitation (mm)
# 											- MeanDailyPET (mm)
# Perennial - (TRUE/FALSE) whether the crop is perennial
# SoilOrganicC_Percent - (0-100) soil OC%
# ClayContent - (0-1) soil clay content
# SandContent - (0-1) soil sand content

RunICBMAndRe <- function(yield,
												 iag,
												 ibg,
												 iman,
												 iag_init = 0,
												 ibg_init = 0,
												 io_init = 0,
												 DailyClimateTable,
												 Perennial,
												 SoilOrganicC_Percent,
												 ClayContent,
												 SandContent,
												 irrigation_use_estimate)
{
	simulation_years <- unique(DailyClimateTable$Year)
	
	# Calculate re for all years ------------------------------------------------
	
	#	re Inputs ---
	#	YearInputTable - a table with 365 rows representing various model
	#									 inputs for the year
	#	irrigation_use_estimate - logical (TRUE/FALSE) indicating whether you want
	#														to use the built-in irrigation equation (TRUE), or
	#														input your own irrigation values (FALSE)
	
	re_input <- DailyClimateTable %>%
		rename(Tavg = MeanDailyAirTemperature,
					 PREC = MeanDailyPrecipitation,
					 PET = MeanDailyPET)
	
	re <- re_input %>%
		group_by(Year) %>%
		group_split() %>%
		map(~calculate_re(.,
											yield = yield,
											perennial = Perennial,
											irrigation_use_estimate=irrigation_use_estimate)) %>% 
		unlist()
	
	# Run ICBM ------------------------------------------------------------------
	
	# ICBM Inputs ---
	# times - the number of years to run
	# iag - annual carbon input to aboveground young C pool (?).
	# 			Should be a vector of the same length as "times"
	# ibg - annual carbon input to belowground young C pool (?).
	# 			Should be a vector of the same length as "times"
	# iman - annual carbon input to manure young C pool (?).
	# 			 Should be a vector of the same length as "times"
	# re - a soil climate-and management parameter that aggregates
	#			 the external influences on soil biological activity
	#			 Should be a single, constant numerical value (?).
	#			 OR MAYBE: Should be a vector of the same length as "times"
	# yopool - A vector of length 3 representing the 
	#					 Initial C pools. 
	#						yopool = c(initial young aboveground C,
	#											 initial young belowground C,
	#											 initial old C)
	yopool = c(iag = iag_init,
						 ibg = ibg_init,
						 io = io_init)
	result <- icbm_holos4_classic_manure(times = simulation_years,
														 iag = iag,
														 ibg = ibg,
														 iman = iman,
														 re = re,
														 yopool = yopool)
	
	return(result)
}



# Test ----------------------------------------------------------------------
# Import weather data
holos_nasa_climate <- readr::read_csv(here::here("holos_nasa_climate.csv")) %>%
	filter(Year >= 2000)

iag <- rep(977,length(unique(holos_nasa_climate$Year)))
ibg <- rep(288,length(unique(holos_nasa_climate$Year)))
iman <- rep(88,length(unique(holos_nasa_climate$Year)))

yield <- rep(3000,nrow(holos_nasa_climate))

RunICBMAndRe(yield = yield,
						 iag = iag,
						 ibg = ibg,
						 iman = iman,
						 iag_init = 0,
						 ibg_init = 0,
						 io_init = 0,
						 DailyClimateTable = holos_nasa_climate,
						 Perennial = FALSE,
						 SoilOrganicC_Percent = 5,
						 ClayContent = 0.05,
						 SandContent = 0.2,
						 irrigation_use_estimate = FALSE)
