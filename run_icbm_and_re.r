
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

RunICBMAndRe <- function(DailyClimateTable,
												 perennial,
												 SoilOrganicC_Percent,
												 ClayContent,
												 SandContent,
												 yield,
												 iag,
												 ibg,
												 iman,
												 iag_init = 0,
												 ibg_init = 0,
												 io_init = 0,
												 alfa = 0.7,
												 SoilTopThickness = 250,
												 Temp_min = -3.78,
												 Temp_max = 30,
												 r_s = 0.42,
												 r_wp = 0.18,
												 ReferenceAdjustment = 0.10516,
												 r_c = NA,
												 tillage_soil = "Brown",
												 tillage_type = "Intensive Tillage",
												 irrigation_region = "Canada",
												 irrigation_use_estimate = FALSE,
												 irrigation = 0)
{
	simulation_years <- unique(DailyClimateTable$Year)
	
	# Calculate re for all years ------------------------------------------------
	
	#	re Inputs ---
	#	YearInputTable - a table with 365 rows representing various model
	#									 inputs for the year
	#	irrigation_use_estimate - logical (TRUE/FALSE) indicating whether you want
	#														to use the built-in irrigation equation (TRUE), or
	#														input your own irrigation values (FALSE)
	
	re <- DailyClimateTable %>%
		group_by(Year) %>%
		group_split() %>%
		map(~calculate_re(.,
											yield = yield,
											perennial = perennial,
											SoilOrganicC_Percent = SoilOrganicC_Percent,
											ClayContent = ClayContent,
											SandContent = SandContent,
											alfa = alfa,
											SoilTopThickness = SoilTopThickness,
											Temp_min = Temp_min,
											Temp_max = Temp_max,
											r_s = r_s,
											r_wp = r_wp,
											ReferenceAdjustment = ReferenceAdjustment,
											r_c = r_c,
											tillage_soil = tillage_soil,
											tillage_type = tillage_type,
											irrigation_region = irrigation_region,
											irrigation_use_estimate = irrigation_use_estimate,
											irrigation = irrigation)) %>% 
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