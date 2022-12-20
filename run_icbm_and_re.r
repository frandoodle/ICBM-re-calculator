# This is a function which runs ICBM on one site/treatment using
# yearly inputs (a site data table and a daily climate table)

dir.create(tempdir()) # This fixes a bug if the temporary directory is not found

here::i_am("run_icbm_and_re.r")
source(here::here("re_functions.r"))
source(here::here("re_script.r"))
source(here::here("ICBM_Sarah_TimeSeries_Tested_Manure_V1.r"))

# Overall Inputs ------------------------------------------------------------
# SiteDataTable - a table with yearly site data
# 								Required columns:
# 											- Yield - the annual crop yield for each year
# 											- iag - annual carbon input to aboveground young C pool for each year
# 											- ibg - annual carbon input to belowground young C pool for each year
# 											- iman - annual carbon input to manure young C pool for each year.
# 											- Perennial - (TRUE/FALSE) whether the crop is perennial
# 											- SoilOrganicC_Percent - (0-100) soil OC%
# 											- ClayContent - (0-1) soil clay content
# 											- SandContent - (0-1) soil sand content
# 											- r_c = NA,
# 											- tillage_soil = "Brown",
# 											- tillage_type = "Intensive Tillage",
# 											- irrigation_region = "Canada",
# 											- irrigation_use_estimate = FALSE,
# 											- irrigation = 0
# DailyClimateTable - a table with daily climate data for every year you
# 										intend to simulate
# 										Required columns:
# 											- Year
# 											- MeanDailyAirTemperature (celsius)
# 											- MeanDailyPrecipitation (mm)
# 											- MeanDailyPET (mm)
# ag_init - initial aboveground young C pool
# bg_init - initial belowground young C pool
# o_init - initial old C pool
#
# Constants:
# alfa = 0.7,
# SoilTopThickness = 250,
# Temp_min = -3.78,
# Temp_max = 30,
# r_s = 0.42,
# r_wp = 0.18,
# ReferenceAdjustment = 0.10516


RunICBMAndRe <- function(DailyClimateTable,
												 SiteDataTable,
												 ag_init = 0,
												 bg_init = 0,
												 o_init = 0,
												 alfa = 0.7,
												 SoilTopThickness = 250,
												 Temp_min = -3.78,
												 Temp_max = 30,
												 r_s = 0.42,
												 r_wp = 0.18,
												 ReferenceAdjustment = 0.10516
												 )
{
	simulation_years <- unique(SiteDataTable$year_name)
	
	if(any(!(simulation_years %in% unique(DailyClimateTable$Year)))) {
		stop("Years in DailyClimateTable do not overlap all years in SiteDataTable.")
	}
	
	# Step 1: Calculate re for all years ------------------------------------------------
	
	re <- DailyClimateTable %>%
		filter(Year %in% simulation_years) %>%
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
	
	# Step 2: Run ICBM ------------------------------------------------------------------
	
	# ICBM Inputs ---
	# times - the number of years to run
	# iag - annual carbon input to aboveground young C pool.
	# 			Should be a vector of the same length as "times"
	# ibg - annual carbon input to belowground young C pool.
	# 			Should be a vector of the same length as "times"
	# iman - annual carbon input to manure young C pool.
	# 			 Should be a vector of the same length as "times"
	# re - a soil climate-and management parameter that aggregates
	#			 the external influences on soil biological activity
	#			 Should be a single, constant numerical value.
	#			 OR MAYBE: Should be a vector of the same length as "times"
	# yopool - A vector of length 3 representing the 
	#					 Initial C pools. 
	#						yopool = c(initial young aboveground C,
	#											 initial young belowground C,
	#											 initial old C)
	yopool = c(ag = ag_init,
						 bg = bg_init,
						 o = o_init)
	
	iag = SiteDataTable$crop_residue_kgha + SiteDataTable$hay_residue_kgha...23 %>%
		replace_na(0)
	ibg = SiteDataTable$roots_residue_kgha %>%
		replace_na(0)
	iman = SiteDataTable$manure_kgha %>%
		replace_na(0)
	
	result <- icbm_holos4_classic_manure(times = simulation_years,
														 iag = iag,
														 ibg = ibg,
														 iman = iman,
														 re = re,
														 yopool = yopool)
	
	return(result)
}