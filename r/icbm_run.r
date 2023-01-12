# This is a function which runs ICBM on one site/treatment using
# yearly inputs (a site data table and a daily climate table)

dir.create(tempdir()) # This fixes a bug if the temporary directory is not found
source(here::here("r/icbm_calculate_re.r"))
source(here::here("r/ICBM_Sarah_TimeSeries_Tested_Manure_V1.r"))

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

run_icbm <- function(DailyClimateTable,
												 SiteDataTable,
												 ag_init = 0,
												 bg_init = 0,
												 o_init = 0,
												 ...)
{
	
	polyid <- unique(SiteDataTable$POLYID)
	DailyClimateTable_polyid <- DailyClimateTable %>%
		filter(POLYID == polyid)
	simulation_years <- unique(SiteDataTable$year_name)
	
	# Errors ----------------------------
	if(any(duplicated(SiteDataTable$year_name))) {
		stop("Duplicate years detected in SiteDataTable.")
	}
	if(length(polyid) > 1) {
		stop("Multiple POLYIDs found in SiteDataTable")
	}
	if(any(!(simulation_years %in% unique(DailyClimateTable_polyid$Year)))) {
		stop("Years in DailyClimateTable_polyid do not overlap all years in SiteDataTable.")
	}
	if(nrow(DailyClimateTable_polyid) < 365) {
		stop("Less than 365 rows in DailyClimateTable_polyid")
	}
	
	# Parameter overrides ---------------------------------------
	params <- list(...)
	if(length(params) > 0) {
		for(paramname in names(params)) {
			if (paramname %in% names(SiteDataTable)) {
				SiteDataTable[paramname] <- params[paramname]
				params <- params[names(params) != paramname]
			}
			if (paramname %in% names(DailyClimateTable_polyid)) {
				DailyClimateTable_polyid[paramname] <- params[paramname]
				params <- params[names(params) != paramname]
			}
		}
	}
	# Step 1: Calculate re for all years ------------------------------------------------
	
	re <- DailyClimateTable_polyid %>%
		filter(Year %in% simulation_years) %>%
		group_by(Year) %>%
		group_split() %>%
		# Note: the way this map works is that it calculates re for every year
		# using the yearly daily climate table as input. The terms such as:
		# filter(SiteDataTable, year_name == .$Year[1])$total_yield
		# pull the relevant data from the site data table for use in the re calculation for that year only
		# Note: do.call is used in order to pass parameter overrides to the function
		map(~do.call(calculate_re, append(
			list(YearInputTable = .,
					 yield = filter(SiteDataTable, year_name == .$Year[1])$total_yield,
					 perennial = filter(SiteDataTable, year_name == .$Year[1])$perennial,
					 SoilOrganicC_Percent = filter(SiteDataTable, year_name == .$Year[1])$soil_total_carbon_px,
					 ClayContent = filter(SiteDataTable, year_name == .$Year[1])$clay_px,
					 SandContent = filter(SiteDataTable, year_name == .$Year[1])$sand_px,
					 r_c = filter(SiteDataTable, year_name == .$Year[1])$r_c,
					 tillage_soil = filter(SiteDataTable, year_name == .$Year[1])$tillage_soil,
					 tillage_type = filter(SiteDataTable, year_name == .$Year[1])$tillage_type,
					 irrigation_region = filter(SiteDataTable, year_name == .$Year[1])$irrigation_region,
					 irrigation = filter(SiteDataTable, year_name == .$Year[1])$irrigation),
			params))) %>%
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
	
	yopool <- c(ag = ag_init,
							bg = bg_init,
							o = o_init)
	
	iag <- SiteDataTable$crop_residue_kgha + SiteDataTable$hay_residue_kgha %>%
		replace_na(0)
	ibg <- SiteDataTable$roots_residue_kgha %>%
		replace_na(0)
	iman <- SiteDataTable$manure_kgha %>%
		replace_na(0)
	
	result <- do.call(icbm_holos4_classic_manure, append(
		list(times = simulation_years,
				 iag = iag,
				 ibg = ibg,
				 iman = iman,
				 re = re,
				 yopool = yopool),
		params)) %>%
		mutate(polyid = polyid, .before = time)
	
	return(result)
}