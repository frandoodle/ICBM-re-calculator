library(ggplot2)
library(knitr)
library(dplyr)
library(geojson)
library(tibble)
library(purrr)
library(stringr)
library(here)

dir.create(tempdir()) # This fixes a bug if the temporary directory is not found

source(here::here("r/icbm_calculate_re_functions.r"))

# INPUTS:
# 
# YearInputTable, a table comprised of 365 rows with the following columns:
# 		Year(int)	-	simulation year
# 		JulianDay(int) - the julian day for each day of the year
# 		Tavg(float)	-	mean daily temperature (celsius)
# 		PREC	-	total daily precipitation (mm d-1)
# 		PET	-	total daily evapotranspiration (mm d-1)
# Yield(float)	-	simulation crop yield (kg dry matter ha-1)
# perennial(logical)	-	whether the simulated crop is perennial (TRUE/FALSE)
# SoilOrganicC_Percent(float)	-	soil OC level from the SLC database (%)
# ClayContent(float)	-	soil clay content (fraction)
# SandContent(float)	soil sand content (fraction)
# alfa(float)	-	Minimum water storage fraction of Wilting Point, constant value, typically 0.7
# SoilTopThickness(float), thickness of topsoil (mm), default = 250
# Temp_min(float), critical soil temperature (celsius), default -3.78
# Temp_max(float), maximum soil temperature (celsius), default = 30
# r_s(float), reference saturation, default = 0.42
# r_wp(float), reference wilting point, default = 0.18
# ReferenceAdjustment(float), Calibration factor for a bare-fallow treatment considering a soil thickness of 25 cm, (Uppsala, Sweden), default = 0.10516,
# r_c(float), tillage factor, can either be manually inputted or estimated, default = NA. When set to NA, r_c is estimated using built-in calculator
# tillage_soil(chr), soil type to use in considering tillage factor, default = "Brown"
# 		Brown
# 		Dark Brown
# 		Black
# tillage_type(chr), tillage type to use in considering tillage factor, default = "Intensive Tillage"
# 		Intensive Tillage
# 		Reduced Tillage
# 		No-till
# irrigation_region(chr), region to use when estimating daily irrigation, default = "Canada"
# 		Canada
# 		BC
# 		AB
# 		SK
# 		MB
# 		ON
# 		QC
# 		Atlantic Provinces
# irrigation_use_estimate(logical), whether the use the built-in irrigation estimation or not (TRUE/FALSE), default=FALSE
# irrigation(float), daily irrigation values (mm/d), default = 0
#
#  OUTPUTS:
#  
#  a numeric value representing the mean annual r_e value given that year's inputs

# table definitions ----------------------------------------------------

# Table 47 in the Holos algorithm document
Irrigation_percentage_monthly <- tibble(
	`Province/Region` = c("Canada", "BC", "AB", "SK", "MB", "ON", "QC", "Atlantic Provinces"),
	`Jan` = 0,
	`Feb` = 0,
	`Mar` = 0,
	`Apr` = c(4.69, 8.00, 3.06, 15.08, 0.22, 3.81, 10.54, 7.30),
	`May` = c(4.69, 8.00, 3.06, 15.08, 0.22, 3.81, 10.54, 7.30),
	`Jun` = c(15.64, 19.54, 13.92, 14.80, 11.30, 18.35, 19.54, 10.62),
	`Jul` = c(38.96, 27.77, 42.65, 29.58, 48.15, 41.46, 32.24, 27.97),
	`Aug` = c(26.92, 25.66, 28.04, 19.40, 34.88, 26.74, 25.97, 34.06),
	`Sep` = c(4.56, 5.51, 4.63, 3.03, 2.61, 2.91, 2.71, 5.63),
	`Oct` = c(4.56, 5.51, 4.63, 3.03, 2.61, 2.91, 2.71, 5.63),
	`Nov` = 0,
	`Dec` = 0
)	

# Table 36 in the Holos algorithm document
Tillage_table <- tibble(`Soil type` = c("Brown", "Dark Brown", "Black"),
												`Intensive Tillage` = c(1,1,1),
												`Reduced Tillage` = c(0.9, 0.85, 0.8),
												`No-till` = c(0.8, 0.7, 0.6))

# calculate_re ----------------------------------------------------------
calculate_re <- function(YearInputTable,
												 yield,
												 perennial,
												 SoilOrganicC_Percent,
												 ClayContent,
												 SandContent,
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
												 irrigation = 0,
												 ...# ellipsis is added to allow for passing unused arguments using do.call when doing parameter overrides
												 ) {
	# Check for NA values
	required_inputs <- list(YearInputTable = YearInputTable,
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
						irrigation = irrigation
						)
	if(any(is.na(required_inputs))) {
		warning(paste0("calculate_re() - NA values found for these inputs: ",paste(names(required_inputs)[is.na(required_inputs)], collapse=", ")))
	}
	if(any(is.na(YearInputTable))) {
		warning("calculate_re() - NA values found in YearInputTable")
	}
	
	# Eq. 2.2.1-1 through Eq. 2.2.1-3
	GAI_return <- calculateGAI(JulianDay = YearInputTable$JulianDay,
														 yield = yield,
														 perennial = perennial)
	GAI = GAI_return[["GAI"]]
	
	# Eq. 2.2.1-4 through Eq. 2.2.1-10
	WaterContentReturn <- calculateWaterContent(SoilOrganicC_Percent = SoilOrganicC_Percent,
																							ClayContent = ClayContent,
																							SandContent = SandContent)
	WiltingPoint <- WaterContentReturn[["WiltingPoint"]]
	FieldCapacity <- WaterContentReturn[["FieldCapacity"]]
	
	# Eq. 2.2.1-11
	SoilMeanDepth <- SoilTopThickness/20
	
	# Eq. 2.2.1-12
	LeafAreaIndex <- 0.8*GAI
	
	## Eq. 2.2.1-13 & Eq. 2.2.1-14
	SurfaceTemp <- ifelse(YearInputTable$Tavg < 0, 0.20*YearInputTable$Tavg,
												YearInputTable$Tavg*(0.95+0.05*exp(-0.4*(LeafAreaIndex-3))))
	
	# input_soiltemp <- YearInputTable %>%
	# 	full_join(GAI_return, by="JulianDay") %>%
	# 	full_join(WaterContentReturn, by="JulianDay") %>%
	# 	bind_cols(SoilTopThickness = SoilTopThickness,
	# 						SoilMeanDepth = SoilMeanDepth,
	# 						LeafAreaIndex = LeafAreaIndex,
	# 						SurfaceTemp = SurfaceTemp)
	
	# Eq. 2.2.1-15 & Eq. 2.2.1-16
	SoilTemp <- calculateSoilTemp(SurfaceTemp = SurfaceTemp,
																GAI = GAI,
																SoilMeanDepth = SoilMeanDepth)
	
	

	# Eq. 2.2.1-17 & Eq. 2.2.1-18
	Irrigation = irrigation
	
	P <- sum(YearInputTable$PREC)
	PE <- sum(YearInputTable$PET)
		
	
	if (is.na(irrigation_use_estimate) | !is.logical(irrigation_use_estimate)) {
		stop(paste0("Specify whether daily irrigation values should be estimated.
		If irrigation_use_estimate = TRUE, then irrigation is estimated using Eqs. 2.2.1-17 and Eq. 2.2.1-18.
		If irrigation_use_estimate = FALSE, then irrigation is defaulted to 0, but can be overridden with a numeric vector of length 365 which represents daily irrigation."))
	}
	if (PE > P & irrigation_use_estimate == T) {
		# Make sure irrigation_region is a valid input
		if(!irrigation_region %in% Irrigation_percentage_monthly$`Province/Region`) {
			stop(paste0(irrigation_region, " is not a valid irrigation_region. Valid regions are: ",paste(Irrigation_percentage_monthly$`Province/Region`,collapse=", ")))
		}
		
		Days_month <- lubridate::days_in_month(as.Date(paste(YearInputTable$Year, YearInputTable$JulianDay, sep="-"), "%Y-%j"))
		
		# Fraction of yearly irrigation for each month
		Fraction_monthly <- Irrigation_percentage_monthly %>%
		filter(`Province/Region` == irrigation_region) %>%
		`[`(names(Days_month)) %>%
		unlist %>%
		`/`(100)
	
	
		Irrigation_annual <- PE - P
		
		# Daily irrigation
		Irrigation <- ((Irrigation_annual * Fraction_monthly)/Days_month) %>%
			unname
	}
	
	# Eq. 2.2.1-19 & Eq. 2.2.1-20
	K_c <- 1.3 - (1.3-0.8) * exp(-0.17*GAI_return[["GAI"]]) 
	ET_0 <- YearInputTable$PET
	ET_c <- ET_0 * K_c
	
	
	Precipitation <- YearInputTable$PREC
	
	# Eq. 2.2.1-21 - Eq. 2.2.1-22
	CropInterception <- ifelse(Precipitation + Irrigation < 0.2*GAI,
														 Precipitation + Irrigation,
														 0.2*GAI)
	
	# Eq. 2.2.1-23
	CropInterception <- ifelse(CropInterception > ET_c, ET_c, CropInterception)
	
	# Eq. 2.2.1-24
	SoilAvailWater <- Precipitation + Irrigation - CropInterception
	
	# Eq. 2.2.1-25 through Eq. 2.2.1-35
	# input_waterstorage <- bind_cols(input_soiltemp,
	# 																					 SoilAvailWater = SoilAvailWater,
	# 																					 ET_c = ET_c)
	
	WaterStorage <- calculateWaterStorage(SoilAvailWater = SoilAvailWater,
																				ET_c = ET_c,
																				alfa = alfa,
																				SoilTopThickness = SoilTopThickness,
																				WiltingPoint = WiltingPoint,
																				FieldCapacity = FieldCapacity)

	# Eq. 2.2.1-36 - Eq. 2.2.1-37
	SoilTemp_dprev <- lag(SoilTemp, default=0)
	
	
	re_temp <- ifelse(SoilTemp_dprev < Temp_min, 0, ((SoilTemp_dprev - Temp_min)^2)/((Temp_max-Temp_min)^2))	

	
	WaterStorage_dprev <- lag(WaterStorage, default = FieldCapacity[1]*SoilTopThickness)
	VolSoilWaterContent <- calculateVolSoilWaterContent(WaterStorage_dprev = WaterStorage_dprev,
																											SoilTopThickness = SoilTopThickness,
																											WiltingPoint = WiltingPoint)
	
	# Eq. 2.2.1-38
	VolSoilWaterContent_sat <- 1.2*FieldCapacity
	# Eq. 2.2.1-39
	VolSoilWaterContent_opt <- 0.9*FieldCapacity
	
	# Eq. 2.2.1-40 - Eq. 2.2.1-42
	re_water <- ifelse(
		# if...
		VolSoilWaterContent > VolSoilWaterContent_opt,
		# then...
		(1 - r_s)*((VolSoilWaterContent-VolSoilWaterContent_opt)/(VolSoilWaterContent_opt-VolSoilWaterContent_sat)) + 1,
		ifelse(
			# elseif...
			VolSoilWaterContent < WiltingPoint,
			# then...
			r_wp*((VolSoilWaterContent)/(WiltingPoint)),
			# else...
			(1-r_wp)*((VolSoilWaterContent-WiltingPoint)/(VolSoilWaterContent_opt-WiltingPoint)) + r_wp))
	
	# Eq. 2.2.1-43
	re_water <- pmin(pmax(0,re_water),1)

	# Eq. 2.2.1-44
	re_x1 <- re_temp*re_water
	
	# Eq. 2.2.1-45
	re_cropdaily <- re_x1/ReferenceAdjustment
	
	# Eq. 2.2.1-46
	re_crop <- mean(re_cropdaily)	
	
	# Tillage factor (r_c)
	if(!tillage_soil %in% Tillage_table$`Soil type`) {
		stop(paste0(tillage_soil, " is not a valid tillage_soil. Valid soil types are: ",paste(Tillage_table$`Soil type`,collapse=", ")))
	}
	if(!tillage_type %in% names(Tillage_table[-1])) {
		stop(paste0(tillage_type, " is not a valid tillage_type. Valid tillage types are: ",paste(names(Tillage_table[-1]),collapse=", ")))
	}
	
	if(any(is.na(r_c))) {
		warning(paste0("NA values detected in r_c. Estimating r_c based on Tillage_table using tillage_soil = ",tillage_soil,", and tillage_type = ",tillage_type,".\n"))
		r_c <- Tillage_table %>%
			filter(`Soil type` == tillage_soil) %>%
			`[[`(tillage_type)
	}
	
	# Eq. 2.2.1-47
	r_edaily <- re_cropdaily*r_c
	r_e <- mean(r_edaily)
	return(r_e)
}
