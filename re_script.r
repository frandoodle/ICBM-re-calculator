library(ggplot2)
library(knitr)
library(dplyr)
library(geojson)
library(tibble)
library(purrr)
library(stringr)

source("re_functions.r")

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

holos_re <- readr::read_csv(here("holos_re_calculation.csv"))

input <- tibble(
	Year = 2020,
	JulianDay = 1:365,
	Region = "QC",
	Yield = rep(3000, 365),
	perennial = rep(FALSE, 365),
	SoilOrganicC_Percent = rep(2, 365),
	ClayContent = rep(0.05, 365),
	SandContent = rep(0.2, 365),
	alfa = rep(0.7, 365)
) %>%
	mutate(Tavg = holos_re$InputTemperature,
				 PREC = holos_re$InputPrecipitation,
				 PET = holos_re$InputEvapotranspiration)


	GAI_return <- calculateGAI(input)
	
	WaterContent_return <- calculateWaterContent(input)
	WiltingPoint <- WaterContentReturn[["WiltingPoint"]]
	FieldCapacity <- WaterContentReturn[["FieldCapacity"]]
	
	SoilTopThickness = 250
	SoilMeanDepth <- SoilTopThickness/20
	
	LeafAreaIndex <- 0.8*GAI 
	
	SurfaceTemp <- ifelse(input$Tavg < 0, 0.20*input$Tavg,
												input$Tavg*(0.95+0.05*exp(-0.4*(LeafAreaIndex-3))))
	
	input_soiltemp <- input %>%
		full_join(GAI_return, by="JulianDay") %>%
		full_join(WaterContent_return, by="JulianDay") %>%
		bind_cols(SoilTopThickness = SoilTopThickness,
							SoilMeanDepth = SoilMeanDepth,
							LeafAreaIndex = LeafAreaIndex,
							SurfaceTemp = SurfaceTemp)
	
	SoilTemp <- calculateSoilTemp(input_soiltemp)
	
	K_c <- 1.3 - (1.3-0.8) * exp(-0.17*GAI_return[["GAI"]]) 
	ET_0 <- input$PET
	ET_c <- ET_0 * K_c

	P <- sum(daily_input_test$PREC)
	PE <- sum(daily_input_test$PET)
	
	Days_month <- lubridate::days_in_month(as.Date(paste(input$Year, input$JulianDay, sep="-"), "%Y-%j"))
	
	Fraction_monthly <- Irrigation_percentage_monthly %>%
		filter(`Province/Region` == input$Region[1]) %>%
		`[`(names(Days_month)) %>%
		unlist %>%
		`/`(100)
	
	if (PE > P) {
		warning("Using Eqs. 2.2.1-45 and 2.2.1-46 to calculate default irrigation values")
		Irrigation_annual <- PE - P
		
		Irrigation <- ((Irrigation_annual * Fraction_monthly)/Days_month) %>%
			unname
	} else {
		Irrigation <- 0
	}
	
	Precipitation <- input$PREC
	
	#Eq. 2.2.1-19 - Eq. 2.2.1-20
	CropInterception <- ifelse(Precipitation + Irrigation < 0.2*GAI,
														 Precipitation + Irrigation,
														 0.2*GAI)
	
	#Eq. 2.2.1-21
	CropInterception <- ifelse(CropInterception > ET_c, ET_c, CropInterception)
	
	#Eq. 2.2.1-22
	SoilAvailWater <- Precipitation + Irrigation - CropInterception
	
	daily_input_test_waterstorage <- bind_cols(input_soiltemp,
																						 SoilAvailWater = SoilAvailWater,
																						 ET_c = ET_c)
	
	WaterStorage <- calculateWaterStorage(daily_input_test_waterstorage)

	SoilTemp_dprev <- lag(SoilTemp, default=0)
	Temp_min <- -3.78
	Temp_max <- 30
	
	# Eq. 2.2.1-34 - Eq. 2.2.1-35
	re_temp <- ifelse(SoilTemp_dprev < -3.78, 0, ((SoilTemp_dprev - Temp_min)^2)/((Temp_max-Temp_min)^2))	
