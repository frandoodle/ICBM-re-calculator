library(ggplot2)
library(knitr)
library(dplyr)
library(geojson)
library(tibble)
library(purrr)
library(stringr)

source("re_functions.r")

# daily_input needs:
# 	JulianDay (optional)
# 	Yield = 3000
# 	perennial = FALSE
# 	
# 	Tavg
# 	PREC
# 	PET
# 	
# 	SoilOrganicC_Percent = 12
# 	ClayContent = 0.05
# 	SandContent = 0.20
	
SoilTemp_all <- NULL
for(i in 1:nrow(daily_input_test)) {
	GAI_return <- calculateGAI(JulianDay = i,
														 Yield = Yield,
														 perennial = perennial)
	GAI <- GAI_return["GAI"]
	
	WaterContent_return <- calculateWaterContent(SoilOrganicC_Percent = SoilOrganicC_Percent,
																							 ClayContent = ClayContent,
																							 SandContent = SandContent)
	WaterContent_return
	
	SoilTopThickness = 250 #millimetres
	SoilMeanDepth <- SoilTopThickness/20
	
	LeafAreaIndex <- 0.8*GAI 
	
	Temperature <- filter(daily_input_test, `JulianDay` == i)$Tavg
	
	if(Temperature < 0) {
		SurfaceTemp <- 0.20*Temperature
	} else {
		SurfaceTemp <- Temperature*(0.95+0.05*exp(-0.4*(LeafAreaIndex-3)))
	}
	
	if(i == 1) {
		SoilTemp_d <- 0
	} else {
		SoilTemp_dprev <- SoilTemp_all[[i-1]]
		SoilTemp_d <- SoilTemp_dprev + (SurfaceTemp-SoilTemp_dprev)*0.24*exp(-SoilMeanDepth*0.0174)*exp(-0.15*GAI)
	}
	SoilTemp_all <- c(SoilTemp_all, SoilTemp_d)
	
	ggplot(SoilTemp_all, aes(x = 1:365, y = SoilTemp_all))
	
}

