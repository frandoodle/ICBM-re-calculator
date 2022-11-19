
calculateGAI <- function(InputTable) {
	#Input should be a df/tibble with at least the columns JulianDay (int),
	#Yield (float), and perennial (bool)
	
	#Holos V4 constant values for different crop types
	#(see section 2.2.1.1.1.1, Green area index dynamics)
	EmergenceDay <- ifelse(InputTable$perennial == TRUE, 75, 141)
	RipeningDay <- ifelse(InputTable$perennial == TRUE, 300, 197)
	Variance <- ifelse(InputTable$perennial == TRUE, 1500, 300)
	
	#Eq. 2.2.1-1
	GAI_max <- 0.0731*(InputTable$Yield/1000)^2 + 0.408*(InputTable$Yield/1000)
	
	#Eq. 2.2.1-2
	MidSeason <- EmergenceDay + ((RipeningDay-EmergenceDay)/2)
	
	#Eq. 2.2.1-3
	GAI <- GAI_max*exp(-(((InputTable$JulianDay-MidSeason)^2)/(2*Variance)))
	
	return(tibble(JulianDay = InputTable$JulianDay, GAI_max = GAI_max, MidSeason = MidSeason, GAI = GAI))
}

calculateWaterContent <- function(InputTable) {
	#Input should be a df/tibble with at least the columns
	#SoilOrganicC_Percent (float), ClayContent (float), and SandContent (float)
	
	#Eq. 2.2.1-4
	OrgCfactor <- -0.837531 + 0.430183*InputTable$SoilOrganicC_Percent
	#Eq. 2.2.1-5
	Clayfactor <- -1.40744 + 0.0661969*InputTable$ClayContent*100
	#Eq. 2.2.1-6
	Sandfactor <- -1.51866 + 0.0393284*InputTable$SandContent*100
	
	#Eq. 2.2.1-7
	WiltingPointPercent <- 14.2568+7.36318*(
		0.06865
		+(0.108713*OrgCfactor)
		-(0.0157225*OrgCfactor^2)
		+(0.00102805*OrgCfactor^3)
		+(0.886569*Clayfactor)
		-(0.223581*OrgCfactor*Clayfactor)
		+(0.0126379*(OrgCfactor^2)*Clayfactor)
		-(0.017059*(Clayfactor^2))
		+(0.0135266*(OrgCfactor*(Clayfactor^2)))
		-(0.0334434*(Clayfactor^3))
		-(0.0535182*(Sandfactor))
		-(0.0354271*(OrgCfactor*Sandfactor))
		-(0.00261313*(OrgCfactor^2)*Sandfactor)
		-(0.154563*(Clayfactor)*(Sandfactor))
		-(0.0160219*(OrgCfactor)*(Clayfactor)*(Sandfactor))
		-(0.0400606*(Clayfactor^2)*(Sandfactor))
		-(0.104875*(Sandfactor^2))
		+(0.0159857*(OrgCfactor)*(Sandfactor^2))
		-(0.0671656*(Clayfactor)*(Sandfactor^2))
		-(0.0260699*(Sandfactor^3))
	)
	
	#Eq. 2.2.1-8
	WiltingPoint <- WiltingPointPercent/100
	
	#Eq. 2.2.1-9
	FieldCapacityPercent <- 29.7528+10.3544*(
		0.0461615
		+0.290955*(OrgCfactor)
		-0.0496845*(OrgCfactor^2)
		+0.00704802*(OrgCfactor^3)
		+0.269101*(Clayfactor)
		-0.176528*(OrgCfactor)*(Clayfactor)
		+0.0543138*(OrgCfactor^2)*(Clayfactor)
		+0.1982*(Clayfactor^2)
		-0.060699*(Clayfactor^3)
		-0.320249*(Sandfactor)
		-0.0111693*(OrgCfactor^2)*(Sandfactor)
		+0.14104*(Clayfactor)*(Sandfactor)
		+0.0657345*(OrgCfactor)*(Clayfactor)*(Sandfactor)
		-0.102026*(Clayfactor^2)*(Sandfactor)
		-0.04012*(Sandfactor^2)
		+0.160838*(OrgCfactor)*(Sandfactor^2)
		-0.121392*(Clayfactor)*(Sandfactor^2)
		-0.061667*(Sandfactor^3)
	)
	
	#Eq. 2.2.1-10
	FieldCapacity <- FieldCapacityPercent/100
	
	return(tibble(JulianDay = InputTable$JulianDay,
								OrgCfactor = OrgCfactor,
				 Clayfactor = Clayfactor,
				 Sandfactor = Sandfactor,
				 WiltingPoint = WiltingPoint,
				 FieldCapacity = FieldCapacity))
}

calculateSurfaceTemp <- function(InputTable){
	#Input should be a df/tibble with the columns JulianDay (int),
	#Tavg (float), and everything that's required for calculateGAI().
	LeafAreaIndex <- 0.8*calculateGAI(InputTable)[["GAI"]]
	SurfaceTemp <- ifelse(InputTable$Tavg < 0, 0.20*InputTable$Tavg, InputTable$Tavg*(0.95+0.05*exp(-0.4*(LeafAreaIndex-3))))
	return(SurfaceTemp)
}

calculateSoilTemp <- function(InputTable) {
	#Input should be a df/tibble with the columns
	#SoilMeanDepth (float)
	#SurfaceTemp (float)
	#GAI (float)
	result <- InputTable %>%
		mutate(SoilTemp = accumulate(.x = row_number()[-1], .init = 0, .f=function(SoilTemp_dprev, row) {
			data <- cur_data_all()
			
			SoilMeanDepth <- data$SoilMeanDepth[row]
			SurfaceTemp <- data$SurfaceTemp[row]
			GAI <- data$GAI[row]
			
			#Eq. 2.2.1-16
			SoilTemp_d <- SoilTemp_dprev + (SurfaceTemp-SoilTemp_dprev) * 0.24 * exp(-SoilMeanDepth*0.0174) * exp(-0.15*GAI)

			return(SoilTemp_d)
			
		}))
	
	return(result$SoilTemp)
}

calculateVolSoilWaterContent <- function(WaterStorage_dprev, SoilTopThickness, WiltingPoint) {
	VolSoilWaterContent <- WaterStorage_dprev/SoilTopThickness
	VolSoilWaterContent_return <- ifelse(VolSoilWaterContent == 0, WiltingPoint, VolSoilWaterContent)
	return(VolSoilWaterContent_return)
}

calculateWaterStorage <- function(InputTable) {
	WaterStorage_dprev_initial <- InputTable$FieldCapacity[1]*InputTable$SoilTopThickness[1]
	#Input should be a df/tibble with these columns:
	#SoilTopThickness (float)
	#WiltingPoint (float)
	#FieldCapacity (float)
	#SoilAvailWater (float)
	#ET_c (float)
	#alfa (float)
	result = InputTable %>%
		#Subtract JulianDay by its first value to get 0.
		#Since accumulate() takes .init from .x[[1]], this sets the initial value to 0 (Eq. 2.2.1-15).
		mutate(d = accumulate(.x = row_number()[-1], .init = WaterStorage_dprev_initial, .f=function(WaterStorage_dprev, row) {
			data <- cur_data_all()
			
			SoilTopThickness <- data$SoilTopThickness[row]
			WiltingPoint <- data$WiltingPoint[row]
			FieldCapacity <- data$FieldCapacity[row]
			SoilAvailWater <- data$SoilAvailWater[row]
			ET_c <- data$ET_c[row]
			alfa <- data$alfa[row]
			
			#Calculate volumetric soil water content
			#Eq. 2.2.1-23 to 2.2.1-24
			VolSoilWaterContent <- calculateVolSoilWaterContent(WaterStorage_dprev, SoilTopThickness, WiltingPoint)
			
			#Calculate actual evapotranspiration
			#Eq. 2.2.1-25
			K_r <- (1 - ((0.95 * FieldCapacity - VolSoilWaterContent)/(0.95 * FieldCapacity - alfa*WiltingPoint)))^2
			
			#Eq. 2.2.1-26
			K_r <- pmin(pmax(0,K_r),1)
			
			#Eq. 2.2.1-27
			if(VolSoilWaterContent < alfa/100*WiltingPoint) {
				K_r <- 0
			}
			
			#Eq. 2.2.1-28
			ET_a <- ET_c * K_r
			
			#Eq. 2.2.1-29 and #Eq. 2.2.1-30 are addressed in the .init parameter of
			#the "accumulate" function
			
			#Calculate water storage
			#Eq. 2.2.1-31
			DeepPerc <- WaterStorage_dprev - FieldCapacity*SoilTopThickness
			#Eq. 2.2.1-32
			DeepPerc <- ifelse(DeepPerc<0,0,DeepPerc)
			
			#Eq. 2.2.1-33
			WaterStorage_d <- WaterStorage_dprev + SoilAvailWater - ET_a - DeepPerc
			
			return(WaterStorage_d)
			
		}))
	
	return(result$d)
}
