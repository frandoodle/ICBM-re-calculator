
calculateGAI <- function(InputTable) {
	#Input should be a df/tibble with at least the columns JulianDay (int),
	#Yield (float), and perennial (bool)
	
	#Holos V4 constant values for different crop types (see section 2.2.1.1.1.1,
	#Green area index dynamics)
	EmergenceDay <- ifelse(InputTable$perennial == TRUE, 75, 141)
	RipeningDay <- ifelse(InputTable$perennial == TRUE, 300, 197)
	Variance <- ifelse(InputTable$perennial == TRUE, 1500, 300)
	
	#Eq. 2.2.1-1
	GAI_max = 0.0731*(InputTable$Yield/1000)^2 + 0.408*(InputTable$Yield/1000)
	
	#Eq. 2.2.1-2
	MidSeason = EmergenceDay + ((RipeningDay-EmergenceDay)/2)
	
	#Eq. 2.2.1-3
	GAI = GAI_max*exp(-(((InputTable$JulianDay-MidSeason)^2)/(2*Variance)))
	
	return(tibble(JulianDay = InputTable$JulianDay, GAI_max = GAI_max, MidSeason = MidSeason, GAI = GAI))
}

calculateWaterContent <- function(InputTable) {
	#Input should be a df/tibble with at least the columns JulianDay (int),
	#SoilOrganicC_Percent (float), ClayContent (float), and SandContent (float)
	
	#Eq. 2.2.1-4
	OrgCfactor = -0.837531 + 0.430183*InputTable$SoilOrganicC_Percent
	#Eq. 2.2.1-5
	Clayfactor = -1.40744 + 0.0661969*InputTable$ClayContent*100
	#Eq. 2.2.1-6
	Sandfactor = -1.51866 + 0.0393284*InputTable$SandContent*100
	
	#Eq. 2.2.1-7
	WiltingPointPercent = 14.2568+7.36318*(
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
	WiltingPoint = WiltingPointPercent/100
	
	#Eq. 2.2.1-9
	FieldCapacityPercent = 29.7528+10.3544*(
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
	FieldCapacity = FieldCapacityPercent/100
	
	return(tibble(JulianDay = InputTable$JulianDay,
								OrgCfactor = OrgCfactor,
				 Clayfactor = Clayfactor,
				 Sandfactor = Sandfactor,
				 WiltingPoint = WiltingPoint,
				 FieldCapacity = FieldCapacity))
}

# This implementation doesn't work because it doesn't take into account the historical progression of soil temperature values. It goes from 0 to the new steady state, without taking into account the actual starting point.
# calculateSoilTempRecursive <- function(JulianDay,
# 															SurfaceTemp,
# 															SoilMeanDepth,
# 															GAI)
# 	{
# 		if(JulianDay == 1) {
# 			SoilTemp_d = 0
# 		} else {
# 			
# 			SoilTemp_dprev = calculateSoilTemp(JulianDay-1, SurfaceTemp, SoilMeanDepth, GAI)
# 			SoilTemp_d = SoilTemp_dprev + (SurfaceTemp-SoilTemp_dprev)*0.24*exp(-SoilMeanDepth*0.0174)*exp(-0.15*GAI)
# 			}
# 	
# 	return(SoilTemp_d)
# }

calculateSurfaceTemp <- function(InputTable){
	#Input should be a df/tibble with at least the columns JulianDay (int),
	#Tavg (float), and everything that's required for calculateGAI().
	LeafAreaIndex = 0.8*calculateGAI(InputTable)[["GAI"]]
	SurfaceTemp <- ifelse(InputTable$Tavg < 0, 0.20*InputTable$Tavg, InputTable$Tavg*(0.95+0.05*exp(-0.4*(LeafAreaIndex-3))))
	return(SurfaceTemp)
}

calculateSoilTemp <- function(JulianDay,SoilTemp_dprev, SoilMeanDepth, SurfaceTemp, GAI) {
	SoilTemp_d = ifelse(JulianDay == 1, 0,SoilTemp_dprev + (SurfaceTemp-SoilTemp_dprev)*0.24*exp(-SoilMeanDepth*0.0174)*exp(-0.15*GAI))
	return(SoilTemp_d)
}

# aaa <- daily_input_test %>%
# 	mutate(b = calculateSoilTemp(.$JulianDay, lag(b), 250, SurfaceTemp[1], GAI[1]))


#SoilMeanDepth
zz <- function(JulianDay, SoilTemp_dprev) {
	GAI <- calculateGAI(InputTable)[["GAI"]]
	SurfaceTemp <- calculateSurfaceTemp(InputTable)
	if(JulianDay == 1) {
		SoilTemp_d = 0
	} else {
		SoilTemp_d = calculateSoilTemp(JulianDay-1, SurfaceTemp, SoilMeanDepth, GAI)
	}
}

xx <- function(JulianDay, SoilTemp_d) {
	SoilTemp_dnew <- SoilTemp_d + (SurfaceTemp-SoilTemp_d)*0.24*exp(-SoilMeanDepth*0.0174)*exp(-0.15*GAI)
	return(SoilTemp_dnew)
}
