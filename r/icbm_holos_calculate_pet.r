holos_calculate_pet <- function(meanDailyTemperature,
															solarRadiation,
															relativeHumidity){
	# This is a vectorized implementation of the Holos reference PET calculator.
	# https://github.com/holos-aafc/Holos/blob/main/H.Core/Calculators/Climate/EvapotranspirationCalculator.cs
	
	term1 = 0.013
	term2 = meanDailyTemperature / (meanDailyTemperature + 15)
	term3 = (23.8856 * solarRadiation) + 50
	term4 = 1 + ((50 - relativeHumidity) / 70)
	
	result <- ifelse(relativeHumidity >= 50,
									 term1 * term2 * term3,
									 term1 * term2 * term3 * term4)
	result <- ifelse(result < 0, 0, result)
	result <- ifelse(meanDailyTemperature <= 0, 0, result)
	
	return(result)
}