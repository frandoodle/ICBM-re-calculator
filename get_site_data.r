# This is a functionction which gets the data table for all treatments of 
# a particular site from the main site database file.


getSiteData <- function(location_data,
												site_name)
	{
	
	site_data <- location_data %>%
		filter(location_name == site_name) %>%
		select(location_name,
					 treatment_number,
					 treatment_name,
					 replication_number,
					 year_name,
					 sand_px,
					 clay_px,
					 silt_px,
					 soil_total_carbon_px,
					 grain_yield_kgha,
					 hay_yield_kgha,
					 straw_biomass_kgha,
					 straw_returned,
					 crop_residue_kgha,
					 hay_residue_kgha...23,
					 roots_residue_kgha,
					 manure_kgha,
					 
					 tillage,
					 province
		)
	
	return(site_data)
	}

