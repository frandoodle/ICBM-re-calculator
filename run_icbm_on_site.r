# Running ICBM + Re using get_site_data

ex_climate_data <- getClimateData(
	climate_data_folder = "climate_data",
	master_data_folder = "master",
	site_name = "244014")

ex_site_data <- getSiteData(readr::read_csv("all_experiments_fin.csv"),
														"Ellerslie")

perennial = FALSE

ex_results <- ex_site_data %>%
	group_by(treatment_name) %>%
	group_split %>%
	map(~RunICBMAndRe(yield = .$grain_yield_kgha,
										iag = .$crop_residue_kgha,
										ibg = .$roots_residue_kgha,
										iman = .$manure_kgha,
										ag_init = 0,
										bg_init = 0,
										o_init = 0,
										DailyClimateTable = ex_climate_data,
										perennial = perennial,
										SoilOrganicC_Percent = .$soil_total_carbon_px,
										ClayContent = .$clay_px,
										SandContent = .$sand_px,
										irrigation_use_estimate = FALSE))
	
