library(here)
library(purrr)

# This function can be used to append new sites to the master climate data files

append_climate_data <- function(climate_data_folder,
															master_folder,
															new_site_folder)
{
	new_master_folder <- paste0(master_folder,"_",new_site_folder,"_appended_",format(Sys.time(), format="%Y_%m_%d_%H_%M_%S"))
	
	# Get master data
	master_files <- list.files(here::here(climate_data_folder, master_folder), full.names = TRUE, pattern = "csv$")
	master_data <- master_files %>%
		purrr::map(~readr::read_csv(., col_types = "ciiiiddddddddd")) %>%
		bind_rows()
	# Get new data
	new_site_files <- list.files(here::here(climate_data_folder, new_site_folder), full.names = TRUE, pattern = "csv$")
	new_site_data <- new_site_files %>%
		purrr::map(~readr::read_csv(., col_types = "ciiiiddddddddd")) %>%
		bind_rows()
	# Check to see if there are any cases where site_name and site_id are not unique
	if (any(unique(master_data$POLYID) %in% unique(new_site_data$POLYID)))
	{
		non_uniques <- unique(master_data$POLYID)[(unique(master_data$POLYID) %in% unique(new_site_data$POLYID))]
		stop(paste0("There are cases where site_id is not unique: ", paste(non_uniques, collapse=', ')))
	}
	# Make new master data
	new_master_data <- master_data %>%
		bind_rows(new_site_data)
	# Write to file
	dir.create(here(climate_data_folder, new_master_folder))
	print(paste0("Writing appended climate data to folder: ", here(climate_data_folder, new_master_folder)))
	new_master_data %>%
		group_by(Year)%>%
		group_split %>%
		map(~readr::write_csv(., file = here(climate_data_folder, new_master_folder, paste0(.$Year[1],".csv")))
	)
}