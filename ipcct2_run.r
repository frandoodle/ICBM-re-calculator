

dir.create(tempdir()) # This fixes a bug if the temporary directory is not found

here::i_am("ipcct2_run.r")

source("SOCTier2Model.r")

run_ipcct2 <- function(site_data,
climate_data,
init_active = 0,
init_slow = 0,
init_passive = 0,
...)
{

    polyid <- unique(site_data$POLYID)
    climate_data_polyid <- climate_data %>%
        filter(POLYID == polyid)
    simulation_years <- unique(site_data$year_name)

site_data <- sitedata_ex_onesite
site_data_ipcct2 <- site_data %>%
mutate(
    site = as.character(POLYID),
    year = year_name,
    sand = sand_px/100,
    cinput = crop_residue_kgha,
    ligfrac = cinput_lignin_px/100,
    nfrac = cinput_n_px/100,
    till = tillage
) %>%
mutate(till = ifelse(till == "CT", "FT", till)) %>%
select(site,year,sand,cinput,ligfrac,nfrac,till)
site_data_ipcct2

irrigation_data <- site_data_ipcct2 %>%
select(site, year, irrig = irrigation) %>%
mutate(irrig = ifelse(is.na(irrig) || irrig == 0, 0, 1))
irrigation_data


climate_data
climate_data_ipcct2 <- climate_data %>%
group_by(site = as.character(POLYID), year = Year, month = Month) %>%
summarise(tavg = mean(Tavg),
mappet = ifelse(is.infinite(sum(PREC) / sum(PET)),0,sum(PREC) / sum(PET))

) %>%
full_join(irrigation_data, by=c("site", "year")) %>%
filter(year %in% simulation_years) %>%
filter(site %in% polyid)

climate_data_ipcct2

init_active = 0
init_slow = 0
init_passive = 0

    IPCCTier2SOMmodel(SiteData = site_data_ipcct2,
    wth = climate_data_ipcct2,
    init.active = init_active,
    init.slow = init_slow,
    init.passive = init_passive,
    params)
}