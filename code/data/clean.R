# # This cleans and integrates data from sources listed below.
# # The data is used in subsequent analysis in other files.

# 1. UN COMTRADE TRADE database
# 2. UCDP-PRIO dataset for battle deaths and intrastate war

save_csv <- 1 # set to 1 if you want to save data

data_path <-  paste0(DATA_DIR, "/data/")

packages <- c(
  "haven", 
  "tidyverse",
  "lubridate",
  "ViewPipeSteps",
  "ggplot2",
  "dplyr",
  "readr",
  "states",
  "timetk",
  "tnet"
  )

pacman::p_load(packages,
               character.only = TRUE,
               install = TRUE)

############### READ DATA ############### 

### COMTRADE trade data of weapons
comtrade <- read.csv(paste0(data_path, "raw/COMTRADE/comtrade.csv")) 


ucdp_brd <- read.csv(paste0(data_path, "raw/ucdp-brd-dyadic-211.csv")) %>%
  mutate(region = recode_factor(region,
                                `1` = "Europe",
                                `2` = "Middle East",
                                `3` = "Asia",
                                `4` = "Africa", 
                                `5` = "Americas"))

ucdp_prio <- read.csv(paste0(data_path, "raw/ucdp-prio-acd-211.csv")) %>%
  mutate(end_year = year(as.Date(ep_end_date)),
         start_year = year(as.Date(start_date))) %>%
  mutate(region = recode_factor(region,
                                `1` = "Europe",
                                `2` = "Middle East",
                                `3` = "Asia",
                                `4` = "Africa", 
                                `5` = "Americas"),
         
         incompatibility = recode_factor(incompatibility,
                                `1` = "territory",
                                `2` = "government",
                                `3` = "both"),
         
         type_of_conflict = recode_factor(incompatibility,
                                         `1` = "extrasystemic",
                                         `2` = "interstate",
                                         `3` = "intrastate",
                                         `3` = "internationalized intrastate")) %>%
  filter(!grepl(',', region))


## country matching between UCDP/PRIO and comtrade 
country_match <- read.csv(paste0(data_path, "intermediate/match_country.csv"))



############### CREATE INTERMEDIATE DATA ############### 

start_year_data <- 1990
end_year_data <- 2019

### Conflict

## Battle deaths in region per year
battle_death <- ucdp_brd %>%
  filter(type_of_conflict != 2) %>%
  select(location_inc, year, bd_best, region, type_of_conflict, incompatibility) %>%
  filter(year >= start_year_data & year <= end_year_data) %>%
  rename(country = location_inc,
         bd = bd_best) %>%
  group_by(country, year, region) %>%
  summarise(bd = sum(bd),
            type_of_conflict = type_of_conflict[1],
            incompatibility = incompatibility[1]) %>%
  mutate(incompatibility = recode_factor(incompatibility,
                                         `1` = "territory",
                                         `2` = "government",
                                         `3` = "both"),
         
         type_of_conflict = recode_factor(incompatibility,
                                          `1` = "extrasystemic",
                                          `2` = "interstate",
                                          `3` = "intrastate",
                                          `4` = "internationalized intrastate"))
  
## End of battle
battle_end <- ucdp_prio %>%
  select(location, year, start_year, 
         end_year, intensity_level, 
         cumulative_intensity, region) %>%
  filter(year >= start_year_data,
           year <= end_year_data,
           is.na(end_year) == FALSE)

### trade data

battle_weapon <- comtrade %>%
  filter(trade_flow == "Export") %>%
  select(year, reporter, partner, trade_value_usd)

# create direcotry for intermediate data
ifelse(!dir.exists(file.path(paste0(data_path, "intermediate"))), 
       dir.create(file.path(paste0(data_path, "intermediate"))), 
       "directory exists")

if (save_csv == 1){
  write.csv(battle_death, paste0(data_path, "intermediate/battle_death.csv"))
  write.csv(battle_end,   paste0(data_path, "intermediate/battle_end.csv"))
  write.csv(battle_weapon, paste0(data_path, "intermediate/battle_weapon.csv"))
}


############### CREATE MAIN DATA ############### 

## year, country, total import of weapons
weapon_export <- comtrade %>%
  filter(trade_flow == "Export") %>%
  select(year, reporter, partner, partner_iso, trade_value_usd) %>%
  group_by(year, partner, partner_iso) %>%
  summarise(total_import = sum(trade_value_usd, na.rm = TRUE)) %>%
  rename(comtrade_country = partner) %>%
  arrange(comtrade_country, year) %>%
  ungroup()

weapon_from <- comtrade %>%
  filter(trade_flow == "Export") %>%
  select(year, reporter, partner, trade_value_usd) %>%
  group_by(year, reporter) %>%
  summarise(total_export = sum(trade_value_usd, na.rm = TRUE)) %>%
  rename(country = reporter) %>%
  arrange(country, year)

## region, end_year, end_intensity
conflict_end <- data.frame()
for (r in unique(battle_end$region)){
  battle_end_region <- battle_end %>%
    filter(region != r) %>%
    select(-region) %>%
    group_by(end_year) %>%
    summarise(num_end = n(),
              endyear_intensity = sum(intensity_level),
              cumulat_intensity = sum(cumulative_intensity)) %>%
    mutate(region = r)
  
  conflict_end <- rbind(conflict_end, battle_end_region)
}

conflict_end <- conflict_end %>%
  rename(year = end_year)

conflict_end_pivot <- conflict_end %>%
  pivot_wider(names_from = year, 
              names_glue = "{.value}_{year}",
              values_from = c(num_end, endyear_intensity, cumulat_intensity))

network_export <- comtrade %>%
  filter(trade_flow == "Export") %>%
  select(year, reporter_code, partner_code, trade_value_usd) %>%
  filter(partner_code > 0)


## compute centrality of each country based on weapon trade network
centrality_df <- data.frame()
for (dat_year in unique(network_export$year)){
  network_export_year <- network_export %>%
    filter(year == dat_year) %>%
    select(reporter_code, partner_code, trade_value_usd) %>%
    distinct()
  centrality_year <- as.data.frame(degree_w(network_export_year,
                                            measure=c("degree","output"), 
                                            type="in", alpha=1))
  
  centrality_year$year <- dat_year
  
  centrality_df <- rbind(centrality_df, centrality_year)
}

centrality_df <- centrality_df %>%
  rename(partner_code = node)
centrality <- left_join(centrality_df, comtrade[, c("partner_code", "partner")], by = "partner_code") %>%
  distinct() %>%
  drop_na(partner) %>%
  rename(comtrade_country = partner)

############### COMBINE DATA ############### 

main <- battle_death %>%
  left_join(country_match, by = "country") %>%
  mutate(comtrade_country = coalesce(comtrade_country, country)) %>%
  left_join(weapon_export %>% select(comtrade_country, partner_iso) %>% distinct(), 
            by = "comtrade_country") %>%
  left_join(weapon_export %>% select(-partner_iso), by = c("year", "comtrade_country")) %>%
  select(-comtrade_country) %>%
  left_join(conflict_end, by = c("region", "year")) %>%
  arrange(country, year) %>%
  mutate(logimport = log(total_import),
         log_battledeath = log(bd)) %>%
  mutate(conflict = ifelse(bd > 0 , 1, 0))

main_pivot <- weapon_export %>%
  left_join(centrality, by = c("comtrade_country", "year")) %>%
  left_join(country_match, by = "comtrade_country") %>%
  mutate(country = coalesce(country, comtrade_country)) %>%
  left_join(battle_death, by = c("country", "year")) %>%
  group_by(country) %>% 
  filter(!all(is.na(bd))) %>% 
  tidyr::fill(region, .direction = "updown") %>%
  replace_na(list(bd = 0)) %>%
  select(-comtrade_country) %>%
  left_join(conflict_end_pivot, by = c("region")) %>%
  arrange(country, year) %>%
  mutate(logimport = log(total_import + 1),
         log_battledeath = log(bd + 1))


nyears_backwards <- 5
year_list <- sort(unique(main_pivot$year))

main_backwards <- data.frame()
for (dat_year in year_list){
  
  main_sub <- main_pivot %>%
    filter(year == dat_year) %>%
    select(country, year, region, bd, log_battledeath, total_import, 
           logimport, type_of_conflict, incompatibility, degree, output,
           one_of(paste0("num_end_", dat_year - nyears_backwards:0)),
           one_of(paste0("endyear_intensity_", dat_year - nyears_backwards:0)),
           one_of(paste0("cumulat_intensity_", dat_year - nyears_backwards:0)))
  
  for (years_backwards in 0:nyears_backwards){
      
      colnames(main_sub)[colnames(main_sub) %in% 
                           c(paste0("num_end_", dat_year - years_backwards),
                             paste0("endyear_intensity_", dat_year - years_backwards),
                             paste0("cumulat_intensity_", dat_year - years_backwards))] <- 
        c(paste0("num_end_", years_backwards),
          paste0("endyear_intensity_", years_backwards),
          paste0("cumulat_intensity_", years_backwards))
      
  }
  
  main_backwards <- rbind(main_backwards, main_sub)

}

main_backwards <- main_backwards %>%
  arrange(country, year) %>%
  group_by(country) %>%
  tk_augment_lags(logimport, .lags = 1:nyears_backwards)


if (save_csv == 1){
  ifelse(!dir.exists(file.path(paste0(data_path, "final"))), dir.create(file.path(paste0(data_path, "final"))), "directory exists")
  write.csv(main, paste0(data_path, "final/main.csv"), row.names = F)
  write.csv(main_backwards, paste0(data_path, "final/main_backwards.csv"), row.names = F)
}

