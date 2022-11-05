# This conducts an initial analysis on the relationship 
# between weapon imports and conflict intensity using a 
# simple OLS

rm(list = ls())

github_path <- "/Users/shotaro/GitHub/weapon_network"
data_path <- "/Users/shotaro/Downloads/weapon_data/"

packages <- c(
  "haven", 
  "tidyverse",
  "lubridate",
  "ViewPipeSteps",
  "ggplot2",
  "dplyr",
  "readr",
  "plm",
  "fixest",
  "AER"
)

pacman::p_load(packages,
               character.only = TRUE,
               install = TRUE)

############### READ DATA ############### 

main <- read.csv(paste0(data_path, "final/main.csv")) %>%
  filter(year >= 1995) %>%
  mutate(time = year - min(year) + 1)

############### REGRESSION ############### 

reg_pois = plm(bd ~ logimport | endyear_intensity + cumulat_intensity + num_end, 
               data = main,
               index = c("country", "year"), 
               model = "within")
summary(reg_pois)


est_iv_fe = feols(log(bd) ~  | year + country | logimport ~  
                    endyear_intensity + cumulat_intensity + num_end, main)
est_iv_fe

etable(summary(est_iv_fe, stage = 1:2))
