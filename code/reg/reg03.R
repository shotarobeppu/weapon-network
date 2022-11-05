# This file is for the main analysis for the shift-share analysis
# used in the last section of the paper with a 10 year window

rm(list = ls())

github_path <- "/Users/shotaro/GitHub/weapon_network/"
data_path <- "/Users/shotaro/GitHub/weapon_network/data/"

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
  "stats"
)

pacman::p_load(packages,
               character.only = TRUE,
               install = TRUE)


############### READ DATA ############### 

main <- read.csv(paste0(data_path, "final/ss_1995_df.csv")) %>%
  group_by(country) %>%
  mutate(logimp = log(total_import + 1),
         logbd = log(bd + 1),
         ss_lag = lag(ss, order_by = year))

main_10 <- main %>% 
  mutate(ranges = cut(year,
                      seq(1994, 2019, 5))) %>%
  group_by(country, ranges) %>% 
  dplyr::summarize(ss = sum(ss),
                   bd = sum(bd),
                   total_import = sum(total_import)) %>%
  mutate(conflict = ifelse(bd > 0, 1, 0))

first = feols(log(total_import+1) ~ log(ss+1) | country + ranges,  main_10)
etable(first)


est_iv = feols(log(bd+1) ~ 1 | 
                 country + ranges | 
                 log(total_import +1) ~ log(ss+1) + conflict, main_10)

fitstat(est_iv, ~ ivf1 + ivwald1 + ivf2 + ivwald2, cluster = "country")


summary(est_iv, stage = 1)

etable(est_iv)

etable(summary(est_iv,stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p, 
       file = paste0(github_path, "output/regression/ss10.tex"))
