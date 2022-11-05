## this runs all the codes for the analysis

rm(list = ls())

MAIN_DIR <- "/Users/shotaro/GitHub/weapon_network"
DATA_DIR <- paste0(MAIN_DIR, "/data/")

## create data ----------- ----------- ----------- ----------- ----------- -----

# for comtrade data, due to the api setting please run this multiple times
# to get the full data 
source("code/data/comtrade.R")

# create main.csv
source("code/data/clean.R")

# create shift-share 
source("code/data/create_ss.R")


## visualization ----------- ----------- ----------- ----------- ----------- ---

# general plot 
source("code/vis/plots.R")

# animation 
source("code/vis/dynamic_network.R")

## regressions  ----------- ----------- ----------- ----------- ----------- ----

# simple 
source("code/reg/reg01.R")

# IV 
source("code/reg/reg02.R")

# shift-share 
source("code/reg/reg03.R")


## simulation  ----------- ----------- ----------- ----------- ----------- -----

# simulation of the model
source("code/sim/simulation.R")
