# This file is for the main analysis for the IV analysis
# using multiple lags and output result to output folder


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

main <- read.csv(paste0(data_path, "final/main_backwards.csv")) %>%
  mutate(logimport_lag0 = logimport) %>%
  mutate(conflict = ifelse(bd > 0 , 1, 0))


############### REGRESSION ############### 

##### First Stage OLS ####

ols_list_first = list()

endvar <- "endyear_intensity_" 
lagnum <- 1
lagnum_end <- 3
endconflict_lags <- paste0(endvar, lagnum:lagnum_end)
endconflict_interactioncon_lags <- paste0(endvar, lagnum:lagnum_end, "*conflict")
endconflict_interactiondeg_lags <- paste0(endvar, lagnum:lagnum_end, "*degree")

for (i in 1:(length(endconflict_lags)-1)){
  
  lags <- endconflict_lags[0:i]
  lags_interactionconf <- endconflict_interactioncon_lags[0:i]
  lags_interactiondeg <- endconflict_interactiondeg_lags[0:i]
  
  formula <- as.formula(paste("logimport~", paste(lags, collapse="+"), 
                              "+", paste(lags_interactionconf, collapse="+"),
                              "+", paste(lags_interactiondeg, collapse="+"),
                              " + conflict + degree| year + country"))
  ols_list_first[[i]] <- feols(formula, main)
  
  print(AIC(ols_list_first[[i]]))
  
}
etable(ols_list_first)
etable(ols_list_first, file = paste0(github_path, "output/regression/ols_first.tex"))

coefplot(ols_list_first)

fe_trade = fixef(ols_list_first[[1]])
plot(fe_trade)

##### OLS #####

ols_list = list()
logimport_lags <- paste0("logimport_lag", 0:5)

for (i in 1:5){

  lags <- logimport_lags[0:i]
  formula <- as.formula(paste("log_battledeath~", paste(lags, collapse="+"), "| year + country"))
  ols_list[[i]] <- feols(formula, main)

  print(AIC(ols_list[[i]]))

}

etable(ols_list, vcov = "twoway", file = paste0(github_path, "output/regression/ols.tex"))


#### IV ####

## first stage

# summary(lm(logimport_lag0 ~ endyear_intensity_0 + endyear_intensity_1, main))

main_iv <- main %>%
  filter(conflict == 1)

est_iv = feols(log_battledeath ~ type_of_conflict| year + country|
                 logimport ~ endyear_intensity_0 +
                 endyear_intensity_0*degree, main_iv)
est_iv

est_iv_nocount = feols(log_battledeath ~ type_of_conflict| year |
                 logimport ~ endyear_intensity_0 +
                 endyear_intensity_0*degree, main_iv)
est_iv

est_iv$collin.var

iv_list = list()
iv_list[[1]] <- est_iv
iv_list[[2]] <- est_iv_nocount

summary(est_iv,  stage = 2)

etable(iv_list, file = paste0(github_path, "output/regression/2sls_fe.tex"))
etable(summary(est_iv, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p, tex = T)
etable(summary(est_iv,stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p, file = paste0(github_path, "output/regression/2sls.tex"))

fe_trade_2sls = fixef(est_iv)
plot(fe_trade_2sls)

