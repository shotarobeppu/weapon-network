# # This calls the comtrade api to extract arms trade by country pair.
## The call is limited to 10000 observations and 1 second apart due to comtrade api's settings.

packages <- c(
  "comtradr",
  "RCurl",
  "RJSONIO",
  "stringr",
  "tidyverse"
)

pacman::p_load(packages,
               character.only = TRUE,
               install = TRUE)

comtrade_weapon = 0

dir_path <- paste0(DATA_DIR, "COMTRADE_TOTAL")

##################################
### Country Code and Name ###
##################################

csv_path = file.path(dir_path, "reporterAreas.csv")
reporters_df = read.csv(csv_path)        
## Read info from local csv
reporters = reporters_df[2:length(reporters_df$text),]    
names(reporters) = c("ccode", "cname")


##################################
### API call ###
##################################

comtrade_df = data.frame()

start_year = 2005
end_year = start_year + 4
data_length_per_group <- ceiling(length(reporters$cname)/5)
for (i in 1:data_length_per_group){
  
  first <- 5*(i-1) + 1
  second <- 5*i
  if (5*i <= length(reporters$cname)){
    sample_countries <- reporters$cname[first: second]
  } else {
    sample_countries <- reporters$cname[first: length(reporters$cname)]
  }
  print(i)
  print(sample_countries)

  if (comtrade_weapon == 1){
    
    sample <- ct_search(reporters = sample_countries, 
                        partners = "All", 
                        trade_direction = "all", 
                        freq = "annual",
                        start_date = start_year, end_date = end_year, 
                        commod_codes = "93", 
                        type = "goods",
                        url = "https://comtrade.un.org/api/get?")
    
  } else{
    
    sample <- ct_search(reporters = sample_countries, 
                        partners = "All", 
                        trade_direction = "all", 
                        freq = "annual",
                        start_date = start_year, end_date = end_year,
                        type = "goods",
                        url = "https://comtrade.un.org/api/get?")
  }
  
  comtrade_df = rbind(comtrade_df, sample)
  
  Sys.sleep(1)
}
write.csv(comtrade_df, paste0(dir_path, "/", "comtrade_", start_year, ".csv"))

ct_get_remaining_hourly_queries()
ct_get_reset_time()

##################################
### Combine ###
##################################
data_path <- DATA_DIR
comtrade <- list.files(path = paste0(data_path, "COMTRADE"),  
                       pattern = "comtrade*", full.names = TRUE) %>% 
  lapply(read_csv) %>%  
  bind_rows   

write.csv(comtrade, paste0(data_path, "COMTRADE/comtrade.csv"))



