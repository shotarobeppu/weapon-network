# This constructs the shift-share instrument.
# This is different from other shift-share since I use two paths; 
# country ending its conflict to the producer
# and the producer to another country
# finally, this notebook creates the shift-share instrument for "reg_03.r"


library(dplyr)
library(purrr)

## load data -------------------------------------------------------------------

## load data
main <- read.csv("data/final/main.csv")
mainb <- read.csv("data/final/main_backwards.csv") %>%
  select(-region)
battle_weapon <- read.csv("data/intermediate/battle_weapon.csv")
weapon_country_list <- union(unique(battle_weapon$reporter), 
                             unique(battle_weapon$partner))
match_country_df <- read.csv("data/intermediate/match_country.csv")


country_iso_df <- main %>%
  select(country, partner_iso, region) %>%
  filter(!is.na(partner_iso)) %>%
  distinct()

import_df <- read.csv("data/intermediate/battle_weapon.csv") %>%
  select(-X) %>%
  filter(partner != "World") %>%
  group_by(partner, year) %>%
  summarise(total_import = sum(trade_value_usd, na.rm = T)) %>%
  rename(country = partner)

country_list <- sort(union(unique(main$country), 
                           union(unique(mainb$country), 
                                 unique(battle_weapon$reporter))))

end_df <- read.csv("data/intermediate/battle_end.csv") %>%
  
  rename(country = location) %>%
  select(country, region, end_year) %>%
  distinct() %>%
  arrange(end_year) %>%
  left_join(match_country_df, by = "country") %>%
  mutate(country = coalesce(comtrade_country, country)) %>%
  select(-comtrade_country)

end_country_list = list()
for (i in 1:nrow(end_df)){
  country_i_list = end_df$country[i][[1]]
  end_country_list[i] = list(country_list %in% country_i_list)
}

end_df$end_country <- end_country_list


## create shift-share ----------------------------------------------------------

## this creates for each individual country the total effect from an end in conflict
create_share_var <- function(year_var, which){
  
  if (which == "reporter"){
    other = "partner"
  } else {
    other = "reporter"
  }
  
  share_weapon_df <-  read.csv("data/intermediate/battle_weapon.csv") %>%
    select(-X) %>%
    filter(partner != "World") %>%
    filter(year == !!year_var) %>%
    select(-year) %>%
    group_by(!!rlang::sym(which)) %>%
    mutate(share = trade_value_usd/sum(trade_value_usd, na.rm = T)) %>%
    ungroup()
  
  pair_list = list()
  share_list = list()
  for (i in 1:nrow(share_weapon_df)){
    pair_i_list = share_weapon_df[[other]][i]
    pair_list[i] = list(weapon_country_list %in% pair_i_list)
    share_list[i] = lapply(pair_list[i],"*",share_weapon_df$share[i])
  }
  
  share_weapon_df <- share_weapon_df %>%
    mutate(pair_list = pair_list,
           share_list = share_list) 
  
  share_weapon_list = list()
  for (i in 1:length(unique(share_weapon_df[[which]]))){
    
    country = unique(share_weapon_df[[which]])[i]
    
    country_share <- share_weapon_df %>%
      filter(!!rlang::sym(which) == !!country)
    
    lst <- matrix(unlist(country_share$share_list), ncol = length(weapon_country_list))
    
    share_weapon_list[country] <-  list(colSums(lst, na.rm = FALSE))
  }
  
  if (which == "reporter"){
    reporter_weapon_share <- share_weapon_list
    return(reporter_weapon_share)
  } else{
    partner_weapon_share <- share_weapon_list
    return(partner_weapon_share)
  }
}

year_var = 1995
reporter_weapon_share <- create_share_var(year_var = year_var, which = "reporter")
partner_weapon_share <- create_share_var(year_var = year_var, which = "partner")


## this creates the overall impact

influence_df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("war", "country_of_interest", "influence")
colnames(influence_df) <- x
for (war in weapon_country_list){
  
  war_index <- match(war, weapon_country_list)
  
  for (country_of_interest in names(partner_weapon_share)){
    
    influence <- 0
    
    for (exporter in names(partner_weapon_share)){
      
      exporter_index <- match(exporter, weapon_country_list)
      
      war_influence <- reporter_weapon_share[exporter][[1]][war_index]
      if (is.null(war_influence)) {war_influence = 0}
      
      import_influence <- partner_weapon_share[country_of_interest][[1]][exporter_index]
      
      influence <- influence + war_influence*import_influence
      
    }
    
    influence_df[nrow(influence_df) + 1, ] = c(war, 
                                               country_of_interest, 
                                               influence)
    
  }
}

write.csv(influence_df, "data/intermediate/influence_df.csv", row.names = F)

ss_df <- end_df %>%
  full_join(influence_df, by = c("country" =  "war")) %>%
  mutate(influence = as.numeric(influence)) %>%
  group_by(end_year, country_of_interest) %>%
  summarise(ss = sum(influence, na.rm = T)) %>%
  ungroup() %>%
  drop_na()

write.csv(ss_df, "data/final/ss_df.csv", row.names = F)

ss_1995_df <- data.frame(country = country_list, rep = 25) %>%
  slice(rep(1:n(), each = rep)) %>%
  group_by(country) %>%
  mutate(year = 1995 + row_number() - 1) %>%
  select(-rep) %>%
  
  left_join(country_iso_df, by = "country") %>%
  
  left_join(read.csv("data/intermediate/battle_death.csv") %>%
              select(-X, -type_of_conflict, -incompatibility), 
            by = c("country", "year", "region")) %>%
  replace_na(list(bd=0)) %>%
  
  left_join(read.csv("data/intermediate/battle_end.csv") %>%
              select(-X, -start_year, -intensity_level, -cumulative_intensity), 
            by = c("country" = "location", "year", "region")) %>%
  mutate(end_year = ifelse(is.na(end_year), 0, 1)) %>%
  
  left_join(import_df, by = c("country", "year")) %>%
  replace_na(list(total_import=0)) %>%
  left_join(ss_df, by = c("country" = "country_of_interest", "year" = "end_year")) %>%
  replace_na(list(ss = 0))

write.csv(ss_1995_df, "data/final/ss_1995_df.csv")









