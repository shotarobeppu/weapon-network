# This creates the animation of weapon trades between countries using the
# COMTRADE data
library(readr)
library(RCurl)
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(gganimate)
library(graphlayouts)
library(patchwork)

## construct data --------------------------------------------------------------

## combine with geocoordinates
country_df <-  read.csv("data/raw/country.csv")
urlfile <- "https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv"
country_coordinates_df <-read_csv(url(urlfile)) %>%
  left_join(country_df, by = c("ISO 3166 Country Code" = "alpha.2")) %>%
  select(alpha.3, Latitude, Longitude) %>% 
  add_row(alpha.3 = "SCG",
          Latitude = 44, 
          Longitude = 21)

## combine with trade
comtrade_df <- read.csv("data/raw/COMTRADE/comtrade.csv")
network_df <- comtrade_df %>%
  filter(trade_flow == "Export") %>%
  select(year, reporter, reporter_iso, partner, partner_iso, trade_value_usd) %>%
  filter(partner != "World") %>%
  left_join(country_coordinates_df, by = c("reporter_iso" = "alpha.3")) %>%
  left_join(country_coordinates_df, by = c("partner_iso" = "alpha.3"), suffix = c("_reporter", "_partner")) %>%
  filter(!is.na(partner_iso))

## and with conflict data
main_df <- read.csv("data/final/main.csv")
ongoing_conflict_df <- main_df %>%
  select(year, partner_iso, conflict)


## create node and edge dataframes
node_df <- country_coordinates_df %>%
  rename(
    name = alpha.3,
    lon = Longitude,
    lat = Latitude
  ) %>%
  drop_na() %>%
  mutate(id = row_number()) %>%
  select(id, lon, lat, name)

edges_df <- network_df %>%
  left_join(ongoing_conflict_df, by = c("year", "partner_iso")) %>%
  replace_na(list(conflict = 0)) %>%
  rename(
    tail = reporter_iso,
    head = partner_iso,
    weight = trade_value_usd,
    onset = year
  ) %>%
  mutate(terminus = onset + 1,
         duration = 1) %>%
  select(tail, head, weight, onset, terminus, conflict) %>%
  left_join(select(node_df, name, id), by = c("tail" = "name")) %>%
  rename(from = id) %>%
  left_join(select(node_df, name, id), by = c("head" = "name")) %>%
  rename(to = id,
         category = conflict) %>%
  mutate(category = as.factor(category)) %>%
  select(from, to, weight, category, onset) %>%
  mutate(conflict = recode_factor(category,
                                  `1` =  "Conflict",
                                  `0` = "Peace")) %>%
  drop_na() %>%
  inner_join(node_df %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(node_df %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

all_edges_df <- edges_df %>%
  group_by(onset) %>%
  filter(category == 1 | quantile(weight, 0.9) < weight) %>%
  mutate(id = paste(from, to, sep = "-"),
         status = TRUE) %>%
  filter(weight > 0)

all_nodes_df <- node_df %>% 
  slice(rep(1:n(), each = length(unique(all_edges_df$onset)))) %>%
  group_by(id) %>%
  mutate(onset = min(unique(all_edges_df$onset)) + row_number() - 1) %>%
  left_join(ongoing_conflict_df,
            by = c("onset" = "year", "name" = "partner_iso")) %>%
  replace_na(list(conflict = 0)) %>%
  mutate(conflict = as.factor(conflict))


year_var <- 1997
edges_year_df <- edges_df %>%
  filter(onset == !!year_var)  %>%
  filter(category == 1 | quantile(weight, 0.9) < weight)
node_year_df <- all_nodes_df %>%
  filter(onset == !!year_var)

## export node and edge df
write.csv(all_nodes_df, "data/network/all_node.csv", row.names = F)
write.csv(all_edges_df, "data/network/all_edge.csv", row.names = F)
write.csv(node_df, "data/network/node.csv", row.names = F)
write.csv(edges_df, "data/network/edge.csv", row.names = F)



## create graph ----------------------------------------------------------------
g <- graph_from_data_frame(edges_year_df, directed = T, vertices = node_df)
node_df$weight = igraph::degree(g)

## get the globe
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

## draw graph for one year only
p <- ggplot(node_year_df) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = conflict, size = weight),
             data = edges_year_df, curvature = 0.2,
             alpha = 0.2,
             arrow = arrow(length = unit(0.2,"cm"))) +
  scale_size_continuous(guide = FALSE, range = c(0.1, 2)) + # scale for edge widths
  geom_point(
    aes(lon, lat, group = name, fill = conflict),
    shape = 21, size = 1, show.legend = FALSE
  )  +
  scale_fill_manual(values = c("lightblue", "red")) + 
  scale_size_continuous(guide = "none", range = c(1, 6)) +    # scale for node size
  mapcoords + maptheme +
  labs(title = paste("Year", year_var))


ggsave("output/images/network/example.png", dpi = 300)


## draw graph this time dynamically
p <- ggplot() + country_shapes +
  geom_curve(data = all_edges_df,
             aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = conflict, size = weight),
             curvature = 0,
             alpha = 0.2,
             arrow = arrow(length = unit(0.2,"cm"))) +
  scale_size_continuous(guide = FALSE, range = c(1, 3)) + 
  geom_point(
    data = all_nodes_df, aes(lon, lat, group = name, fill = conflict),
    shape = 21, size = 1, show.legend = FALSE
  ) +
  scale_fill_manual(values = c("white", "red")) +
  ease_aes("quadratic-in-out") +
  transition_states(onset, state_length = 1, wrap = FALSE) +
  labs(title = "Year {closest_state}") +
  mapcoords + maptheme 

saveHTML(animate(p, 
                 nframes = 49,         # fewer frames for simplification
                 device = "current"), 
         img.name = "gganimate_plot", 
         htmlfile = "output/images/network/gg.html")

## output as gif file for presentation
mygif <- animate(p, width=800, height=400, renderer=gifski_renderer(loop=FALSE))  
# anim_save(filename="/Users/shotaro/GitHub/weapon_network/output/images/network/animation.gif", mygif)






