# This creates plots used for presentations as well as basic 
# descriptive analysis using the data.
github_path <- MAIN_DIR

start_year_df <- ucdp_prio %>%
  drop_na(end_year)

end_year_df <- ucdp_prio %>%
  drop_na(end_year) %>%
  distinct(location, end_year, .keep_all= TRUE)


start_end_year_p <- ggplot() +
  geom_histogram(aes(start_year, fill = "red1"), binwidth = 2, fill = "red1", data = start_year_df, alpha = 0.6) +
  geom_histogram(aes(end_year, fill = "lightblue"), binwidth = 2, data = end_year_df, alpha = 0.6) +
  scale_fill_manual(name="conflict", values=c("r" = "red1", "b"="grey"), labels=c("b"="End", "r"="Start")) + 
  labs(x = "year", title = "Number of conflicts started/ended each year")
start_end_year_p
ggsave(paste0(github_path, "/output/images/start_end_year.png"))

start_end_year_region_p <- ggplot() +
  geom_histogram(aes(start_year, fill = "red1"), binwidth = 5, fill = "red1", data = start_year_df, alpha = 0.6) +
  geom_histogram(aes(end_year, fill = "lightblue"), binwidth = 5, data = end_year_df, alpha = 0.6) +
  scale_fill_manual(name="conflict", values=c("r" = "red1", "b"="grey"), labels=c("b"="End", "r"="Start")) + 
  facet_wrap("region") + 
  labs(x = "year", title = "Number of conflicts started/ended each year")
start_end_year_region_p
ggsave(paste0(github_path, "/output/images/start_end_year_region.png"))

intensity_yearly_df <- ucdp_prio %>%
  group_by(year) %>%
  dplyr::summarise(mean_intensity = mean(intensity_level),
            mean_cumulative_intensity = mean(cumulative_intensity))

intensity_yearly_p <- intensity_yearly_df %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_intensity, color = "intensity")) +
  # geom_line(aes(x = year, y = mean_cumulative_intensity, color = "cumulative")) +
  scale_color_manual(name = "Category", values = c("intensity" = "tomato1", "cumulative" = "grey1")) +
  labs(y = "intensity", title = "Intensity of Battles")
intensity_yearly_p
ggsave(paste0(github_path, "/output/images/intensity_yearly.png"))

ggplot(ucdp_brd) +
  geom_histogram(aes(bd_best))

death_yearly_df <- ucdp_brd %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(mean_death = mean(bd_best),
                   total_death = sum(bd_best))
death_yearly_p <- death_yearly_df %>%
  ggplot() +
  geom_line(aes(x = year, y = total_death)) +
  labs(y = "total death", title = "Total of Battles Deaths per year")
death_yearly_p
ggsave(paste0(github_path, "/output/images/death_yearly.png"))

export_yearly_df <- comtrade %>%
  filter(trade_flow == "Export", partner == "World")
export_yearly_df <- aggregate(export_yearly_df$trade_value_usd, list(export_yearly_df$year), FUN=sum)
export_yearly_p <- export_yearly_df %>%
  ggplot() +
  geom_line(aes(x = Group.1, y = log(x))) +
  labs(x = "year", y = "log export (USD)", title = "log export of SALW")
export_yearly_p
ggsave(paste0(github_path, "/output/images/export_yearly.png"))

import_share_p <- weapon_export %>%
  filter(!comtrade_country %in% c("World", "Special Categories", "Areas, nes", "Bunkers")) %>%
  group_by(year) %>%
  mutate(world_import = sum(total_import)) %>% 
  ungroup() %>%
  mutate(import_share = total_import/world_import) %>%
  group_by(year) %>%
  arrange(desc(import_share)) %>%
  slice(1:5) %>%
  ggplot(aes(x = year, y = import_share, group = comtrade_country, colour = comtrade_country)) +
  geom_line()
import_share_p

export_share_p <- weapon_from %>%
  filter(!country %in% c("World", "Special Categories", "Areas, nes", "Bunkers")) %>%
  group_by(year) %>%
  mutate(world_export = sum(total_export)) %>% 
  ungroup() %>%
  mutate(export_share = total_export/world_export) %>%
  group_by(year) %>%
  arrange(desc(export_share)) %>%
  slice(1:3) %>%
  ggplot(aes(x = year, y = export_share, group = country, colour = country)) +
  geom_line()
export_share_p
ggsave(paste0(github_path, "/output/images/weapon_export_share.png"))
