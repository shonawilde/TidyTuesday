library(tidyverse)
library(tidytuesdayR)
library(shonarrr)


# read data
df_astronauts <- tt_load_gh(last_tuesday()) %>% 
  tt_download() %>% 
  `[[`(1)

df_astronauts %>% 
  group_by(nationality) %>% 
  filter(n() > 5) %>% 
  summarise(avg_hours = mean(eva_hrs_mission)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(nationality, avg_hours), y = avg_hours)) +
  coord_polar()

# plot ---- 
df_astronauts %>% 
  group_by(year_of_mission) %>% 
  summarise(avg_hours = mean(hours_mission)) %>% 
  ggplot() +
  geom_col(aes(year_of_mission, avg_hours, fill = avg_hours)) +
  coord_polar(theta = "y") +
  scale_fill_gradient2(low = "firebrick",
                       mid = "darkorchid",
                       high = "cadetblue1",
                       midpoint = 796,
                       labels = scales::comma,
                       breaks = c(0, 1250, 2500, 3500, 4500, 5500
                                  )) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.grid = element_line(size = 0.02, colour = "gray90"),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(colour = "white", family = "Roboto Condensed"),
    axis.text.y = element_text(colour = "white"),
    legend.title = element_text(colour = "white", face = "bold"),
    plot.title = element_text(size = 15, face = "bold")
  ) +
  labs(
    y = "",
    x = "Year\n",
    title = "Avergage space mission duration by year"
  ) +
  guides(fill = guide_colorbar(title = "Average mission time (hours)",
                               barwidth = 20,
                               barheight = 0.5,
                               title.position = "top")
         
         )


# my version -----

glimpse(df_astronauts)

df_astronauts %>% 
  group_by(ascend_shuttle) %>% 
  #filter(n() > 6) %>% 
  distinct(ascend_shuttle) %>% 
  print_inf()
  
df_astronauts %>% 
  group_by(year_of_selection, sex) %>% 
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(year_of_selection, n, fill = sex),
           position = "dodge")

df_astronauts %>% 
  arrange(total_eva_hrs %>% desc()) %>% 
  distinct(name, .keep_all = T) %>% 
  slice(1:10)

# total eva hours by country year o bottom ggbumb

# find top 10 countries for total eva hours
total_eva_hours_country <- df_astronauts %>% 
  group_by(nationality) %>% 
  summarise(total_hours = sum(total_eva_hrs)) %>% 
  arrange(desc(total_hours)) %>% 
  slice(1:10)

top_n_countries <-
  total_eva_hours_country %>% 
  distinct(nationality) %>% 
  pull()

# get cumsum of hours by year
df_ranks <- df_astronauts %>% 
  filter(nationality %in% top_n_countries) %>% 
  select(year_of_mission, nationality, total_eva_hrs) %>% 
  group_by(nationality, year_of_mission) %>% 
  summarise(total_yearly_hours = sum(total_eva_hrs)) %>% 
  mutate(cum_hours = cumsum(total_yearly_hours)) %>% 
  group_by(year_of_mission, nationality) %>% 
  filter(year_of_mission == 1992) %>% 
  group_by(year_of_mission) %>% 
  arrange(desc(cum_hours)) %>% 
  mutate(rank =  rank(cum_hours, ties.method = "first"))

df_ranks %>% 
  ungroup() %>% 
  distinct(rank)

df_ranks %>% 
  print_inf()


df_ranks %>% 
  ggplot() +
  geom_col(aes(year_of_mission, cum_hours)) +
  facet_wrap(~nationality, scales = "free_y")





df_astronauts %>% 
  group_by(year_of_mission, nationality) %>% 
  mutate(sum_eva_hours = sum(total_eva_hrs)) %>% 
  ggplot() +
  geom_col(aes(year_of_mission, sum_eva_hours, fill = nationality))


data_year_rankings <- df_astronauts %>% 
  filter(nationality %in% top_n_countries) %>% 
  select(year_of_mission, 
         nationality, 
         total_eva_hrs) %>% 
  group_by(year_of_mission,
           nationality) %>% 
  summarise(total_eva_hrs = sum(total_eva_hrs, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(nationality,
          year_of_mission) %>% 
  group_by(nationality) %>% 
  mutate(total_eva_hrs_cumsum = cumsum(total_eva_hrs)) %>% 
  ungroup() %>% 
  arrange(year_of_mission,
          -total_eva_hrs_cumsum) %>% 
  group_by(year_of_mission) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup()

data_year_rankings %>% 
  filter(year_of_mission == 1970)

data_year_rankings %>% 
  ggplot(aes(year_of_mission, rank, color = nationality, group = nationality)) +
  geom_bump(smooth = 15, size = 2) +
  scale_y_reverse()

  



data_year_rankings %>% 
  group_by(year_of_mission) %>% 
  mutate(rank = rank(total_eva_hrs, ties.method = "first")) %>% 
  arrange(rank) %>% 
  print_all()
  