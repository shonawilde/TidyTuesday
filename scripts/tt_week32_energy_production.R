# load packages ----
library(tidyverse)
library(tidytuesdayR)
library(magrittr)
library(sf)
library(rnaturalearth)
library(ggtext)

setwd("C:/Users/Shona/Google Drive/PhD/R Code/TidyTuesday")

# LOAD ----
tt_data <- tt_load_gh(last_tuesday()) %>% 
  tt_download() 

df_energy <- tt_data %>% 
  extract2(1)

# view 
glimpse(df_energy)

# check
df_energy %>% 
  distinct(type)

# CLEAN --- 
df_energy_tidy <- df_energy %>% 
  pivot_longer(cols = c(`2016`:`2018`),
                        names_to = "year",
                        values_to = "value") %>% 
  mutate(type = case_when(
    type == "Conventional thermal" ~ "Fossil fuels",
    type %in% c("Hydro",
                "Pumped hydro power",
                "Wind",
                "Solar",
                "Geothermal") ~ "Renewable",
    TRUE ~ type
  ))


# is Europe getting greener? ----

# define custom function
percent_change <- function (previous, new) {
  
  x <- ((new - previous)/previous) * 100
  
  return(x)
  
}

# find percentage change between 2016 and 2018
df_renewable <- df_energy_tidy %>% 
  filter(type == "Renewable") %>% 
  group_by(year, country, country_name) %>% 
  summarise(total = sum(value, na.rm = T)) %>% 
  pivot_wider(names_from = "year",
              values_from = "total",
              names_prefix = "total_") %>% 
  mutate(change = percent_change(total_2016, total_2018))

df_renewable %>% 
  arrange(change %>% desc())


# MAP ----
st_europe <- ne_countries(continent = "Europe",
                          scale = "medium",
                          returnclass = "sf") %>% 
  select(name, geometry)

# find mis-matches between country names - no Cyprus, Georgia or Turkey on map
df_no_match <- df_renewable %>% 
  anti_join(st_europe,
             by = c("country_name" = "name"))

df_no_match %>% 
  distinct(country_name)

# fix mis-matches
df_renewable_clean <- df_renewable %>% 
  mutate(country_name = case_when(
    country_name ==  "Bosnia & Herzegovina" ~ "Bosnia and Herz.",
    country_name == "North Macedonia" ~ "Macedonia",
    country_name == "Czechia" ~ "Czech Rep.",
    country == "UK" ~ "United Kingdom",
    country == "EL" ~ "Greece",
    TRUE ~ country_name
  ))

# join
st_renewable <- df_renewable_clean %>% 
  inner_join(st_europe,
             by = c("country_name" = "name")) %>% 
  ungroup() %>%  
  st_as_sf()
  

# PLOT ----
st_renewable %>% 
  ggplot() +
  geom_sf(aes(fill = change),
          size = 0.75,
          col = "white") +
  coord_sf() +
  xlim(c(-10, 40)) +
  ylim(c(35, 72)) +
  scale_fill_gradient2(low = "mediumvioletred",
                       mid = "gray55",
                       high = "lightseagreen",
                       breaks = c(-100, -50, 0, 25, 50),
                       labels = function(x) paste0(x, "%")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12.5),
        legend.title = element_blank(),
        plot.title = element_text(size = 30,
                                  face = "bold",
                                  margin = margin(t = 10)),
        plot.subtitle = element_markdown(size = 15),
        plot.caption = element_markdown(color = "gray30",
                                        size = 10,
                                        margin = margin(t = 20,
                                                        b = 20))) +
  guides(fill = guide_colorbar(barwidth = 25)) +
  labs(title = "Is Europe getting greener?",
       subtitle = "Percentage <b style='color:lightseagreen'>increase</b> or <b style='color:mediumvioletred'>decrease</b> of energy generated from <br> <b >renewables</b> between 2016 - 2018",
       caption = "#TidyTuesday | @shonawilde | Source: Energy Generation Statistics")

# SAVE ----
ggsave("plots/tt_week32_energy.png",
       height = 10,
       width = 10)

