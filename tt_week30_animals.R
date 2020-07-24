# load packages ----
library(tidyverse)
library(tidytuesdayR)
library(magrittr)
library(sf)
library(ozmaps)
library(janitor)
library(wesanderson)
library(ggrepel)
library(ggtext)

setwd("C:/Users/Shona/Google Drive/PhD/R Code/TidyTuesday")

# LOAD ----
tt_data <- tt_load_gh(last_tuesday()) %>% 
  tt_download() 

df_outcomes <- tt_data %>% 
  extract2(1)

# MAP ----
st_aus <- ozmap_states %>% 
  rename(name = NAME)

# find centroids
st_centroids <- st_aus %>% 
  cbind(st_centroid(st_aus$geometry) %>% st_coordinates())

# create look-up table of states
df_states <- tribble(
  ~name, ~code,
  "New South Wales", "NSW",
  "Victoria", "VIC",
  "Queensland", "QLD",
  "South Australia", "SA",
  "Western Australia", "WA",
  "Tasmania", "TAS",
  "Northern Territory", "NT",
  "Australian Capital Territory", "ACT",
  "Other Territories", "Other"
)

# CLEAN ----
df_outcomes_long <- df_outcomes %>% 
  mutate(across(everything(), str_to_lower),
         across(ACT:Total, as.numeric)) %>% 
  filter(animal_type == "dogs",
         year == 2018) %>% 
  select(year, ACT:WA) %>% 
  pivot_longer(cols = c(ACT:WA),
               names_to = "code",
               values_to = "value")

# summarise to find number of dogs in each state in 2018
df_total <- df_outcomes_long %>% 
  group_by(code) %>% 
  summarise(total = sum(value)) 

# join 
df_total_states <- df_total %>% 
  left_join(df_states,
            by = "code") %>% 
  select(name, code, total) %>% 
  arrange(desc(total))

# join to spatial data
st_aus_join <- st_centroids %>% 
  left_join(df_total_states,
            by = "name") %>% 
  mutate(animals_per_m2 = total/area_m2) %>% 
  filter(!is.na(total))


# PLOT ----

# create colour palette
pal <- wes_palette(name = "Zissou1",
                   n = 21,
                   type = "continuous")
# build plot
st_aus_join %>% 
  ggplot() +
  geom_sf(
    col = "white",
    fill = "gray20",
    size = 1
  ) +
  geom_label_repel(
    aes(X, Y,
        label = name,
        colour = total),
    segment.colour = "gray20",
    box.padding = 2.8
  ) +
  geom_text(
    aes(X, Y, 
        label = "\U0001f436", 
        size = total,
        col = total)
  ) +
  scale_color_gradientn(colours = pal,
                        breaks = seq(0, 12000, 3000),
                        name = "Total no. of dogs") +
  scale_size(range = c(15, 30)) +
  guides(
    size = F,
    col = guide_colorbar(direction = "vertical",
                         barwidth = 1,
                         barheight = 30)
  ) +
  labs(
    caption = "#TidyTuesday | @shonawilde | Source: Brisbane Open Data - Animal Complaints",
    title = '"How many dogs were in each Australian state in 2018?"',
    subtitle = "<b style='color:red'>Queensland</b> has over <b style='color:red'>11,000</b> dogs!"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown(),
    plot.caption = element_text(margin = margin(b = 10))
  )


# SAVE ----
ggsave("plots/tt_week30_animals.png",
       height = 12,
       width = 14)

