# load packages ----
library(tidyverse)
library(tidytuesdayR)
library(magrittr)
library(ggtext)
library(tvthemes)

setwd("C:/Users/Shona/Google Drive/PhD/R Code/TidyTuesday")

# LOAD ----
tt_data <- tt_load_gh(last_tuesday()) %>% 
  tt_download() 

df_avatar <- tt_data %>% 
  extract2(1)

# find top 20 highest rated episodes
top_20 <- df_avatar %>% 
  distinct(book, book_num, chapter, chapter_num, imdb_rating) %>% 
  arrange(desc(imdb_rating)) %>% 
  slice(1:20)

top_20 %>% 
  count(book)

# PLOT ----

# import fonts
import_avatar()

# build plot

top_20 %>% 
  arrange(imdb_rating) %>% 
  mutate(chapter = factor(chapter, levels = chapter)) %>%
  ggplot() +
  geom_col(aes(imdb_rating, chapter, fill = book),
           col = "white",
           size = 1) +
  theme_avatar(text.font = "Slayer") +
  scale_fill_avatar() +
  coord_cartesian(expand = F) +
  theme(plot.title = element_markdown(size = 15,
                                      hjust = 0),
        plot.subtitle = element_markdown(hjust = 0),
        plot.caption = element_text(margin = margin(t = 10)),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11,
                                    margin = margin(r = 10)),
        axis.title.x = element_text(size = 11,
                                    margin = margin(t = 10))) +
  labs(title = "Highest rated chapters in <br> <b >Avatar: The Last Airbender</b>",
       subtitle = "10 out of the top 20 chapters were from <b style='color:firebrick'>fire</b> ",
       caption = "#TidyTuesday | @shonawilde | Source: (appa) https://github.com/averyrobbins1/appa")

# SAVE ----
ggsave("plots/tt_week33_avatar.png",
       height = 8,
       width = 12)

  