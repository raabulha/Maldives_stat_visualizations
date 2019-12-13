library(tidyverse)
library(gganimate)

islpop <- read.csv("Data/pop_proj_islands.csv", stringsAsFactors = FALSE)

dat <- islpop %>% 
  filter(isl_type == "admin") %>% 
  gather(key, pop, 4:85) %>% 
  mutate(year = as.integer(substr(key, 2,5)),
         sex = substr(key, 6, length(key))) %>% 
  group_by(atoll, island, year) %>% 
  summarize(totp = sum(pop))

dat$atoll <- factor(dat$atoll,
                    levels = c("HA","HDh","Sh",
                               "N", "R","B","Lh",
                               "K","AA","ADh",
                               "V","M","F","Dh",
                               "Th","L","GA",
                               "GDh","Gn","S"))

atoll_plot <- dat %>% ggplot(aes(year, totp/1000, fill = totp / 1000, color = totp / 1000)) + 
  geom_point(shape = 21, size = 2) + 
  theme_bw() + 
  facet_wrap(~atoll, scales = "free_y", ncol = 4) + 
  scale_fill_gradientn(name = "Population (in thousands)", 
                       colours = c("green", "cyan4", "blue")) + 
  scale_color_gradientn(name = "Population (in thousands)", 
                       colours = c("green", "cyan4", "blue")) + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 11, color = "black"), 
        strip.text = element_text(size = 12, face = "bold", color = "white"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "#03919B"))

anim <- atoll_plot +   transition_time(year) + 
  shadow_mark(alpha = 0.5, size = 0.5) + 
  labs(title = "Population projection of atolls by islands, Maldives (2014-2054)",
       subtitle = "Year: {frame_time}",
       x = NULL, 
       y = "Population (in thousands)\n",
       caption = "Data source: National Bureau of Statistics, Ministry of National Planning and Infrastructure,\n\n visualization: @raabulha (twitter)") 

animate(anim, width = 800, height = 1000)

anim_save("atoll_pop_project.gif")
