library(tidyverse)
library(gganimate)

mlpop <- read.csv("Data/pop_proj_male.csv", stringsAsFactors = FALSE)

dat <- mlpop %>% 
  filter(isl_type == "admin") %>% 
  gather(key, pop, 4:85) %>% 
  mutate(year = as.integer(substr(key, 2,5)),
         sex = substr(key, 6, length(key))) %>% 
  group_by(atoll, island, year) %>% 
  summarize(totp = sum(pop))

dat$island <- factor(dat$island)

ml_plot <- dat %>% ggplot(aes(year, totp/1000, fill = totp / 1000, color = totp / 1000)) + 
  geom_point(shape = 21, size = 2) + 
  theme_bw() + 
  facet_wrap(~island, scales = "free_y") + 
  scale_fill_gradientn(name = "Population (in thousands)", 
                       colours = c("green", "cyan4", "blue")) + 
  scale_color_gradientn(name = "Population (in thousands)", 
                        colours = c("green", "cyan4", "blue")) + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 11, color = "black"), 
        strip.text = element_text(size = 12, face = "bold", color = "white"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "#03919B")) 

anim <- ml_plot +
  transition_time(year) + 
  shadow_mark(alpha = 0.5, size = 0.5) + 
  labs(title = "Population projection of Male', Maldives (2014-2054)",
       subtitle = "Year: {frame_time}", 
       y = "Population (in thousands)\n",
       x = NULL,
       caption = "Data source: National Bureau of Statistics, Ministry of National Planning and Infrastructure,\n\n visualization: @raabulha (twitter)") 


animate(anim, width = 730, height = 530)

anim_save("Male_pop_project.gif")
