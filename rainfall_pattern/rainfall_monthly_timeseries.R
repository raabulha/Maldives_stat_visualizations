library(tidyverse)

rdat <- read.csv("rainfall_agg.csv", stringsAsFactors = FALSE)

rdat <- rdat %>% 
  gather(month, rval, 4:15)

rdat$month <- factor(rdat$month, levels = c("Jan","Feb","Mar","Apr","May",
                                            "Jun","Jul","Aug","Sep","Oct",
                                            "Nov","Dec"))

rdat$Location <- factor(rdat$Location, levels = c("Hanimaadhoo","Male","Kadhdhoo",
                                                  "Kaadedhdhoo","Gan"))


c.pal <- c("#ffffc0","#f0f060","cyan4","blue2","blue4")

rdat %>% 
  ggplot(aes(Year, month, fill=rval/10)) + 
  geom_tile(color = "white") + 
  labs(x = "\nYear", y = "Month\n") + 
  scale_fill_gradientn(name = "Rainfall \n(in centimeters)", 
                       colours = c.pal) +
  scale_x_continuous(breaks = seq(1975,2018, by = 5)) + 
  facet_wrap(~Location, scales = "free", ncol = 2) + 
  coord_cartesian(expand = c(0,0)) + theme_bw() +
  theme(axis.text = element_text(size = 9, color = "black"),
        axis.line = element_line(size = 0.5))
