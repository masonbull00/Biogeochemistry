
rm(list=ls())

library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)


datInjection <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework5/Reach1_N_blank.csv")

ratio <- 0.0543 * 1000

datInjection <- datInjection %>% mutate(BackgroundRatio = (Background.N.NO3.mg.L / Background.Cl.mg.L)*1000)

datInjection <- datInjection %>% mutate(Time_collected = ifelse(grepl("^9:", Time_collected), paste0("0", Time_collected), Time_collected)) %>%
  pivot_longer(cols = starts_with("Background"))  %>%
  mutate(name = case_when(grepl("Cl", name)~ "Cl",
                          grepl("NO3", name)~ "NO3",
                          grepl("Ratio", name)~ "Ratio"))

ConcentrationCurve <- datInjection  %>% ggplot(aes(x = Time_collected, y = value, color = name)) + 
  theme_bw() + 
  geom_hline(yintercept = ratio, color = "green") +
  annotate("rect", xmin = -Inf, xmax = "09:35:00", ymax = Inf, ymin = -Inf, fill = "#807979", alpha = 0.35) +
  annotate("rect", xmin = Inf, xmax = "10:53:00", ymax = Inf, ymin = -Inf, fill = "#807979", alpha = 0.35) +
  geom_point() +
  labs(color = "Tracer Element", x = "Time of Collection", y = "Concentration of Tracer (mg/L)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.8, size = 10)) +
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework5/ConcentrationCurve.png", ConcentrationCurve, width = 8.1, height = 4.1)



## Part 3 ##

datNitrate <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework5/IndianCr_data.csv")

NitrateDischarge <- datNitrate %>% ggplot(aes(x = Q, y = Nitrate)) + geom_point() + 
  labs(x = "Discharge (cfs)" , y = "[NO2 + NO3] (mg/L)") +
  theme_bw()

ggsave("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework5/NitrateDischarge.png", NitrateDischarge)
