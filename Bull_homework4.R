#load in necessary libraries
library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)
library(stargazer)

#grab the data and name the dataset
datExtract <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework4/HW3_extract_results.csv")

#add columns, which are the differences of P extracted in each field type, the total differences, and the ratio of the two
datExtract$Diff <- datExtract$Pasture - datExtract$Wheat 
datExtract$totalDiff <- sum(datExtract$Pasture - datExtract$Wheat)
datExtract$totalPDiffRatio <- (datExtract$Diff / datExtract$totalDiff)*100

#create the plot showing the ratio differences
Figure <- datExtract %>% ggplot(aes(x = totalPDiffRatio, y = Method)) + geom_point(size = 1) + 
  labs(x = "Total P Difference Ratio", y = "Method of Extraction") +
  geom_col(width = 0.1) + 
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  theme_bw() + facet_wrap(~Pool, nrow = 3)


#save out the plot
ggsave("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework4/pasturePlot.png", Figure, width = 6.5, height = 4.5)
