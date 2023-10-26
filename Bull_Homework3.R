##import necessary libraries
library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)
library(cowplot)

##import the dataset
datIncubation <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework3/incubation_data_HW3.csv")
##change the cycle column to a factor to make it discrete values
datIncubation$Cycle <- as.factor(datIncubation$Cycle) 
#get all of the values (except DOC) into the same concentration units
datIncubation <- datIncubation %>% mutate(Sulfate_uM = Sulfate_mM*1000, 
                                          Error.1 = Error.1*1000)

#pivot the data to separate the error and concentration values to get single interacting variables for plotting  
datIncubation2 <- pivot_longer(data = datIncubation, cols = starts_with("Error"), names_to = "Error", values_to = "ErrorVal")
datIncubation3 <- pivot_longer(data = datIncubation2, cols = DOC_mgL.1:Sulfate_uM, names_to = "Element", values_to = "ConVal")

#filter data to a column where each error column corresponds only to its respective concentration
datFilter <- datIncubation3 %>% mutate(ErrElem = case_when(Error == "Error" ~ "DOC_mgL.1",
                                              Error == "Error.1" ~ "Sulfate_uM",
                                              Error == "Error.2" ~ "Fe_uM",
                                              Error == "Error.3" ~ "Mn_uM", TRUE ~ NA)) %>%
  filter(Element == ErrElem)

#order the data
datFilter$ElementOrder <- factor(datFilter$Element, levels = c("DOC_mgL.1", "Sulfate_uM", "Fe_uM", "Mn_uM"), ordered = TRUE)
datFilter <- datFilter %>% mutate(cycleOrder = case_when(State == "Anoxic" & Cycle == "0" ~ "a",
                                            State == "Oxic" & Cycle == "0" ~ "b",
                                            State == "Anoxic" & Cycle == "1" ~ "c",
                                            State == "Oxic" & Cycle == "1" ~ "d",
                                            State == "Anoxic" & Cycle == "2" ~ "e",
                                            State == "Oxic" & Cycle == "2" ~ "f",
                                            State == "Anoxic" & Cycle == "3" ~ "g",
                                            State == "Oxic" & Cycle == "3" ~ "h"))

#create plotting labels
elementLabs <- c("DOC (mg/L)", "Sulfate (uM)", "Fe (uM)", "Mn (uM)")
names(elementLabs) <- c(unique(datFilter$ElementOrder))

#create the plots of anoxic and oxic separately, as the values of each are so different so that its easier to visualize
Figure <- datFilter %>% ggplot(aes(x = cycleOrder, y = ConVal)) + 
  geom_point(aes(shape = State, color = State), size = 2.4) + 
  facet_wrap(~ElementOrder, scales = 'free_y', labeller = labeller(ElementOrder = elementLabs), strip.position = "left") +
  geom_errorbar(aes(ymin = ConVal - ErrorVal, ymax = ConVal + ErrorVal), width = 0.1) +
  scale_x_discrete(labels = c('0', '0', '1', '1', 
                              '2', '2', '3', '3')) +
  labs(x = ' ' , y = ' ') + theme_bw() + 
  theme(strip.placement = "outside")
  
#save out the image of the plot
ggsave(filename = "C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/homework3/incubation_data_HW3/Figure.png", height = 4.7, width = 8.5)


