#load in necessary libraries
library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)
library(stargazer)
library(cowplot)

#read in and order the data
datRelease <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/final/Bohara_etal_data-1.csv")
datRelease <- datRelease %>% mutate(Tree.species = factor(Tree.species, levels = Tree.species, ordered = TRUE))

#pivot the data to show the desired variables in a workable order
datRelease <- datRelease %>% mutate(CN = C / N)
datRelease <- datRelease %>% mutate(CP = C / P)
datRelease <- datRelease %>% mutate(NP = N / P)
datRelease2 <- pivot_longer(data = datRelease, cols = elevation, names_to = 'elevation', values_to = 'meters')
datRelease3 <- pivot_longer(data = datRelease2, cols = c(C, N, P), names_to = 'startCon', values_to = 'percElem')
datRelease4 <- pivot_longer(data = datRelease3, cols = c(C.std..err, N.std..Err, P.std..Err), names_to = 'startConErr', values_to = 'percElemErr')
datRelease5 <- pivot_longer(data = datRelease4, cols = c(C.Release, N.Release, P.Release), names_to = 'conRel', values_to = 'percElemRel')
datRelease6 <- pivot_longer(data = datRelease5, cols = c(C.rel.std..err, N.rel.std..err, P.rel.std..err), names_to = 'RelErr', values_to = 'percRelErr')

#get rid of repeat rows in the data
datPlot1 <- datRelease4 %>% mutate(startConErr = gsub(".std..err|.std..Err", "", startConErr)) %>% 
  filter(startCon == startConErr)

#create a column for the initial stoichiometry
datRatio <- pivot_longer(data = datPlot1B, cols = c(CN, CP, NP), names_to = 'ratios', values_to = 'ratioValues') 

#find error data
datRatio %>% filter(startCon == startConErr & startCon == conRel & startCon == RelErr)
# standard error = standard deviation / sqrt(n), n = 3
# standard deviation of x = sd(X)
# standard deviation of a*x = abs(a)*sd(x) so as long as we multiple our sd by what ever we multiply our 
# starting values by then it will hold

#repeat the above steps to get the final concentration values in the order we need them
datPlot1B <- datRelease6 %>% mutate(startConErr = gsub(".std..err|.std..Err", "", startConErr), 
                                    conRel = gsub(".Release", "", conRel), 
                                    RelErr = gsub(".rel.std..err", "", RelErr)) %>% 
  filter(startCon == startConErr & startCon == conRel & startCon == RelErr) %>%
  mutate(percLeft = percElem * (1 - (percElemRel/100)))

datPlot1C <- datPlot1B %>% select(Tree.species, meters, startCon, percElem, percElemErr, endPerc, se_endPerc) %>% 
  pivot_longer(cols = c("percElem", "endPerc"), 
               names_to = "time_mean_elementPercent", values_to = "mean_elementPercent") %>% 
  pivot_longer(cols = c("percElemErr", "se_endPerc")) 

datPlot1D <- datPlot1C %>% mutate(time_mean_elementPercent = ifelse(time_mean_elementPercent == "percElem", "initial", "end"), 
                                  name = ifelse(name == "percElemErr", "initial", "end")) %>%
  filter(time_mean_elementPercent == name)

#create the four separate graphs to compare 
ratioK <- datRatio %>%
  ggplot(aes(x = ratioValues, y = K, color = ratios, shape = Tree.species)) + 
  geom_point() +
  scale_shape_manual(values = 1:nlevels(datRatio$Tree.species)) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 15)) +
  labs(x = "Chemcial Ratios", y = "Decay Rate (K)")

ratioLitter <- datRatio %>%  ggplot(aes(x = ratioValues, y = Litter.Mass.Loss, color = ratios, shape = Tree.species)) + 
  geom_point() +
  scale_shape_manual(values = 1:nlevels(datRatio$Tree.species)) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 15)) +
  labs(x = "Chemical Ratios", y = "Litter Mass Loss %", shape = "Tree Species", color = "Chemical Ratios")

ratioRemain <- datRatio %>%  ggplot(aes(x = meters, y = percLeft, color = RelErr, shape = Tree.species)) + 
  geom_point() +
  scale_shape_manual(values = 1:nlevels(datRatio$Tree.species)) +
  theme_minimal() +
  scale_color_manual(values = c('C' = 'darkgreen',
                                'N' = 'blue',
                                'P' = 'red')) + 
  geom_point()+
  theme(legend.position = "none", text = element_text(size = 15)) +
  #facet_wrap(~conRel, ncol = 1, scales = "free") +
  labs(x = "Elevation (m)", y = "Remaining Concentration")

elevK <- datRatio %>%  ggplot(aes(x = meters, y = K, shape = Tree.species)) + 
  geom_point() +
  scale_shape_manual(values = 1:nlevels(datRatio$Tree.species)) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 15)) +
  labs(x = "Elevation (m)", y = "K", shape = "Tree Species")

#plot all 4 graphs together
plotAll <- plot_grid(ratioK, ratioLitter, ratioRemain, elevK, nrow = 2, labels = "AUTO")

# plot just to get the legend
legend  <- datRatio %>%  ggplot(aes(x = ratioValues, y = Litter.Mass.Loss, color = ratios, shape = Tree.species)) + 
  geom_point() +
  scale_shape_manual(values = 1:nlevels(datRatio$Tree.species)) +
  theme_minimal() +
  theme(text = element_text(size = 15)) + 
  labs(x = "Chemical Ratios", y = "Litter Mass Loss %", shape = "Tree Species", color = "Chemical Ratios")

#plot the graphs in a square with the legend attached
plot_grid(plotAll, get_legend(legend), rel_widths = c(1,0.4), nrow = 1)
