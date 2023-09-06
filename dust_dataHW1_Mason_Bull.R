rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggtext)

#load in the original dust data we were provided
dust_dataHW1 <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/dust_dataHW1.csv", header=TRUE)
View(dust_dataHW1)

#assuming that water has a density of 1kg/L, (1,000,000mg/L) the dimensional analysis of converting concentrations to flux is 
# [P or Ca]mg/L * 1L/1,000,000mg * flux g/m^2/yr = flux[P or Ca]g/m^2/yr
#I performed these calculations and created a new column with the created data
dust_dataHW1$P_flux <- dust_dataHW1$P_conc/1000000*dust_dataHW1$flux_gm2yr
dust_dataHW1$Ca_flux <- dust_dataHW1$Ca_conc/1000000*dust_dataHW1$flux_gm2yr

#pivot the data to create a dataframe for calling individual instances for plotting later
datDustLong <- dust_dataHW1 %>% pivot_longer(cols = P_flux:Ca_flux, names_to= "Element_fluxes", values_to = "flux_value")

#summarize the data to show the data type and name of each factor in the dataframe
summary(as.factor(datDustLong$Ecosystem))

#change Ecosystem into a factor to allow us to order in the way we want, in this case elevation. From chr to factor
datDustLong$Ecosystem <- factor(x = datDustLong$Ecosystem, levels = c("Plains", "Foothills", "Montane", "Subalpine", "Alpine"), ordered = T)

#plot the data using ggplot
caption <- "**Fig. 1** This figure shows that there is generally more calcium available in each ecosystem during all seasons.<br>Calcium shows a major drop off between the plains and the foothills during the July August September<br>months (JAS), but a much gentler decline between the plains and foothills followed by a steep decline into the<br>montane in the spring May and June months (MJ). Phosphorus flux has a much less extreme shift in flux across the<br>elevations, with overall slight declines between the plains and alpine in the summer months.However, there is an extreme<br>uptick in phosphorus flux between the plains and foothills in the spring, which then drops down aggresively into the montane.<br>These value fluxes are likely attributed to spring and early summer storms transporting the available phosphorus and calcium downslope,<br>thus increasing concentrations in the lower ecosystems.</span>"
datDustLong %>% ggplot(aes(y=flux_value, x=Ecosystem, color = Season, group=interaction(Season, Element_fluxes)))+
  geom_point(aes(shape = Season))+geom_line(aes(linetype = Element_fluxes))+
  labs(y="Flux"~(g/m^2/yr), linetype = "Element", title = "Element Flux by Ecosystem per Season",
       caption = caption) +
  theme_bw()+theme(plot.caption = element_markdown(hjust = 0.5))
  scale_linetype_discrete(labels=c("Ca Flux", "P Flux"))

#save the plot out as a pdf
ggsave(filename= "C:\\Users\\mason\\OneDrive\\Desktop\\BSU\\Classes\\BioGeoChem\\Homework1\\homework1plot.pdf")