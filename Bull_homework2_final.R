## code for homework 2 for Biogeochemistry
# import necessary libraries
library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)
library(stargazer)

#read in data and give a named variable
datNPP <- read.csv("C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/Homework2/Ecosystem_NPP.csv")

#sort dataframe by NPP values
datNPP <- datNPP %>% arrange(desc(MeanNPPobs_gCm.2yr.1))

#add a new column that is numbered vegetation types
datNPP$numVeg <- 1:nrow(datNPP)

#summarize a linear regression model to establish significance for each predictor variable in the dataframe
summary(lm(MeanNPPobs_gCm.2yr.1 ~ as.numeric(Vegetation), data = datNPP))

summary(lm(MeanNPPobs_gCm.2yr.1 ~ as.numeric(numVeg), data = datNPP))

summary(lm(MeanNPPobs_gCm.2yr.1 ~ Precipitation_cm, data = datNPP))

summary(lm(MeanNPPobs_gCm.2yr.1 ~ MAT_C, data = datNPP))

summary(lm(MeanNPPobs_gCm.2yr.1 ~ C.N, data = datNPP))

#summarize data from multiple linear regression to compare MLRs with different data
summary(lm(MeanNPPobs_gCm.2yr.1 ~ Vegetation + Precipitation_cm, data = datNPP))

summary(lm(MeanNPPobs_gCm.2yr.1 ~ MAT_C + C.N + Precipitation_cm + numVeg, data = datNPP))

#create named variables from each MLR and summarize the outputs in table form to determine what is and is not significant
full  <- lm(MeanNPPobs_gCm.2yr.1 ~ MAT_C + C.N + Precipitation_cm, data = datNPP)
redu1 <- lm(MeanNPPobs_gCm.2yr.1 ~ C.N + Precipitation_cm, data = datNPP)
redu2 <- lm(MeanNPPobs_gCm.2yr.1 ~ MAT_C + C.N, data = datNPP)
lmtest::lrtest(redu1, full)
lmtest::lrtest(redu2, full)

summary(full)
summary(datNPP)
summary(as.factor(datNPP$Vegetation))

#export the best MLR table to an HTML table 
fullSum <- list(full)
stargazer(fullSum, type = 'html', out = "C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/Homework2/fullSum.html")

caption <- "This plot of NPP vs Preciptation shows that there is a strong corellation between the two. Precipitation is the strongest predicter of NPP values relative\nto the others in the above table, with the only P value below 0.05. The non-adjusted R^2 value also shows a strong correlation between the two.\nWhile we must consider that the error with so few observations in a linear regression is high, the precipitation correlation to mean NPP is so\nmuch higher than the other than the other variables it is safe to assume that it is the primary prediction class."

#create a ggplot of the best NPP predictor (precipitation) versus NPP
datNPP %>% ggplot(aes(y = MeanNPPobs_gCm.2yr.1, x = Precipitation_cm)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Precipitation (cm)", y = "Mean NPP (gC/m2/yr)", caption = caption) +
  theme(plot.caption = element_markdown(hjust = 0.5)) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  theme_bw()
ggsave(filename = "C:/Users/mason/OneDrive/Desktop/BSU/Classes/BioGeoChem/Homework2/homework2plot.pdf")