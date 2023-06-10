#Script to data analyses 
#Structural patterns in plant-pollinator networks

#Load packages
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(car)
library(lme4)
library(MASS)
library(stargazer)
library(ggeffects)
library(effects)
library(sjPlot)
library(jtools)
library(fitdistrplus)
library(modelsummary)


#Read data
data <- read.csv("results_networks.csv", header = TRUE, sep = ";", dec = ",")
summary(data)
head(data)

# Exploring the data
# Number of levels in each variable
count_df <- data %>%
  summarise_all(~list(length(unique(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count")

count_df

#count values for categorical variables by each level
data_categoric <- data.frame(Region = data$Region, Hemisphere = data$Hemisphere, 
                             Continent = data$Continent, Biome = data$Biome_WWF, 
                             Realm = data$Realm_WWF)

count_df <- data_categoric %>%
  gather(key = "Variable", value = "Value") %>%
  count(Variable, Value, name = "Count") %>%
  arrange(Variable, Value)

count_df

#boxplot
p <- ggplot(data, aes(x=Hemisphere, y=Size)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")

#boxplot
p <- ggplot(data, aes(x=Region, y=Size)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")

#boxplot
p <- ggplot(data, aes(x=Realm_WWF, y=Connectance)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")

#boxplot
p <- ggplot(data, aes(x=Biome_WWF, y=Connectance)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")


#Building models to test
#Testing assumptions
# Plot correlation matrix
pairs(~ Size+Connections+Connectance+NODF+Asymmetry+M_Beckett+Animals+Plants, data = data)
hist(data$Connectance)
hist(data$NODF)
hist(data$M_Beckett)
boxplot(Plants ~ Region, data = data)
boxplot(Animals ~ Region, data = data)
boxplot(Connectance ~ Region, data = data)
boxplot(Asymmetry ~ Region, data = data)

#a big outlier come from the Robertson (1929) network
data <- data %>% filter(Web_Code != "robertson_1929")

#the Flooded grasslandd and savanas biome just has one network
data <- data %>% filter(Biome_WWF != "Flooded grasslands and savannas")

################################################################
#First need to relevel the reference group according to > networks number
data$Realm_WWF <- as.factor(data$Realm_WWF)
data$Realm_WWF = relevel(data$Realm_WWF, ref=5)
data$Biome_WWF <- as.factor(data$Biome_WWF)
data$Biome_WWF = relevel(data$Biome_WWF, ref=6)
data$Continent <- as.factor(data$Continent)
data$Continent = relevel(data$Continent, ref=4)

###################### Connectance
#Model to test the Connectance that has non-normal distribution
fitdistr(data$Connectance, "lognormal")
model1 <- lmer(log(Connectance)~ 1 + (1 | Size) + (1 | Asymmetry), data = data)
model2 <- lmer(log(Connectance)~ Hemisphere + Continent + Region + Realm_WWF + Biome_WWF + (1 | Size) + (1 | Asymmetry), data = data)

plot(model2)
summary(model2)
anova(model1, model2)

summ(model2)
sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)
sjPlot::tab_model(model2)

##################### NODF
fitdistr(data$NODF, "normal")
model1 <- lmer(NODF ~ 1 + (1 | Size) + (1 | Asymmetry), data = data)
model2 <- lmer(NODF ~ Hemisphere + Continent + Region + Realm_WWF + Biome_WWF + (1 | Size) + (1 | Asymmetry), data = data)

plot(model2)
summary(model2)
anova(model1,model2)

summ(model2)
sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)
sjPlot::tab_model(model2)

##################### Modularity
fitdistr(data$M_Beckett, "normal")
model1 <- lmer(M_Beckett ~ 1 + (1 | Size) + (1 | Asymmetry), data = data)
model2 <- lmer(M_Beckett ~ Hemisphere + Continent + Region + Realm_WWF + Biome_WWF + (1 | Size) + (1 | Asymmetry), data = data)

plot(model2)
summary(model2)
anova(model1,model2)

sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)

summary(model2)$coefficients[, "Estimate"]
summary(model2)$coefficients[, "Std. Error"]
##############
#for taxonomic levels
fitdistr(data$M_Beckett, "normal")
# Fit linear mixed-effects model with NA values dropped for "Animal_taxonomic_level"
model1 <- lmer(M_Beckett ~ 1 + (1 | Size) + (1 | Asymmetry),
               data = na.omit(data[, c("M_Beckett", "Animal_taxonomic_level", "Size", "Asymmetry")]))
model2 <- lmer(M_Beckett ~ Animal_taxonomic_level + (1 | Size) + (1 | Asymmetry),
               data = na.omit(data[, c("M_Beckett", "Animal_taxonomic_level", "Size", "Asymmetry")]))


sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)

######################################
modelplot(model2)

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black"))
