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

# Set directory
setwd("D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 3 - Structural pattern/results")

# Read data
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
hist(log(data$Connectance))

# Transforming LAT and LONG on numeric variables
data$LAT <- as.numeric(data$LAT)
data$LONG <- as.numeric(data$LONG)
summary(data)
sqrt(data$LAT*data$LAT)

# Transforming Latitude on positive values
data <- data %>%
  mutate(LAT_pos = sqrt(LAT*LAT))

data$LAT_pos
summary(data)

ggplot(data, aes(x="", y=LAT_pos)) + 
  geom_boxplot()

# Model to test
model1 <- lm(log(Connectance)~ 1, data = data)
model2 <- lm(log(Connectance)~ Realm_WWF + Biome_WWF + log(Size) + log(LAT_pos), data = data)

plot(model2)
summary(model2)
anova(model1, model2)

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black"))+
  ggtitle("Connectance")

summ(model2)
sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)
sjPlot::tab_model(model2)

##################### NODF
scale(data$Size)
log(data$Size)
fitdistr(data$NODF, "normal")
model1 <- lm(NODF ~ 1, data = data)
model2 <- lm(NODF ~ Realm_WWF + Biome_WWF + log(Size) + log(LAT_pos), data = data)

plot(model2)
summary(model2)
anova(model1,model2)

summ(model2)
sjPlot::plot_model(model3, show.values=TRUE, show.p=TRUE)
sjPlot::tab_model(model2)

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black")) +
  ggtitle("NODF")

##################### Modularity
fitdistr(data$M_Beckett, "normal")
model1 <- lm(M_Beckett ~ 1, data = data)
model2 <- lm(M_Beckett ~ Realm_WWF + Biome_WWF + log(Size) + log(LAT_pos), data = data)

plot(model2)
summary(model2)
anova(model1,model2)

sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black")) +
  ggtitle("Modularity")

summary(model2)$coefficients[, "Estimate"]
summary(model2)$coefficients[, "Std. Error"]

##############
# TAXONOMIC ANALYSIS
#############
# Ordering variables levels
data <- na.omit(data)
data <- data %>%
  mutate(Plant_Numeric_level = ifelse(Plant_taxonomic_level == "NER", 2,
                                      ifelse(Plant_taxonomic_level == "family", 1, Plant_taxonomic_level)))

data <- data %>%
  mutate(Animal_Numeric_level = ifelse(Animal_taxonomic_level == "NER", 7,
                                      ifelse(Animal_taxonomic_level == "phylum", 6, 
                                      ifelse(Animal_taxonomic_level == "class", 5,
                                      ifelse(Animal_taxonomic_level == "order", 4,
                                      ifelse(Animal_taxonomic_level == "superfamily", 3,
                                      ifelse(Animal_taxonomic_level == "family", 2,
                                      ifelse(Animal_taxonomic_level == "subfamily", 1,
                                      ifelse(Animal_taxonomic_level == "tribe", 1,
                                      ifelse(Animal_taxonomic_level == "genus", 1,
                                      Animal_taxonomic_level))))))))))

# Fit linear model with NA values dropped for some variables
# Connectance
model1 <- lm(Connectance ~ 1, data = na.omit(data[, c("Connectance", "Plant_Numeric_level", 
                                                      "Animal_Numeric_level", "Size", "LAT_pos")]))
model2 <- lm(Connectance ~ Plant_Numeric_level + Animal_Numeric_level + log(Plants) + log(Animals) + log(LAT_pos),
               data = na.omit(data[, c("Connectance", "Plant_Numeric_level", 
                                       "Animal_Numeric_level", "Plants", "Animals", "LAT_pos")]))
model3 <- lmer(Connectance ~ Plant_Numeric_level + Animal_Numeric_level + log(Plants) + log(Animals) + log(LAT_pos) + (1|Realm_WWF), data = data)

summary(model3)

summary(data)
#package nlme
data$Animal_Numeric_level <- as.numeric(data$Animal_Numeric_level)
data$Plant_Numeric_level <- as.numeric(data$Plant_Numeric_level)
summary(model2)

sjPlot::plot_model(model2, show.values=TRUE, show.p=TRUE)

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black")) +
  ggtitle("Connectance")

# NODF
model2 <- lm(NODF ~ Plant_Numeric_level + Animal_Numeric_level + log(Size) + log(LAT_pos),
             data = na.omit(data[, c("NODF", "Plant_Numeric_level", 
                                     "Animal_Numeric_level", "Size", "LAT_pos")]))

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black")) +
  ggtitle("NODF")

# Modularity 
model2 <- lm(M_Beckett ~ Plant_Numeric_level + Animal_Numeric_level + log(Size) + log(LAT_pos),
             data = na.omit(data[, c("M_Beckett", "Plant_Numeric_level", 
                                     "Animal_Numeric_level", "Size", "LAT_pos")]))

modelplot(model2, coef_rename = TRUE, coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black")) +
  ggtitle("Modularity")

######################################
#save final data in R file
save(data, file = "path_data.RData")

