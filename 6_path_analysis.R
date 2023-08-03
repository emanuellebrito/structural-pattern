# Load packages
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)
library(lavaanPlot)
library(AICcmodavg)
library(tidyverse)
library(ggplot2)


# Read the data
setwd("D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 3 - Structural pattern/data")
data <- read.csv("data_structural_pattern.csv", header = TRUE, sep = ";", dec = ",")

summary(data)
data$Residual_Connectance <- as.numeric(data$Residual_Connectance)
data$LAT_pos <- as.numeric(data$LAT_pos)

data_path <- data.frame(Region = data$Region, Hemisphere = data$Hemisphere, Size = data$Size, Connections = data$Connections, 
                        Connectance = data$Connectance, Asymmetry = data$Asymmetry,
                        NODF = data$NODF, Plants = log(data$Plants), Animals = log(data$Animals),
                        Modularity = data$M_Beckett, LAT_pos = data$LAT_pos, 
                        Plant_Numeric_level = data$Plant_Numeric_level, 
                        Animal_Numeric_level = data$Animal_Numeric_level,
                        Realm = data$Realm_WWF)

# Transform the number of potential and realized interactions in log
data_path$Potential_Connections <- log(data$Plants*data$Animals)
data_path$Connections_Log <- log(data$Connections)

# Fit a linear regression model 
lm_model <- lm(Connections_Log ~ Potential_Connections, data = data_path)

residuals <- round(residuals(lm_model), 3)

# Extract the residuals to create a new variable
data_path$Residual_Connectance <- round(residuals(lm_model), 3)

# Save as CSV file
write.csv(data_path, "D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 3 - Structural pattern/data/data_all2.csv", row.names=FALSE)

# If necessary to see correlation
cor1 = cor(data_path)
corrplot(cor1, method = 'square')

# Model selection by AIC
test1 <- lm(log(Connectance) ~ Plant_Numeric_level + Animal_Numeric_level + Animals + Plants + LAT_pos, data = data_path)
test2 <- lm(log(Connectance) ~ Animal_Numeric_level + Animals + Plants + LAT_pos, data = data_path)
test3 <- lm(log(Connectance) ~ Animal_Numeric_level + Animals + Plants, data = data_path)

# Define list of models
models <- list(test1, test2, test3)

# Specify model names
mod.names <- c('tax.plant.lat', 'no.plant', 'no.plant.lat')

# Calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)

#Variables in log: animals and plants
#
model1 = '
Animals ~ Animal_Numeric_level + LAT_pos + Plants
Connectance ~ Animal_Numeric_level + Animals + Plants
'

fit1 = cfa(model1, data = data_path)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit1, 'std', layout = 'tree',
         sizeMan = 10, sizeInt = 10, sizeLat = 10,
         edge.label.cex=1.5,
         fade=FALSE)


lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")

model2 = '
Animals ~ Animal_Numeric_level + LAT_pos + Plants
NODF ~ Animals + Plants + Animal_Numeric_level + Residual_Connectance + LAT_pos
Residual_Connectance ~ Animal_Numeric_level + LAT_pos + Plants + Animals
'

fit2 = cfa(model2, data = data_path)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit2, 'std', layout = 'tree',
         sizeMan = 10, sizeInt = 10, sizeLat = 10,
         edge.label.cex=1.5,
         fade=FALSE)

lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")



model3 = '
Animals ~ Animal_Numeric_level + LAT_pos + Plants
Modularity ~ Animals + Plants + Residual_Connectance + LAT_pos
Residual_Connectance ~ Animal_Numeric_level + LAT_pos + Plants + Animals
'

fit3 = cfa(model3, data = data_path)
summary(fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit3, 'std', layout = 'tree',
         sizeMan = 10, sizeInt = 10, sizeLat = 10,
         edge.label.cex=1.5,
         fade=FALSE)

lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")


###
model4 = '
Animal_Numeric_level ~ LAT_pos
Animals ~ Animal_Numeric_level + Plants + LAT_pos
Modularity ~ Animals + Animal_Numeric_level + Residual_Connectance
'

fit4 = cfa(model4, data = data_path)
summary(fit4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit4, 'std', layout = 'tree',
         sizeMan = 10, sizeInt = 10, sizeLat = 10,
         edge.label.cex=1.5,
         fade=FALSE)

lavaanPlot(model = fit4, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")


#
ggplot(data_path, aes(x=LAT_pos, y=Animals))+
  geom_point()


# plot a scatter plot
plot(data_path$LAT_pos, data_path$Animals)

# plot a regression line
abline(lm(Animals ~ LAT_pos, data=data_path), col='red')

#
count_result <- data %>%
  count(Animal_taxonomic_level, Region)

ggplot(count_result, aes(x=Animal_taxonomic_level, y=n, fill=Region)) +
  geom_bar(stat="identity", position=position_dodge())

#
count_result_2 <- data_path %>%
  count(Animal_Numeric_level, Region)

ggplot(count_result_2, aes(x=Animal_Numeric_level, y=n, fill=Region)) +
  geom_bar(stat="identity", position=position_dodge())

#####

# Dataset with most represented realms = Afrotropical, Nearctic, Neotropical, Palearctic
data_realm <- data %>% filter(Realm_WWF %in% c("Afrotropical", "Nearctic", "Neotropic", "Palearctic"))
#Variables in log: animals and plants
data_realm$Plants_LOG <- log(data_realm$Plants)
data_realm$Animals_LOG <- log(data_realm$Animals)

summary(data_realm)

#Creating dummy variables
library(fastDummies)
data_realm <- dummy_cols(data_realm, select_columns = "Realm_WWF")
head(data_realm)

# Path Analysis with categorical variables
library(seminr)
# Create measurement model
simple_mm <- constructs(
  composite("M_Beckett", single_item("M_Beckett")),
  composite("Realm_WWF_Neartic", single_item("Realm_WWF_Neartic")),
  composite("Realm_WWF_Neotropic", single_item("Realm_WWF_Neotropic")),
  composite("Realm_WWF_Paleartic", single_item("Realm_WWF_Paleartic")))

# Create structural model
simple_sm <- relationships(
  paths(from = c("Neartic", "Neotropic", "Paleartic"), to = "Modularity"))

# Estimate the model
simple_model <- estimate_pls(data = data_realm,
                             measurement_model = simple_mm,
                             structural_model = simple_sm,
                             missing = mean_replacement,
                             missing_value = "-99")


model6 = '
Animal_Numeric_level ~ Realm_WWF_Afrotropical + Realm_WWF_Neotropic
M_Beckett ~ Animals_LOG + Plants_LOG + Residual_Connectance + Realm_WWF_Afrotropical + Realm_WWF_Neotropic
'
fit6 = cfa(model6, data = data_realm)
summary(fit6, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

lavaanPlot(model = fit6, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")


model5 = '
Animal_Numeric_level ~ Realm_WWF
M_Beckett ~ Animals_LOG + Plants_LOG + Residual_Connectance + Realm_WWF
Residual_Connectance ~ Animal_Numeric_level + Realm_WWF + Plants_LOG + Animals_LOG
'

fit5 = cfa(model5, data = data_realm)
summary(fit5, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

lavaanPlot(model = fit5, node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "blue"), coefs = T, stand = T, stars = "regress")

summary(data_realm)
data_realm$Realm_WWF <- as.factor(data_realm$Realm_WWF)

# Model to test with most represented realms
model1 <- lm(log(Connectance) ~ 1, data = data_realm)
model2 <- lm(log(Connectance) ~ Realm_WWF + log(Size) + Animal_Numeric_level, data = data_realm)

plot(model2)
summary(model2)
anova(model1, model2)


# Dataset with grouping biomes
# Deserts and xeric shrublands = warm dry
# Mangrove = warm wet
# Mediterranean forests, woodlands, and shrub = cold wet
# Montane grasslands and shrublands = cold dry
# Tundra = cold dry
# Temperate wet, Temperate dry = cold
# Tropical wet, Tropical dry = warm

data_biomes <- data %>%
  mutate(Biome_WWF = ifelse(Biome_WWF == "Mangrove", "Warm wet",
                     ifelse(Biome_WWF == "Tundra", "Cold dry", 
                     ifelse(Biome_WWF == "Deserts and xeric shrublands", "Warm dry",
                     ifelse(Biome_WWF == "Mediterranean forests, woodlands, and shrub", "Cold wet",
                     ifelse(Biome_WWF == "Montane grasslands and shrublands", "Cold dry",
                     ifelse(Biome_WWF == "Temperate broadleaf and mixed forests", "Cold wet",
                     ifelse(Biome_WWF == "Temperate coniferous forests", "Cold wet",
                     ifelse(Biome_WWF == "Temperate grasslands, savannas, and shrublands", "Cold dry",
                     ifelse(Biome_WWF == "Tropical and subtropical coniferous forests", "Warm dry",
                     ifelse(Biome_WWF == "Tropical and subtropical dry broadleaf forests", "Warm wet",
                     ifelse(Biome_WWF == "Tropical and subtropical grasslands, savannas, and shrublands", "Warm dry",
                     ifelse(Biome_WWF == "Tropical and subtropical moist broadleaf forests", "Warm wet",
                            Biome_WWF)))))))))))))

# Checking on boxplot

ggplot(data_biomes, aes(x=Biome_WWF, y=Connectance)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")

# Test
model1 <- lm(log(Connectance) ~ 1, data = data_biomes)
model2 <- lm(log(Connectance) ~ Biome_WWF + log(Size) + Animal_Numeric_level, data = data_biomes)

plot(model2)
summary(model2)
anova(model1, model2)


##########
# Latitude x Animal Numeric Level
summary(data)
data$Animal_Numeric_level <- as.factor(data$Animal_Numeric_level)

ggplot(data, aes(x=Animal_Numeric_level, y=LAT_pos)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2),
              alpha = .4, size = 1,
              color = "brown")

# plot a scatter plot
plot(data$Animal_Numeric_level, data$LAT_pos)

model1 <- glm(log(Connectance) ~ Animal_Numeric_level, data = data)
model2 <- glm(LAT_pos ~ Animal_Numeric_level, data = data)

summary(model2)
