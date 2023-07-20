# Load packages
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)
library(lavaanPlot)
library(AICcmodavg)
library(tidyverse)


# Read the data
load("D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 3 - Structural pattern/structural-pattern/structural-pattern/path_data.RData")

summary(data)
data$Plant_Numeric_level <- as.numeric(data$Plant_Numeric_level)

data_path <- data.frame(Region = data$Region, Hemisphere = data$Hemisphere, Size = data$Size, Connections = data$Connections, 
                        Connectance = data$Connectance, Asymmetry = data$Asymmetry,
                        NODF = data$NODF, Plants = log(data$Plants), Animals = log(data$Animals),
                        Modularity = data$M_Beckett, LAT_pos = data$LAT_pos, 
                        Plant_Numeric_level = data$Plant_Numeric_level, 
                        Animal_Numeric_level = data$Animal_Numeric_level,
                        Realm = data$Realm_WWF)

# Transform the number of potential and realized interactions in log
data_path$Potential_Connections <- log(data$Plants*data$Animals)
data_path$Connections <- log(data$Connections)

# Fit a linear regression model 
lm_model <- lm(Connections ~ Potential_Connections, data = data_path)

residuals <- residuals(lm_model)

# Extract the residuals to create a new variable
data_path$Residual_Connectance <- residuals(lm_model)

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
#Plants + Animals ~ Region
model1 = '
Connectance ~ Animal_Numeric_level + Animals + Plants
Animals ~ Animal_Numeric_level + LAT_pos + Plants
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
NODF ~ Animals + Plants + Animal_Numeric_level + Residual_Connectance
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
Modularity ~ Animals + Plants + Animal_Numeric_level + Residual_Connectance
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