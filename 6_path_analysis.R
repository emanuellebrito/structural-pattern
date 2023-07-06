# Load packages
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)

# Read the data
load("D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 3 - Structural pattern/structural-pattern/structural-pattern/path_data.RData")

summary(data)
data$Plant_Numeric_level <- as.numeric(data$Plant_Numeric_level)

data_path <- data.frame(Size = data$Size, Connections = data$Connections, 
                        Connectance = data$Connectance, Asymmetry = data$Asymmetry,
                        NODF = data$NODF, Plants = data$Plants, Animals = data$Animals,
                        Modularity = data$M_Beckett, LAT_pos = data$LAT_pos, 
                        Plant_Numeric_level = data$Plant_Numeric_level, 
                        Animal_Numeric_level = data$Animal_Numeric_level,
                        Realm = data$Realm_WWF)

cor1 = cor(data_path)
corrplot(cor1, method = 'square')

model1 = '
Plant_Numeric_level ~ Plants
Animal_Numeric_level ~ Animals
Animals + Plants ~ LAT_pos
Connectance ~ LAT_pos + Animals + Plants + Plant_Numeric_level + Animal_Numeric_level
'

fit1 = cfa(model1, data = data_path)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit1, 'std', layout = 'circle',
         sizeMan = 10, sizeInt = 10, sizeLat = 10,
         edge.label.cex=1.5,
         fade=FALSE)
