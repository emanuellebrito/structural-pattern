library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggplotify)

#Read data
data <- read.csv("results_networks.csv", header = TRUE, sep = ";", dec = ",")
summary(data)
head(data)

#Count values for categorical variables by each level and put the metrics means for each one
data_mean <- data.frame(Region = data$Region, Hemisphere = data$Hemisphere, 
                             Continent = data$Continent, Biome = data$Biome_WWF, 
                             Realm = data$Realm_WWF, Connectance = data$Connectance,
                             NODF = data$NODF, M_Beckett = data$M_Beckett, Size = data$Size,
                             Asymmetry = data$Asymmetry)

#Checking the means and sd for each variable levels
count_df <- data_mean %>%
  select(Region, Hemisphere, Continent, Biome, Realm, Connectance, NODF, M_Beckett) %>%
  gather(key = "Variable", value = "Value", -Connectance,-NODF,-M_Beckett) %>%
  group_by(Variable, Value) %>%
  summarise(Count = n(), Connectance_mean = mean(Connectance), 
            Connectance_sd = sd(Connectance), NODF_mean = mean(NODF), 
            NODF_sd = sd(NODF), M_Beckett_mean = mean(M_Beckett), 
            M_Beckett_sd = sd(M_Beckett))

# Create a new column "range" with categorized values for "Size"
data_group <- data_mean %>%
  mutate(range = cut(Size, breaks = c(0, 25, 50, 100, Inf),
                     labels = c("0-25", "26-50", "51-100", ">100"), include.lowest = TRUE))

# Count the frequencies in each range for "Size" and the count of different taxonomic levels
# Here I classified networks into mini, small, medium and large
count_data_combined <- data_group %>%
  group_by(range) %>%
  summarize(n = n(), mean_NODF = mean(NODF), sd_NODF = sd(NODF),
            mean_mod = mean(M_Beckett), sd_mod = sd(M_Beckett),
            mean_connect = mean(Connectance), sd_connect = sd(Connectance))

# Check data
print(count_data_combined)

#Reorder levels according to variables group
levels_order <- c("Deserts and xeric shrublands", "Flooded grasslands and savannas", 
  "Mangrove", "Mediterranean forests, woodlands, and shrub", "Montane grasslands and shrublands", 
  "Temperate broadleaf and mixed forests", "Temperate coniferous forests", 
  "Temperate grasslands, savannas, and shrublands", "Tropical and subtropical coniferous forests",
  "Tropical and subtropical dry broadleaf forests", "Tropical and subtropical grasslands, savannas, and shrublands", 
  "Tropical and subtropical moist broadleaf forests", "Tundra", "Africa", "Asia", "Europe", 
  "North America", "Oceania", "South America", "Northern", "Southern", "Afrotropical", "Australasian",
  "Indomalayan", "Nearctic", "Neotropic", "Oceanian", "Palearctic", "Temperate", "Tropical")


###########################################
# Create a new data frame with: Variables, Levels and Metrics
new_data <- data %>%
  select(Hemisphere, Region, Continent, Biome_WWF, Realm_WWF, Connectance, NODF, M_Beckett) %>%
  pivot_longer(cols = c(-Connectance, -NODF, -M_Beckett), names_to = "Variable", values_to = "Value")

# Check the data
print(new_data)

#Plotting the means for each level of variables groups
plot1 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = Connectance, color = Variable)) +
  geom_boxplot(width = 0.35) +
  xlab("Geographic reference") + 
  ylab("Connectance") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = c(-0.9, 0.1), axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 16))

plot2 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = NODF, color = Variable)) +
  geom_boxplot(width = 0.35) +
  xlab("") + 
  ylab("Nestedness") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position="none",
        axis.title.x = element_text(size = 16)) 

plot3 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = M_Beckett, color = Variable)) +
  geom_boxplot(width = 0.35) +
  xlab("") + 
  ylab("Modularity") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position="none",
        axis.title.x = element_text(size = 16))

# Convert each plot to a gtable object
plot1_gtable <- as_gtable(ggplotGrob(plot1))
plot2_gtable <- as_gtable(ggplotGrob(plot2))
plot3_gtable <- as_gtable(ggplotGrob(plot3))

# Remove x-axis from plot2 and plot3
plot2_gtable <- plot2_gtable[, -4]
plot3_gtable <- plot3_gtable[, -4]

plot1_width <- 3  # Adjust the width of plot1
plot2_width <- 1  # Width of plot2
plot3_width <- 1  # Width of plot3

# Combine the plot gtables into a single gtable
combined_gtable <- cowplot::plot_grid(
  plot1_gtable, plot2_gtable, plot3_gtable,
  ncol = 3, align = "h",
  rel_widths = c(plot1_width, plot2_width, plot3_width)
)

# Convert the combined gtable back to a plot
combined_plot <- as.ggplot(combined_gtable)

# Display the combined plot
combined_plot

# Saving figure
tiff('geo_metrics.tif', w=4700, h=4800, units="px", res=300, compression = "lzw")
combined_plot
dev.off()

############################
# Checking data for taxonomic variables
# First remove the NA's
data <- drop_na(data) 

# Create a new dataset only with taxnomic variables
taxon <- data.frame(Plant_level = data$Plant_taxonomic_level,
                        Plant_group = data$Plant_taxon_group, 
                        Animal_level = data$Animal_taxonomic_level,
                        Animal_group = data$Animal_taxon_group, 
                        Connectance = data$Connectance,
                        NODF = data$NODF, M_Beckett = data$M_Beckett, Size = data$Size,
                        Asymmetry = data$Asymmetry)

#Checking the means and sd for each variable levels
count_taxon <- taxon %>%
  select(Plant_level, Plant_group, Animal_level, Animal_group, Connectance, NODF, M_Beckett) %>%
  gather(key = "Variable", value = "Value", -Connectance, -NODF, -M_Beckett) %>%
  group_by(Variable, Value) %>%
  summarise(Count = n(), Connectance_mean = mean(Connectance), 
            Connectance_sd = sd(Connectance), NODF_mean = mean(NODF), 
            NODF_sd = sd(NODF), M_Beckett_mean = mean(M_Beckett), 
            M_Beckett_sd = sd(M_Beckett))



# Plotting
plot1 <- taxon %>%
  ggplot(aes(x = Animal_level, y = Connectance)) +
  geom_boxplot(width = 0.35) +
  xlab("Taxonomic level") + 
  ylab("Connectance") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = c(-0.9, 0.1), axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 16))

# Plotting
plot2 <- taxon %>%
  ggplot(aes(x = Animal_group, y = Connectance)) +
  geom_boxplot(width = 0.35) +
  xlab("Taxonomic group") + 
  ylab("Connectance") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = c(-0.9, 0.1), axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 16))


