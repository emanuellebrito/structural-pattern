library(tidyverse)
library(ggplot2)

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
  select(Region, Hemisphere, Continent, Biome, Realm, Connectance) %>%
  gather(key = "Variable", value = "Value", -Connectance) %>%
  group_by(Variable, Value) %>%
  summarise(Count = n(), Connectance_mean = mean(Connectance), 
            Connectance_sd = sd(Connectance), NODF_mean = mean(NODF), 
            NODF_sd = sd(NODF), M_Beckett_mean = mean(M_Beckett), 
            M_Beckett_sd = sd(M_Beckett))


count_df <- as.data.frame(count_df)

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

# View the new data set
print(new_data)

#Plotting the means for each level of variables groups
#Connectance
new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x=Value,y=Connectance)) +
  geom_boxplot(width=0.25, fill = "white", outlier.color = "black")+
  geom_rect(aes(xmin = 0, xmax = 2.5, ymin = -Inf, ymax = +Inf), alpha = 0.01, fill = "#70D6FF") +
  geom_rect(aes(xmin = 2.5, xmax = 9.5, ymin = -Inf, ymax = +Inf), alpha = 0.01, fill = "#FF70A6") +
  geom_rect(aes(xmin = 9.5, xmax = 11.5, ymin = -Inf, ymax = +Inf), alpha = 0.01, fill = "#FF9770") +
  geom_rect(aes(xmin = 11.5, xmax = 17.5, ymin = -Inf, ymax = +Inf), alpha = 0.01, fill = "#FFD670") +
  geom_rect(aes(xmin = 17.5, xmax = 30.5, ymin = -Inf, ymax = +Inf), alpha = 0.01, fill = "#E9FF70") +
  xlab("Geographic reference") + 
  ylab("Connectance") +
  coord_flip()

new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x=Value,y=Connectance, color=Variable)) +
  geom_boxplot(width=0.25)+
  xlab("Geographic reference") + 
  ylab("Connectance") +
  coord_flip()

new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x=Value,y=NODF, color=Variable)) +
  geom_boxplot(width=0.25)+
  xlab("Geographic reference") + 
  ylab("Nestedness") +
  coord_flip()

new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x=Value,y=M_Beckett, color=Variable)) +
  geom_boxplot(width=0.25)+
  xlab("Geographic reference") + 
  ylab("Modularity") +
  coord_flip()



#####
library(patchwork)
library(cowplot)
library(ggplotify)

plot1 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = Connectance, color = Variable)) +
  geom_boxplot(width = 0.25) +
  xlab("Geographic reference") + 
  ylab("Connectance") +
  coord_flip() +
  theme(legend.position="none")

plot2 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = NODF, color = Variable)) +
  geom_boxplot(width = 0.25) +
  xlab("") + 
  ylab("Nestedness") +
  coord_flip() +
  theme(legend.position="none") 

plot3 <- new_data %>%
  mutate(Value = factor(Value, levels = rev(levels_order))) %>%
  ggplot(aes(x = Value, y = M_Beckett, color = Variable)) +
  geom_boxplot(width = 0.25) +
  xlab("") + 
  ylab("Modularity") +
  coord_flip() +
  theme(legend.position="none")

# Arrange the plots side by side
combined_plot <- plot1 + plot2 + plot3 +
  plot_layout(ncol = 3, guides = "collect")

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


tiff('geo_metrics.tif', w=2700, h=4000, units="px", res=300, compression = "lzw")
combined_plot
dev.off()

