# =====================================================
# URBAN GROWTH, POPULATION DENSITY & CCA ANALYSIS PIPELINE
# Author: Khalid Sulaiman
# =====================================================

# --- Load Required Libraries ---
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(factoextra)
library(cluster)
library(vegan)

# --- Set Working Directory ---
setwd("D:\\Population")

# =====================================================
# STEP 1: LOAD DATA
# =====================================================
urban_data <- read.csv("Updated_Urban_Area.csv")
population_data <- read.csv("Updated_Population_Data.csv")

# =====================================================
# STEP 2: RESHAPE TO LONG FORMAT
# =====================================================
urban_data_long <- urban_data %>%
  pivot_longer(cols = -Year, names_to = "City", values_to = "Urban_Area")

population_data_long <- population_data %>%
  pivot_longer(cols = -Year, names_to = "City", values_to = "Population")

# =====================================================
# STEP 3: MERGE BOTH DATASETS
# =====================================================
merged_data <- merge(urban_data_long, population_data_long, by = c("City", "Year"))

# =====================================================
# STEP 4: COMPUTE CHANGE (%) AND POPULATION DENSITY
# =====================================================
merged_data <- merged_data %>%
  arrange(City, Year) %>%
  group_by(City) %>%
  mutate(
    Change_Area = round(((Urban_Area - lag(Urban_Area)) / lag(Urban_Area)) * 100, 2),
    Change_Pop  = round(((Population - lag(Population)) / lag(Population)) * 100, 2),
    Density     = round(Population / Urban_Area, 2)
  )

# Replace NA in first-year changes with 0
merged_data <- merged_data %>%
  mutate(Change_Area = ifelse(is.na(Change_Area), 0, Change_Area),
         Change_Pop = ifelse(is.na(Change_Pop), 0, Change_Pop))

# =====================================================
# STEP 5: CLASSIFY URBAN AREAS BASED ON DENSITY THRESHOLD
# =====================================================
# 250 persons/km² threshold (World Bank & OECD standard)
merged_data <- merged_data %>%
  mutate(Urban_Class = ifelse(Density >= 250, "Urban", "Rural"))

# Display classification summary
print(table(merged_data$Urban_Class))

# =====================================================
# STEP 6: PERFORM K-MEANS CLUSTERING
# =====================================================
clustering_data <- merged_data %>% 
  select(Urban_Area, Population, Density) %>% 
  na.omit() %>%
  filter_all(all_vars(is.finite(.)))

# Scale features before clustering
if (nrow(clustering_data) > 3) {
  set.seed(123)
  clustering_scaled <- scale(clustering_data)
  kmeans_result <- kmeans(clustering_scaled, centers = 3, nstart = 25)
  
  merged_data$Cluster <- NA
  merged_data$Cluster[!is.na(merged_data$Urban_Area) &
                      !is.na(merged_data$Population) &
                      !is.na(merged_data$Density)] <- kmeans_result$cluster
  merged_data$Cluster <- as.factor(merged_data$Cluster)
} else {
  merged_data$Cluster <- NA
}

# =====================================================
# STEP 7: SAVE URBAN CHANGE TABLE
# =====================================================
write.csv(merged_data, "Urban_Change_Analysis.csv", row.names = FALSE)

# =====================================================
# STEP 8: GENERATE URBAN AREA GROWTH PLOTS
# =====================================================
figure_dir <- "D:\\Figures_Gen"
dir.create(figure_dir, showWarnings = FALSE)

for (city_name in unique(merged_data$City)) {
  city_data <- merged_data %>% filter(City == city_name)
  
  model <- lm(Urban_Area ~ Year, data = city_data)
  eq <- paste0("y = ", round(coef(model)[2], 4), "x + ", round(coef(model)[1], 1))
  
  plot <- ggplot(city_data, aes(x = Year, y = Urban_Area)) +
    geom_point(color = "red", size = 2) +
    geom_line(color = "red", linewidth = 1, linetype = "dotted") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
    annotate("text",
             x = min(city_data$Year, na.rm = TRUE) + 5,
             y = max(city_data$Urban_Area, na.rm = TRUE) - 5,
             label = eq, color = "black", size = 5) +
    labs(title = paste("Urban Area Change in", city_name),
         x = "Year", y = "Urban Area (km²)") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(filename = paste0(figure_dir, "/Urban_Area_", city_name, ".png"),
         plot = plot, width = 10, height = 6, dpi = 300)
}

# =====================================================
# STEP 9: SUMMARIZE CLASSIFICATION STATISTICS
# =====================================================
summary_stats <- merged_data %>%
  group_by(City, Urban_Class) %>%
  summarise(
    Mean_Area_Growth = mean(Change_Area, na.rm = TRUE),
    Mean_Pop_Growth  = mean(Change_Pop, na.rm = TRUE),
    Mean_Density     = mean(Density, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(summary_stats, "Urban_Summary_Stats.csv", row.names = FALSE)

# =====================================================
# STEP 10: CANONICAL CORRESPONDENCE ANALYSIS (CCA)
# =====================================================
cca_response <- merged_data %>% select(Urban_Area, Population, Density)
cca_explanatory <- merged_data %>% select(Year, Cluster, Urban_Class)

# Run CCA model
cca_model <- cca(cca_response ~ Year + Cluster + Urban_Class, data = merged_data)
summary(cca_model)
anova(cca_model, by = "term")  # significance of each explanatory variable

# =====================================================
# STEP 11: VISUALIZE CCA RESULTS
# =====================================================
plot(cca_model, display = "sites", type = "n", main = "CCA of Urban Dynamics")
points(cca_model, display = "sites",
       col = ifelse(merged_data$Urban_Class == "Urban", "red", "blue"), pch = 19)
legend("topright", legend = c("Urban", "Rural"),
       col = c("red", "blue"), pch = 19, bty = "n")

# Optional visualization for cluster influence
fviz_cluster(list(data = scale(merged_data[, c("Urban_Area", "Population", "Density")]),
                  cluster = merged_data$Cluster),
             geom = "point", ellipse.type = "convex",
             main = "K-Means Clusters of Urban Growth Dynamics")

# =====================================================
# STEP 12: SAVE FINAL CLASSIFIED DATASET
# =====================================================
write.csv(merged_data, "Urban_CCA_Classified_Data.csv", row.names = FALSE)

# =====================================================
# END OF SCRIPT
# =====================================================
