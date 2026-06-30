# Environment -------------------------------------------------------------

# Load the datasets
Population <- read.csv("Population_Estimated.csv", 
                       header = TRUE)
Consumption <- read.csv("Water_Consumption.csv", 
                        header = TRUE)
Production <- read.csv("Water_Production.csv", 
                       header = TRUE)
Capacity <- read.csv("Desalination_Capacity.csv", 
                     header = TRUE)
Permits <- read.csv("Building_Permits_Issued.csv", 
                    header = TRUE)
Full_df <- read.csv("Full_Dataset.csv", 
                    header = TRUE)

# Libraries
library(car)
library(corrplot)
library(dplyr)
library(ggplot2)
library(janitor)
library(mgcv)
library(tidyverse)
library(visdat) 


# Data Cleaning ----------------------------------------------------------------


# Clean the names
Population <- clean_names(Population)
Consumption <- clean_names(Consumption)
Production <- clean_names(Production)
Capacity <- clean_names(Capacity)
Permits <- clean_names(Permits)
Full_df <- clean_names(Full_df)

# Check the structure of the data (for the data types)
str(Full_df)
names(Full_df)

# Remove the permit
Full_df <- Full_df %>% 
  select(-contains("number_new"),
         -contains("number_additions"),
         -contains("permits"))

# Rename some columns as they are too long
Full_df <- Full_df %>%
  rename(
    private_villa_area_modifications = private_villa_area_square_foot_additions_and_amendments,
    private_villa_area_new = private_villa_area_square_foot_new_building,
    investment_villa_area_modifications = investment_villa_area_square_foot_additions_and_amendments,
    investment_villa_area_new = investment_villa_area_square_foot_new_building,
    industrial_buildings_area_modifications = industrial_buildings_area_square_foot_additions_and_amendments,
    industrial_buildings_area_new = industrial_buildings_area_square_foot_new_building,
    public_buildings_area_modifications = public_buildings_area_square_foot_additions_and_amendments,
    public_buildings_area_new = public_buildings_area_square_foot_new_building,
    multi_storey_buildings_area_modifications = multi_storey_buildings_area_square_foot_additions_and_amendments,
    multi_storey_buildings_area_new = multi_storey_buildings_area_square_foot_new_building,
    floor_area_ratio_buildings_area_modifications = floor_area_ratio_buildings_area_square_foot_additions_and_amendments,
    floor_area_ratio_buildings_area_new = floor_area_ratio_buildings_area_square_foot_new_building
  )

# Check for missing values
vis_miss(Full_df) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Fix the government NA entries by assuming 0 activity
Full_df$floor_area_ratio_buildings_area_new[is.na(Full_df$floor_area_ratio_buildings_area_new)] <- 0
Full_df$floor_area_ratio_buildings_area_modifications[is.na(Full_df$floor_area_ratio_buildings_area_modifications)] <- 0

# Build the model using only rows that ARE NOT missing data
temp_model <- lm(total_system_requirement_groundwater ~ residential_quantity_consumed + commercial_quantity_consumed + industrial_quantity_consumed + other_quantity_consumed, 
                 data = Full_df, 
                 na.action = na.omit)

# Define the target rows
missing_rows <- is.na(Full_df$total_system_requirement_groundwater)

# Fill Groundwater with rounded-up predictions
Full_df$total_system_requirement_groundwater[missing_rows] <- ceiling(predict(temp_model, 
                                                                              newdata = Full_df[missing_rows, ])
)

# Update the Grand Total to reflect the new values
Full_df$total_system_requirement[missing_rows] <- 
  Full_df$total_system_requirement_desalination_water_demand[missing_rows] + 
  Full_df$total_system_requirement_groundwater[missing_rows]

# Cleanup
rm(temp_model, missing_rows)

# Create lags for the urban development
Full_df <- Full_df %>%
  arrange(year) %>%
  mutate(
    # --- 1-YEAR LAG ---
    across(
      .cols = c(
        contains("modifications"),
        contains("private_villa_area_new"),
        contains("investment_villa_area_new")
      ),
      .fns = ~lag(.x, 1),
      .names = "{.col}_lagged"
    ),
    
    # --- 2-YEAR LAG ---
    across(
      .cols = c(
        contains("multi_storey_buildings_area_new"), 
        contains("floor_area_ratio_buildings_area_new"),
        contains("industrial_buildings_area_new"),
        contains("public_buildings_area_new"),
        contains("commercial_buildings_area_new")
      ),
      .fns = ~lag(.x, 2),
      .names = "{.col}_lagged"
    )
  ) %>%
  # --- THE NEW TOTAL ---
  mutate(
    total_area_new_building_lagged = rowSums(select(., ends_with("_new_lagged")), na.rm = TRUE),
    total_area_additions_and_amendments_lagged = rowSums(select(., ends_with("_modifications_lagged")), na.rm = TRUE),
    total_area_impact = rowSums(select(., ends_with("_lagged")), na.rm = TRUE)
  )

# Create a new dataframe to store only the values of the study
Full_df <- Full_df[Full_df$year >= 2015 & Full_df$year <= 2023, ]

# Look at the df summary
summary(Full_df)

# Create a new dataframe to store only the values of the study
df_wide <- Full_df

head(df_wide)


# EDA ---------------------------------------------------------------------


# Look at the df summary
summary(df_wide)

# Pivot the df for visualization purposes
df_long <- df_wide %>%
  pivot_longer(
    cols = -year,
    names_to = "Metric",
    values_to = "Value"
  )

# Add themes
df_long <- df_long %>%
  mutate(Theme = case_when(
    str_detect(Metric, "population|emirati") ~ "Demographics",
    str_detect(Metric, "quantity_consumed|customer|system_requirement") ~ "Water Consumption",
    str_detect(Metric, "quantity_produced") ~ "Water Production",
    str_detect(Metric, "capacity") ~ "Capacity",
    str_detect(Metric, "area") ~ "Licensed Area (Square Foot)"
  ))

# Time Series of Consumption
ggplot(df_wide, aes(x = year, y = total_quantity_consumed)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 1) +
  labs(
    title = "Annual Total Water Consumption Trend in Dubai (2015–2023)",
    x = "Year",
    y = "Water Consumption (Million Imperial Gallons)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# Urban Expansion Plot
ggplot(df_wide, aes(x = total_area_impact, y = total_quantity_consumed)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) + 
  labs(
    title = "Impact of Total Lagged Urban Development on Total Water Consumption",
    x = "Lagged Licensed Area (Square Foot)",
    y = "Water Consumption (Million Imperial Gallons)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# Demographic Plot
ggplot(df_wide, aes(x = total_population, y = total_quantity_consumed)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = TRUE) + 
  labs(
    title = "Impact of Population Growth on Total Water Consumption",
    x = "Number of Residents",
    y = "Water Consumption (Million Imperial Gallons)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# Full graphs
ggplot(df_long, aes(x = year, y = Value)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point() +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Lineary Check: Main Metrics vs. Year",
       y = "Metric Value",
       x = "Year")


# Theme 1: Demographics & Population
theme_demographics <- c("emirati", "non_emirati", "total_population")

df_long %>%
  filter(Metric %in% theme_demographics) %>%
  ggplot(aes(x = year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("emirati" = "darkgreen", 
                                "non_emirati" = "red", 
                                "total_population" = "black")) +
  theme_minimal() +
  labs(title = "Annual Population Trends by Nationality in Dubai (2015–2023)",
       y = "Number of Residents",
       x = "Year",
       color = "Nationality") +
  theme(legend.position = "right")

# Theme 2: Water Consumption & Customers
theme_consumption <- c("residential_quantity_consumed", "commercial_quantity_consumed", "industrial_quantity_consumed", "other_quantity_consumed", "total_quantity_consumed")

df_long %>%
  filter(Metric %in% theme_consumption) %>%
  ggplot(aes(x = year, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Total Water Consumption by Sector in Dubai (2015–2023)",
       y = "Water Consumption (Million Imperial Gallons)",
       x = "Year",
       color = "Sector") +
  theme(legend.position = "right")

# Theme 3: Licensed Area Activity
theme_new <- c("private_villa_area_new", "investment_villa_area_new", "industrial_buildings_area_new", "public_buildings_area_new", "multi_storey_buildings_area_new", "floor_area_ratio_buildings_area_new", "total_area_new_building")

df_long %>%
  filter(Metric %in% theme_new) %>%
  ggplot(aes(x = year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Licensed New Building Construction Area by Sector in Dubai (2015–2023)",
       y = " Licensed Area (Square Foot)",
       x = "Year",
       color = "Building Type") +
  theme(legend.position = "right")

theme_modif <- c("private_villa_area_modifications", "investment_villa_area_modifications", "industrial_buildings_area_modifications", "public_buildings_area_modifications", "multi_storey_buildings_area_modifications", "floor_area_ratio_buildings_area_modifications", "total_area_additions_and_amendments")

df_long %>%
  filter(Metric %in% theme_modif) %>%
  ggplot(aes(x = year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Licensed Modifications Area by Sector in Dubai (2015–2023)",
       y = " Licensed Area (Square Foot)",
       x = "Year",
       color = "Building Type") +
  theme(legend.position = "right")

df_long %>%
  filter(Metric %in% c("total_area_new_building", "total_area_additions_and_amendments", "total_area")) %>%
  ggplot(aes(x = year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Macro Construction Trends: New vs. Existing Buildings",
       y = "Licensed Area (Square Foot)",
       x = "Year",
       color = "Permit Type") +
  theme(legend.position = "right")

# Theme 4: Infrastructure & System Capacity
theme_infra <- c("desalination_stations_capacity", "total_capacity", "total_system_requirement", "quantity_produced", "total_quantity_consumed")

df_long %>%
  filter(Metric %in% theme_infra) %>%
  ggplot(aes(x = year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Water Infrastructure Performance: Capacity, Production, and Consumption in Dubai (2015–2023)",
       y = "Volume (Million Imperial Gallons)",
       x = "Year",
       color = "Metric") + 
  theme(legend.position = "right")


# Main variables graphs
df_long %>%
  filter(Metric %in% c("quantity_produced", "total_population", "total_quantity_consumed", "total_area")) %>%
  ggplot(aes(x = factor(year), y = Value, group = Metric)) + 
  geom_line(color = "firebrick") +
  geom_point() +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() + 
  labs(x = "Year")


# Distribution
df_long %>%
  filter(Metric %in% c("total_area_new_building", "total_area_additions_and_amendments")) %>%
  ggplot(aes(x = Value, fill = Metric)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
  geom_density(alpha = 0.2) +
  theme_minimal() +
  labs(title = "Distribution of Licensed Construction Area",
       x = "Licensed Area (Square Foot)",
       y = "Density")

# Area Comparison (Combined New & Modifications)
df_long %>%
  filter(grepl("_area_", Metric)) %>%
  filter(!grepl("total|_lagged", Metric)) %>%
  mutate(Category = if_else(grepl("new", Metric), "New Buildings", "Modifications")) %>%
  ggplot(aes(x = Metric, y = Value, fill = Category)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, outlier.color = "red") +
  facet_wrap(~Category, scales = "free", ncol = 1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Outlier Analysis: Licensed Building Area by Category", 
       x = "", 
       y = "Licensed Area (Square Foot)")

# Water Outliers
df_long %>%
  filter(grepl("quantity_consumed|capacity|produced", Metric)) %>%
  filter(!grepl("total", Metric)) %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_boxplot(fill = "steelblue", outlier.color = "firebrick", outlier.shape = 1) +
  facet_wrap(~Metric, scales = "free", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Outlier Analysis: Water Supply, Capacity, and Consumption", 
       x = "", 
       y = "Volume (Million Imperial Gallons)")

# Population Outliers
df_long %>%
  filter(Metric %in% c("emirati", "non_emirati")) %>%
  ggplot(aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Outlier Analysis: Population Segments by Nationality",
       x = "Nationality",
       y = "Number of Residents")


# Define the base metrics
all_construction_metrics <- c(theme_new, theme_modif)

# Identify the already created lagged columns (assuming they end in '_lagged')
all_lagged_metrics <- paste0(all_construction_metrics, "_lagged")

# Create log-transformed columns for both original and lagged versions
df_wide <- df_wide %>%
  mutate(across(all_of(c(all_construction_metrics, all_lagged_metrics)), 
                list(log = ~log(. + 1)), 
                .names = "log_{.col}"))

# Verification: Density plot of a sample variable
ggplot(df_wide, aes(x = total_area_new_building)) + 
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Original Distribution: Total New Building Area")

ggplot(df_wide, aes(x = log_total_area_new_building)) + 
  geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(title = "Log-Transformed Distribution: Total New Building Area")



# Diagnosis ---------------------------------------------------------------

## Define your full list of predictors (excluding Y)
# We include the original themes and the transformed construction themes
predictors <- names(df_wide %>% select(where(is.numeric), -year, -total_quantity_consumed))

par(mfrow = c(3, 3)) 

# Generate the Q-Q plots
for (var in predictors) {
  qqnorm(df_wide[[var]], main = paste("QQ:", var))
  qqline(df_wide[[var]], col = "darkblue")
}

# Generate the Scatterplots
for (var in predictors) {
  if (var %in% names(df_wide)) {
    plot(df_wide[[var]], df_wide$total_quantity_consumed,
         main = paste("total_quantity_consumed vs", var),
         xlab = var, ylab = "Water Consumption",
         pch = 19, col = "grey50")
    
    # Adding a trend line to identify non-linear patterns
    lines(lowess(df_wide[[var]], df_wide$total_quantity_consumed), 
          col = "red", lwd = 2)
  }
}

par(mfrow = c(1, 1))


# Correlation -------------------------------------------------------------


# Create the model_df by excluding the raw construction metrics
corr_df <- df_wide

# Calculate Kendall's Tau Correlation Matrix
kendall_matrix <- cor(corr_df, use = "complete.obs", method = "kendall")

# Look specifically at what correlates with total consumption
kendall_consumption <- kendall_matrix[, "total_quantity_consumed"]
sort(kendall_consumption, decreasing = TRUE)

# Perform the drop
corr_df <- corr_df %>% 
  select(-any_of(
    c(all_construction_metrics, 
      all_lagged_metrics, 
      paste0("log_", all_construction_metrics)
  )))

# Calculate Kendall's Tau Correlation Matrix
kendall_matrix <- cor(corr_df, use = "complete.obs", method = "kendall")

# Look specifically at what correlates with total consumption
kendall_consumption <- kendall_matrix[, "total_quantity_consumed"]
sort(kendall_consumption, decreasing = TRUE)

# Update the column names in your matrix
colnames(kendall_matrix) <- gsub("log_|", "", colnames(kendall_matrix))
colnames(kendall_matrix) <- gsub("_lagged", "", colnames(kendall_matrix))

# Update the row names to match
rownames(kendall_matrix) <- colnames(kendall_matrix)

# Visualize with a heatmap
corrplot(kendall_matrix, 
         method = "color", 
         order = "hclust", 
         tl.cex = 0.4, 
         title = "Correlation Matrix (Kendall's Tau)",
         mar = c(0,0,1,0))


# Trends ------------------------------------------------------------------


# The "Efficiency" Check
df_wide <- df_wide %>%
  mutate(MIG_per_person = total_quantity_consumed / total_population)

ggplot(df_wide, aes(x = year, y = MIG_per_person)) +
  geom_line(color = "steelblue", size = 1) + 
  geom_point() +
  labs(title = "Annual Water Consumption per Capita", 
       y = "Water Consumption (Million Imperial Gallons / Person)", 
       x = "Year") +
  theme_minimal()

# Feature engineering
df_wide <- df_wide %>%
  arrange(year) %>%
  mutate(across(
    .cols = c(total_population, total_quantity_consumed, 
              quantity_produced, total_capacity, total_area_impact),
    .fns = ~ ((.x - lag(.x)) / lag(.x)) * 100,
    .names = "{.col}_yoy"
  )) %>%
  mutate(
    water_intensity_growth_ratio = total_quantity_consumed_yoy / total_area_impact_yoy,
    per_capita_efficiency_ratio = total_quantity_consumed_yoy / total_population_yoy,
    densification_ratio = total_population_yoy / total_area_impact_yoy)

# Compare Ratios
ggplot(df_wide, aes(x = year)) +
  geom_line(aes(y = water_intensity_growth_ratio, color = "Water Intensity Growth Ratio"), size = 1) +
  geom_line(aes(y = per_capita_efficiency_ratio, color = "Per Capita Efficiency Ratio"), size = 1) +
  geom_line(aes(y = densification_ratio, color = "Densification Ratio"), size = 1) +
  labs(title = "Relative Growth Intensity Ratios",
       y = "Growth Ratio (Index)",
       x = "Year",
       color = "Metric type") +
  theme_minimal()


# Compare Growth
ggplot(df_wide, aes(x = year)) +
  geom_line(aes(y = total_population_yoy, color = "Population Growth"), size = 1) +
  geom_line(aes(y = total_quantity_consumed_yoy, color = "Water Consumption Growth"), size = 1) +
  geom_line(aes(y = quantity_produced_yoy, color = "Water Production Growth"), size = 1) +
  geom_line(aes(y = total_capacity_yoy, color = "Water Capacity Growth"), size = 1) +
  #geom_line(aes(y = total_area_impact_yoy, color = "Licensed Areas Growth"), size = 1) +
  labs(title = "Year-over-Year (YoY) Growth Comparisons",
       y = "Annual Percentage Change (%)",
       x = "Year",
       color = "Growth Metrics") +
  theme_minimal()



# Modelling ----------------------------------------------------------------


# Drop the redundancy (all the sums)
model_df <- corr_df %>% 
  select(-c(residential_quantity_consumed,
            commercial_quantity_consumed,
            industrial_quantity_consumed,
            other_quantity_consumed,
            total_population,
            total_number_of_customer,
            total_system_requirement,
            total_capacity,
            log_total_area_new_building_lagged,
            log_total_area_additions_and_amendments_lagged,
            total_area_impact,
            total_area))

# Drop |cor| < 0.5   
model_df <- model_df %>% 
  select(-c(log_multi_storey_buildings_area_new_lagged,
            log_floor_area_ratio_buildings_area_new_lagged,
            log_private_villa_area_modifications_lagged,
            total_system_requirement_groundwater,
            log_public_buildings_area_modifications_lagged,
            log_floor_area_ratio_buildings_area_modifications_lagged))

# Drop |cor| = 1
model_df <- model_df %>% 
  select(-c(total_system_requirement_desalination_water_demand,
            quantity_produced))

# Drop multicollinearity with non_emirati
model_df <- model_df %>% 
  select(-c(year,
            emirati,
            residential_number_of_customer,
            commercial_number_of_customer))
vif(lm(non_emirati ~ industrial_number_of_customer + other_number_of_customer, data = model_df))

# Drop multicollinearity with desalination_stations_capacity
vif(lm(desalination_stations_capacity ~ wells_capacity  + log_public_buildings_area_new_lagged, data = model_df))
model_df <- model_df %>% 
  select(-c(wells_capacity,
            log_public_buildings_area_new_lagged))

# Create a clean version with no NA values
model_df <- na.omit(model_df)

# Create a correlation matrix
cor_model <- cor(model_df, use = "complete.obs", method = "kendall")

# Look specifically at what correlates with total consumption
model_cor <- cor_model[, "total_quantity_consumed"]
sort(model_cor, decreasing = TRUE)

corrplot(cor_model, method = "color", order = "hclust", tl.cex = 0.5)


# The models
model1 <- gam(total_quantity_consumed ~ s(non_emirati, k=3), 
             data = model_df, method = "REML")
summary(model1)
AIC(model1)

model2 <- gam(total_quantity_consumed ~ s(non_emirati, k=3) + s(log_investment_villa_area_modifications_lagged, k=3), 
             data = model_df, method = "REML")
summary(model2)
AIC(model2)

model3 <- gam(total_quantity_consumed ~ s(non_emirati, k=3) + s(industrial_number_of_customer, k=3), 
             data = model_df, method = "REML")
summary(model3)
AIC(model3)

model4 <- gam(total_quantity_consumed ~ s(non_emirati, k=3) + s(log_industrial_buildings_area_new_lagged, k=3), 
             data = model_df, method = "REML")
summary(model4)
AIC(model4)

model5 <- gam(total_quantity_consumed ~ s(non_emirati, k=3) + s(log_investment_villa_area_modifications_lagged, k=3) + s(log_industrial_buildings_area_new_lagged, k=3), 
             data = model_df, method = "REML")
summary(model5)
AIC(model5)

gam.check(model5)
