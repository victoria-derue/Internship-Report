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
library(corrr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyverse)
library(visdat) 


# Cleaning ----------------------------------------------------------------


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

# Create lags for the urban development
Full_df <- Full_df %>%
  arrange(year) %>%
  mutate(
    # --- 1-YEAR LAG ---
    across(
      .cols = c(
        contains("modifications"),
        contains("private_villa_area_square_foot_new_building"),
        contains("investment_villa_area_square_foot_new_building")
      ),
      .fns = ~lag(.x, 1),
      .names = "{.col}_lagged"
    ),
    
    # --- 2-YEAR LAG ---
    across(
      .cols = c(
        contains("multi_storey_buildings_area_square_foot_new_building"), 
        contains("floor_area_ratio_buildings_area_square_foot_new_building"),
        contains("industrial_buildings_area_square_foot_new_building"),
        contains("public_buildings_area_square_foot_new_building"),
        contains("commercial_buildings_area_square_foot_new_building")
      ),
      .fns = ~lag(.x, 2),
      .names = "{.col}_lagged"
    )
  ) %>%
  # --- THE NEW TOTAL ---
  mutate(
    total_area_impact = rowSums(select(., ends_with("_lagged")), na.rm = TRUE)
  )

# Check for missing values
vis_miss(Full_df) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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

# Look at the df summary
summary(Full_df)

# Transform the data
Full_df <- Full_df %>%
  mutate(across(
    .cols = where(is.numeric) & !all_of(c("year")),
    .fns = ~ asinh(.x), 
    .names = "trans_{.col}"
  ))

qqnorm(Full_df$trans_total_quantity_consumed)
qqline(Full_df$trans_total_quantity_consumed, col = "red")

# Set your transformed target
target_var <- "trans_total_quantity_consumed"

# Identify predictors: Only those starting with 'trans_' OR 'year'
# But remove the target itself so you don't plot it against itself
plot_vars <- names(Full_df)[grepl("^trans_|year", names(Full_df))]
plot_vars <- setdiff(plot_vars, target_var)

# Run the loop
for (v in plot_vars) {
  p <- ggplot(Full_df, aes(x = .data[[v]], y = .data[[target_var]])) +
    geom_point(alpha = 0.6, color = "midnightblue") +
    geom_smooth(method = "lm", color = "blue", se = TRUE) +   
    geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") + 
    labs(
      title = paste("Linearity Check: ", v, "vs", target_var),
      subtitle = "Closer Red/Blue lines indicate better fit for Linear Regression",
      x = v,
      y = "Transformed Consumption"
    ) +
    theme_minimal()
  
  print(p)
  Sys.sleep(0.3) # Slightly faster sleep
}
# Some red lines are relatively straight, others are pretty wavy. The year plot line is also relatively straight and increasing over time.

# Check for normality
shapiro.test(Full_df$trans_total_quantity_consumed)
# The normality assumption is violated

# Create a new dataframe to store only the values of the study
df_wide <- Full_df[Full_df$year >= 2015 & Full_df$year <= 2023, ]

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
  geom_line() +
  geom_point() +
  labs(title = "Annual Consumption Trend",
       x = "Year",
       y = "Total Quantity Consumed") +
  theme_minimal()

# Urban Expansion Plot
ggplot(df_wide, aes(x = total_area_impact, y = total_quantity_consumed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "firebrick") + 
  labs(title = "Impact of Urban Expansion on Consumption",
       x = "Total Area (Square Foot)",
       y = "Total Quantity Consumed") +
  theme_minimal()


# Demographic Plot
ggplot(df_wide, aes(x = total_population, y = total_quantity_consumed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Demographic Drivers of Consumption",
       x = "Total Population",
       y = "Total Quantity Consumed") +
  theme_minimal()

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
  labs(title = "Population Dynamics (2015-2023)",
       y = "Number of People",
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
  labs(title = "Water Consumption Trends by Sector",
       y = "Volume (Million Imperial Gallons)",
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
  labs(title = "Construction Trends (New Buildings): Licensed Area by Sector",
       y = "Total Area (Square Foot)",
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
  labs(title = "Construction Trends (Modifications): Licensed Area by Sector",
       y = "Total Area (Square Foot)",
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
       y = "Total Area (Square Foot)",
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
  labs(title = "Water Infrastructure: Capacity vs. Production",
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
  labs(title = "New Buildings vs. Modifications",
       x = "Area (Square Foot)",
       y = "Density")

# Area Comparison (Combined New & Modifications)
df_long %>%
  filter(grepl("_area_", Metric)) %>%
  filter(!grepl("total|trans_", Metric)) %>%
  mutate(Category = if_else(grepl("new", Metric), "New Buildings", "Modifications")) %>%
  ggplot(aes(x = Metric, y = Value, fill = Category)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, outlier.color = "red") +
  facet_wrap(~Category, scales = "free", ncol = 1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Building Area Outliers", 
       x = "", 
       y = "Square Feet")

# Water Outliers
df_long %>%
  filter(grepl("quantity_consumed|capacity|produced", Metric)) %>%
  filter(!grepl("total|trans_", Metric)) %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_boxplot(fill = "steelblue", outlier.color = "firebrick", outlier.shape = 1) +
  facet_wrap(~Metric, scales = "free", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Water Supply & Consumption Outliers", 
       x = "", 
       y = "Volume (Million Imperial Gallons)")

# Population Outliers
df_long %>%
  filter(Metric %in% c("emirati", "non_emirati")) %>%
  ggplot(aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Outlier Detection: Population Segments",
       x = "Nationality",
       y = "Number of People")


# Correlation -------------------------------------------------------------


# Focus only on the transformed variables for the correlation
trans_df <- df_wide %>% select(year, starts_with("trans_"))

# Create a correlation matrix
cor_matrix <- cor(trans_df, use = "complete.obs")

# Look specifically at what correlates with total consumption
consumption_cor <- cor_matrix[, "trans_total_quantity_consumed"]
sort(consumption_cor, decreasing = TRUE)

corrplot(cor_matrix, method = "color", order = "hclust", tl.cex = 0.5)


# Trends ------------------------------------------------------------------


# The "Efficiency" Check
df_wide <- df_wide %>%
  mutate(MIG_per_person = total_quantity_consumed / total_population)

ggplot(df_wide, aes(x = year, y = MIG_per_person)) +
  geom_line(color = "steelblue", size = 1) + 
  geom_point() +
  labs(title = "Is the City becoming more efficient?", 
       y = "Million Imperial Gallons per Capita") +
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
  labs(title = "Comparative Annual Growth Rates (YoY)",
       y = "% Change YoY",
       x = "Year",
       color = "Growth Metrics") +
  theme_minimal()



# Modelling ----------------------------------------------------------------


# Using the full dataframe
model_df <- Full_df %>% 
  dplyr::select(year, starts_with("trans_"))

model_df <- model_df %>%
  dplyr::select(
    -any_of(c("trans_total_population", 
              "trans_quantity_produced", 
              "trans_total_capacity", 
              "trans_total_number_of_customer",
              "trans_total_area_new_building",
              "trans_total_area_additions_and_amendments",
              "trans_total_area")))

# Create a clean version with no NA values
model_df <- na.omit(model_df)

# Create a correlation matrix
cor_model <- cor(model_df, use = "complete.obs")

# Look specifically at what correlates with total consumption
model_cor <- cor_model[, "trans_total_quantity_consumed"]
sort(model_cor, decreasing = TRUE)

corrplot(cor_model, method = "color", order = "hclust", tl.cex = 0.5)


# The models
exp_model <- lm(trans_total_quantity_consumed ~ 
                    trans_non_emirati + 
                    trans_investment_villa_area_new, 
                  data = model_df)
summary(exp_model)
vif(exp_model)
AIC(exp_model)

exp_model_1 <- lm(trans_total_quantity_consumed ~ 
                    trans_non_emirati + 
                    trans_investment_villa_area_new +
                    trans_multi_storey_buildings_area_modifications_lagged ,
                  data = model_df)
summary(exp_model_1)
vif(exp_model_1)
AIC(exp_model_1)

exp_model_2 <- lm(trans_total_quantity_consumed ~ 
                    trans_non_emirati + 
                    trans_multi_storey_buildings_area_modifications_lagged +
                    trans_investment_villa_area_modifications_lagged,
                  data = model_df)
summary(exp_model_2)
vif(exp_model_2)
AIC(exp_model_2)

exp_model_3 <- lm(trans_total_quantity_consumed ~ 
                    trans_non_emirati + 
                    trans_multi_storey_buildings_area_modifications_lagged +
                    trans_investment_villa_area_modifications_lagged +
                    trans_total_area_impact,
                  data = model_df)
summary(exp_model_3)
vif(exp_model_3)
AIC(exp_model_3)
