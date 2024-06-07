library(tidyverse)  # Load the tidyverse package, which includes dplyr and other useful packages

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define the file path for the old and new raions CSV file
old_new_raions_file_path <- file.path("displacement_data", "old_new_raions.csv")

# Define the file path for the raions CSV file
raions_file_path <- file.path("displacement_data", "raions.csv")

# Read the old and new raions CSV file into a dataframe
old_new_raions_df <- read_csv(old_new_raions_file_path)

# Read the raions CSV file into a dataframe
raions_df <- read_csv(raions_file_path)

# Merge the raions dataframe with the old_new_raions dataframe based on columns "NAME_1" and "NAME_2"
merged_df <- left_join(raions_df, old_new_raions_df, by = c("NAME_1", "NAME_2"))

# Calculate the updated total baseline population for each raion
updated_total_baseline_raion_pop <- merged_df %>%
  distinct(ADM1_EN, ADM2_EN, total_baseline_raion_population) %>%  # Select unique rows
  group_by(ADM1_EN, ADM2_EN) %>%  # Group by ADM1_EN and ADM2_EN
  summarise(
    total_baseline_raion_population = sum(total_baseline_raion_population, na.rm = TRUE),  # Sum the population, ignoring NA values
    .groups = "drop"  # Drop grouping after summarising
  ) %>%
  arrange(ADM1_EN, ADM2_EN)  # Arrange the result by ADM1_EN and ADM2_EN

# Calculate the updated total baseline device count for each raion
updated_total_baseline_raion_device_count <- merged_df %>%
  distinct(ADM1_EN, ADM2_EN, median_pop) %>%  # Select unique rows
  group_by(ADM1_EN, ADM2_EN) %>%  # Group by ADM1_EN and ADM2_EN
  summarise(
    median_baseline_raion_device_count = sum(median_pop, na.rm = TRUE),  # Sum the device count, ignoring NA values
    .groups = "drop"  # Drop grouping after summarising
  ) %>%
  arrange(ADM1_EN, ADM2_EN)  # Arrange the result by ADM1_EN and ADM2_EN

# Calculate the updated total daily device counts for each raion
updated_raions_total_daily_device_counts <- merged_df %>%
  group_by(ADM1_EN, ADM2_EN, month, day) %>%  # Group by ADM1_EN, ADM2_EN, month, and day
  summarise(
    total_daily_count_after_invasion = sum(total_count, na.rm = TRUE),  # Sum the daily counts, ignoring NA values
    .groups = "drop"  # Drop grouping after summarising
  ) %>%
  arrange(ADM1_EN, ADM2_EN, month, day)  # Arrange the result by ADM1_EN, ADM2_EN, month, and day

# Calculate the updated net refugee outflow for each day
updated_net_refugee_outflow <- merged_df %>%
  select(month, day, net_outflow) %>%  # Select relevant columns
  distinct(month, day, net_outflow) %>%  # Select unique rows
  mutate(
    net_refugee_outflow = net_outflow  # Rename net_outflow to net_refugee_outflow
  ) %>%
  select(month, day, net_refugee_outflow)  # Select relevant columns

# Create a base dataframe with distinct values for each raion and day
merged_df_2 <- merged_df %>%
  group_by(ADM1_EN, ADM2_EN, ADM1_PCODE, ADM2_PCODE, month, day) %>%  # Group by relevant columns
  distinct(
    "ADM1_EN", "ADM2_EN", "ADM1_PCODE", "ADM2_PCODE", "month", "day"  # Select unique rows for these columns
  ) %>%
  select(
    c("ADM1_EN", "ADM2_EN", "ADM1_PCODE", "ADM2_PCODE", "month", "day")  # Select relevant columns
  ) %>%
  filter(!is.na(ADM1_EN))  # Filter out rows where ADM1_EN is NA

# Merge the dataframe with the updated total baseline device count
merged_df_3 <- left_join(merged_df_2,
                         updated_total_baseline_raion_device_count,
                         by = c("ADM1_EN", "ADM2_EN"))

# Merge the dataframe with the updated total baseline population
merged_df_4 <- left_join(merged_df_3,
                         updated_total_baseline_raion_pop,
                         by = c("ADM1_EN", "ADM2_EN"))

# Merge the dataframe with the updated total daily device counts
merged_df_5 <- left_join(
  merged_df_4,
  updated_raions_total_daily_device_counts,
  by = c("ADM1_EN", "ADM2_EN", "month", "day")
)

# Calculate the baseline penetration rate
merged_df_5$baseline_penetration_rate <- merged_df_5$median_baseline_raion_device_count / merged_df_5$total_baseline_raion_population

# Calculate the current population size
merged_df_5$current_pop_size <- merged_df_5$total_daily_count_after_invasion / merged_df_5$baseline_penetration_rate

# Calculate the total baseline population of Ukraine
merged_df_5$total_ukraine_baseline_pop <- sum(unique(merged_df_5$total_baseline_raion_population), na.rm = TRUE)

# Merge the dataframe with the updated net refugee outflow
merged_df_5 <- left_join(
  merged_df_5,
  updated_net_refugee_outflow,
  by = c("month", "day")
)

# Calculate the updated total population by month and day after invasion
updated_total_pop_by_month_and_day_after_invasion <- merged_df_5 %>%
  group_by(month, day) %>%
  summarise(total_pop_by_month_and_day_after_invasion = sum(current_pop_size, na.rm = TRUE), .groups = "drop")

# Merge the dataframe with the updated total population by month and day after invasion
merged_df_5 <- left_join(
  merged_df_5,
  updated_total_pop_by_month_and_day_after_invasion,
  by = c("month", "day")
)

# Calculate the scaling factor for population adjustment
merged_df_5$scaling_factor <- (merged_df_5$total_ukraine_baseline_pop - merged_df_5$net_refugee_outflow) / merged_df_5$total_pop_by_month_and_day_after_invasion

# Calculate the adjusted population size
merged_df_5$adjusted_pop_size <- merged_df_5$scaling_factor * merged_df_5$current_pop_size

# Calculate the net population change
merged_df_5$net_pop_change <- merged_df_5$adjusted_pop_size - merged_df_5$total_baseline_raion_population

# Calculate the proportional net change
merged_df_5$proportional_net_change <- merged_df_5$net_pop_change / merged_df_5$total_baseline_raion_population

# Define the file path to save the updated dataframe
file_to_save_to <- file.path(
  "displacement_data",
  "updated_raions.csv"
)

# Save the updated dataframe to a CSV file
write_csv(
  merged_df_5,
  file_to_save_to
)
