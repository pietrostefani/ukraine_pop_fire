library(tidyverse)  # Load the tidyverse package, which includes dplyr and other useful packages

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define the file path for the raions CSV file
raions_file_path <- file.path("data", "displacement_data", "raions.csv")

new_raions_file_path <- file.path("data", "displacement_data", "updated_raions.csv")

# Read the raions CSV file into a dataframe
raions_df <- read_csv(raions_file_path)

new_raions_df <- read_csv(new_raions_file_path)

# Obtain the distinct month, day, and week values
weeks_df <- raions_df %>% 
  select(month, day, week, week_adjusted) %>% 
  distinct()

# Add the week values to the updated raions csv
new_raions_df_2 <- left_join(
  new_raions_df,
  weeks_df,
  by = c("month", "day")
)

# Define the file path to save the updated dataframe
file_to_save_to <- file.path(
  "data",
  "displacement_data",
  "updated_raions_2.csv"
)

# Save the updated dataframe to a CSV file
write_csv(
  new_raions_df_2,
  file_to_save_to
)
