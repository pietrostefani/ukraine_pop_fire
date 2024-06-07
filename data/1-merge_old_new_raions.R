# Load the tidyverse package for data manipulation and visualization
library(tidyverse)
# Load the sf package for handling spatial data
library(sf)

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Specify the file path for the raions shapefile
gadm_raion_file_path <- file.path("gadm41_UKR_shp", "gadm41_UKR_2.shp")
cod_raion_file_path <- file.path("UKR_cod_shp", "ukr_admbnda_adm2_sspe_20240416.shp")

# Read the GADM raions shapefile as an 'sf' object
ukr_gadm_raions <- st_read(gadm_raion_file_path)
# Read the COD raions shapefile as an 'sf' object
ukr_cod_raions <- st_read(cod_raion_file_path)

# Calculate centroids of the GADM raions shapefile
centroids <- st_centroid(ukr_gadm_raions)
# Transform the coordinate reference system (CRS) of the GADM raions to match the COD raions
ukr_gadm_raions <- st_transform(ukr_gadm_raions, st_crs(ukr_cod_raions))

# Perform spatial join to match centroids with COD raions within the same location
centroid_matches <- st_join(centroids, ukr_cod_raions, join = st_within)

# Select relevant columns from the centroid_matches data
centroid_matches_2 <- centroid_matches %>% 
  select(NAME_1, NAME_2, ADM2_EN, ADM2_UA, ADM2_PCODE, ADM1_EN, ADM1_UA, ADM1_PCODE) 

# Merge GADM raions with the centroid_matches_2 data frame by specified columns, dropping geometry to avoid duplication
merged_data <- merge(
  ukr_gadm_raions, 
  st_drop_geometry(centroid_matches_2), 
  by=c("NAME_1", "NAME_2")
)

# Create a base plot with COD raions filled in light blue and outlined in black
base_plot <- ggplot() +
  geom_sf(data = ukr_cod_raions, fill = "lightblue", color = "black") +
  theme_minimal()

# Add centroid matches to the base plot in red, and set the plot title
centroid_plot <- base_plot +
  geom_sf(data = centroid_matches, color = "red", size = 0.5) +
  ggtitle("Old Raions Centroids on Top of Updated Raions")

# Print the final plot
print(centroid_plot)

old_new_raions <- st_drop_geometry(merged_data) %>% 
  select(NAME_1, NAME_2, ADM1_EN, ADM2_EN, ADM1_UA, ADM2_UA, ADM1_PCODE, ADM2_PCODE,)

file_to_save_to <- file.path(
  "displacement_data",
  "old_new_raions.csv"
  )

write_csv(
  old_new_raions,
  file_to_save_to
)

