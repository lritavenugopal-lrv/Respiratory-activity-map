

library(tidyverse)
library(readxl)
library(ggrepel)
library(qcc)
library(EpiEstim)
library(gridExtra)

levels <- read_excel("covidlevels.xlsx")



#Filter out latest week
levels <- levels %>%
  slice(-n())

levels$total <- (levels$ID_Adams + levels$ID_Canyon + levels$ID_Gem + levels$ID_Owyhee+
                   levels$ID_Payette + levels$ID_Washington)/6



#Regional baselines

canyon_center <- 2.613
canyon_sd <- 0.791

adams_center <- 1.966
adams_sd <- 0.977

gem_center <- 1.962
gem_sd <- 0.912

owyhee_center <- 2.309
owyhee_sd <- 0.804

payette_center <- 2.307
payette_sd <- 0.908

washington_center <- 1.985
washington_sd <- 0.863



# Calculate the Exponentially Weighted Moving Average (EWMA) : Canyon
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Canyon[1]
levels <- levels %>%
  mutate(ewma_c = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_c[i] <- alpha * levels$ID_Canyon[i] +
    (1 - alpha) * levels$ewma_c[i - 1]
}



# Calculate the Exponentially Weighted Moving Average (EWMA) : Adams
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Adams[1]
levels <- levels %>%
  mutate(ewma_a = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_a[i] <- alpha * levels$ID_Adams[i] +
    (1 - alpha) * levels$ewma_a[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Gem
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Gem[1]
levels <- levels %>%
  mutate(ewma_g = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_g[i] <- alpha * levels$ID_Gem[i] +
    (1 - alpha) * levels$ewma_g[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Owyhee
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Owyhee[1]
levels <- levels %>%
  mutate(ewma_o = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_o[i] <- alpha * levels$ID_Owyhee[i] +
    (1 - alpha) * levels$ewma_o[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) :Payette
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Payette[1]
levels <- levels %>%
  mutate(ewma_p = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_p[i] <- alpha * levels$ID_Payette[i] +
    (1 - alpha) * levels$ewma_p[i - 1]
}


# Calculate the Exponentially Weighted Moving Average (EWMA) : Washington
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Washington[1]
levels <- levels %>%
  mutate(ewma_w = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_w[i] <- alpha * levels$ID_Washington[i] +
    (1 - alpha) * levels$ewma_w[i - 1]
}


# Define thresholds
c_2sd <- canyon_center + (2 * canyon_sd)
c_3sd <- canyon_center + (3 * canyon_sd)
c_4sd <- canyon_center + (4 * canyon_sd)
c_5sd <- canyon_center + (5 * canyon_sd)
c_7sd <- canyon_center + (7 * canyon_sd)
c_8sd <- canyon_center + (8 * canyon_sd)
c_9sd <- canyon_center + (9 * canyon_sd)
c_10sd <- canyon_center + (10 * canyon_sd)

# Define thresholds for Adams County
adams_2sd <- adams_center + (2 * adams_sd)
adams_3sd <- adams_center + (3 * adams_sd)
adams_4sd <- adams_center + (4 * adams_sd)
adams_5sd <- adams_center + (5 * adams_sd)
adams_7sd <- adams_center + (7 * adams_sd)
adams_8sd <- adams_center + (8 * adams_sd)
adams_9sd <- adams_center + (9 * adams_sd)
adams_10sd <- adams_center + (10 * adams_sd)

# Define thresholds for Gem County
gem_2sd <- gem_center + (2 * gem_sd)
gem_3sd <- gem_center + (3 * gem_sd)
gem_4sd <- gem_center + (4 * gem_sd)
gem_5sd <- gem_center + (5 * gem_sd)
gem_7sd <- gem_center + (7 * gem_sd)
gem_8sd <- gem_center + (8 * gem_sd)
gem_9sd <- gem_center + (9 * gem_sd)
gem_10sd <- gem_center + (10 * gem_sd)

# Define thresholds for Owyhee County
owyhee_2sd <- owyhee_center + (2 * owyhee_sd)
owyhee_3sd <- owyhee_center + (3 * owyhee_sd)
owyhee_4sd <- owyhee_center + (4 * owyhee_sd)
owyhee_5sd <- owyhee_center + (5 * owyhee_sd)
owyhee_7sd <- owyhee_center + (7 * owyhee_sd)
owyhee_8sd <- owyhee_center + (8 * owyhee_sd)
owyhee_9sd <- owyhee_center + (9 * owyhee_sd)
owyhee_10sd <- owyhee_center + (10 * owyhee_sd)

# Define thresholds for Payette County
payette_2sd <- payette_center + (2 * payette_sd)
payette_3sd <- payette_center + (3 * payette_sd)
payette_4sd <- payette_center + (4 * payette_sd)
payette_5sd <- payette_center + (5 * payette_sd)
payette_7sd <- payette_center + (7 * payette_sd)
payette_8sd <- payette_center + (8 * payette_sd)
payette_9sd <- payette_center + (9 * payette_sd)
payette_10sd <- payette_center + (10 * payette_sd)

# Define thresholds for Washington County
washington_2sd <- washington_center + (2 * washington_sd)
washington_3sd <- washington_center + (3 * washington_sd)
washington_4sd <- washington_center + (4 * washington_sd)
washington_5sd <- washington_center + (5 * washington_sd)
washington_7sd <- washington_center + (7 * washington_sd)
washington_8sd <- washington_center + (8 * washington_sd)
washington_9sd <- washington_center + (9 * washington_sd)
washington_10sd <- washington_center + (10 * washington_sd)



levels <- levels %>%
  # Create Activitys for Canyon County
  mutate(
    Activity_c = case_when(
      ewma_c <= c_2sd ~ 1,                               # SD ≤ 2
      ewma_c > c_2sd & ewma_c <= c_4sd ~ 2,              # SD = 3–4
      ewma_c > c_4sd & ewma_c <= c_7sd ~ 3,              # SD = 5–7
      ewma_c > c_7sd & ewma_c <= c_9sd ~ 4,              # SD = 8–9
      ewma_c > c_9sd ~ 5                                 # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Adams County
  mutate(
    Activity_a = case_when(
      ewma_a <= adams_2sd ~ 1,                           # SD ≤ 2
      ewma_a > adams_2sd & ewma_a <= adams_4sd ~ 2,      # SD = 3–4
      ewma_a > adams_4sd & ewma_a <= adams_7sd ~ 3,      # SD = 5–7
      ewma_a > adams_7sd & ewma_a <= adams_9sd ~ 4,      # SD = 8–9
      ewma_a > adams_9sd ~ 5                             # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Gem County
  mutate(
    Activity_g = case_when(
      ewma_g <= gem_2sd ~ 1,                             # SD ≤ 2
      ewma_g > gem_2sd & ewma_g <= gem_4sd ~ 2,          # SD = 3–4
      ewma_g > gem_4sd & ewma_g <= gem_7sd ~ 3,          # SD = 5–7
      ewma_g > gem_7sd & ewma_g <= gem_9sd ~ 4,          # SD = 8–9
      ewma_g > gem_9sd ~ 5                               # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Owyhee County
  mutate(
    Activity_o = case_when(
      ewma_o <= owyhee_2sd ~ 1,                          # SD ≤ 2
      ewma_o > owyhee_2sd & ewma_o <= owyhee_4sd ~ 2,    # SD = 3–4
      ewma_o > owyhee_4sd & ewma_o <= owyhee_7sd ~ 3,    # SD = 5–7
      ewma_o > owyhee_7sd & ewma_o <= owyhee_9sd ~ 4,    # SD = 8–9
      ewma_o > owyhee_9sd ~ 5                            # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Payette County
  mutate(
    Activity_p = case_when(
      ewma_p <= payette_2sd ~ 1,                         # SD ≤ 2
      ewma_p > payette_2sd & ewma_p <= payette_4sd ~ 2,  # SD = 3–4
      ewma_p > payette_4sd & ewma_p <= payette_7sd ~ 3,  # SD = 5–7
      ewma_p > payette_7sd & ewma_p <= payette_9sd ~ 4,  # SD = 8–9
      ewma_p > payette_9sd ~ 5                           # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Washington County
  mutate(
    Activity_w = case_when(
      ewma_w <= washington_2sd ~ 1,                      # SD ≤ 2
      ewma_w > washington_2sd & ewma_w <= washington_4sd ~ 2, # SD = 3–4
      ewma_w > washington_4sd & ewma_w <= washington_7sd ~ 3, # SD = 5–7
      ewma_w > washington_7sd & ewma_w <= washington_9sd ~ 4, # SD = 8–9
      ewma_w > washington_9sd ~ 5                        # SD ≥ 10
    )
  )



# Extract the last row of the dataset
last_row <- tail(levels, 1) %>%
  select(Activity_a, Activity_c, Activity_g, Activity_o, Activity_p, Activity_w)

# Display the last row
print(last_row)

# Pivot the data to long format
Activity <- last_row %>%
  pivot_longer(cols = everything(),  # Select all columns to pivot
               names_to = "NAME",  # The new column for the county names
               values_to = "Activity_level")  # The new column for Activity levels


# Load required libraries
library(sf)        # For spatial data
library(ggplot2)   # For plotting
library(tigris)
library(viridis)
library(reshape2)
library(leaflet)
library(htmltools)

options(tigris_use_cache = TRUE)

# Replace with your actual path
shapefile_path <- "tl_2020_us_county.shp"

# Read all US counties
all_counties <- st_read(shapefile_path)

# Filter for Idaho (STATEFP == "16")
idaho_counties <- all_counties[all_counties$STATEFP == "16", ]

# Load Idaho counties shapefile
#idaho_counties <- counties(state = "16", year = 2020, class = "sf")

# Rename counties in the "county" column
Activity <- Activity %>%
  mutate(
    NAME = case_when(
      NAME == "Activity_a" ~ "Adams",
      NAME == "Activity_c" ~ "Canyon",
      NAME == "Activity_g" ~ "Gem",
      NAME == "Activity_o" ~ "Owyhee",
      NAME == "Activity_p" ~ "Payette",
      NAME == "Activity_w" ~ "Washington",
      TRUE ~ NAME  # Keep other values unchanged
    )
  )

# Join the levels data with spatial data
map_data <- left_join(idaho_counties, Activity, by = "NAME")


# Filter for specific counties
map_data <- map_data %>%
  filter(NAME %in% c("Adams", "Canyon", "Gem", "Owyhee", "Payette", "Washington"))


# Ensure `map_data` is an sf object
map_data <- st_as_sf(map_data)


# Convert `Activity_level` to a factor and explicitly define all levels
map_data <- map_data %>%
  mutate(Activity_level = factor(Activity_level, levels = 1:5, labels = c("Minimal", "Low", "Moderate", "High", "Very High")))

# Reproject the spatial data to WGS84
map_data <- st_transform(map_data, crs = 4326)  # EPSG code 4326 corresponds to WGS84


# Define Activity level colors
Activity_colors <- c(
  "Minimal" = "green4",       # Soft green
  "Low" = "gold",   # Orange
  "Moderate" = "orangered",      # Darker purple
  "High" = "red3",      # Pink
  "Very High" = "purple4"  # Dark red
)


# Create a color palette for leaflet
Activity_palette <- colorFactor(
  palette = Activity_colors,
  levels = c("Minimal", "Low", "Moderate", "High", "Very High")
)

# Add popup information
map_data <- map_data %>%
  mutate(popup_info = paste0(
    "<strong>County:</strong> ", NAME, "<br>",
    "<strong>Activity Level:</strong> ", Activity_level
  ))




# Create the interactive map - Hover option
covid_map <- leaflet(data = map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~Activity_palette(Activity_level),  # Color based on Activity level
    weight = 1,                              # Border thickness
    color = "white",                         # Border color
    fillOpacity = 0.6,                       # Transparency of polygons
    label = ~lapply(
      sprintf(
        "<strong>County:</strong> %s<br><strong>Activity Level:</strong> %s",
        NAME, Activity_level
      ), 
      htmltools::HTML
    ),           
    labelOptions = labelOptions(
      style = list("font-weight" = "bold"),  # Bold text in label
      textsize = "12px",                     # Adjust text size
      direction = "auto"                     # Auto position labels
    )
  ) %>%
  addLegend(
    position = "topleft",                # Set legend to bottom left
    pal = Activity_palette,
    values = factor(c("Minimal", "Low", "Moderate", "High", "Very High"),
                    levels = c("Minimal", "Low", "Moderate", "High", "Very High")),
    title = "<div style='text-align:center;'> Activity Level</div>",  # Center the title
    opacity = 0.6
  ) %>%
  setView(lng = -116.7, lat = 43.5, zoom = 7)  # Center and zoom level


covid_map

levels <- read_excel("flulevels.xlsx")

#Filter out latest week
levels <- levels %>%
  slice(-n())



levels$ID_total <- (levels$ID_Adams + levels$ID_Canyon + levels$ID_Gem + levels$ID_Owyhee+
                      levels$ID_Payette + levels$ID_Washington)/6




# Regional baselines
canyon_center <- 1.481
canyon_sd     <- 0.62

adams_center  <- 0.646
adams_sd      <- 0.583

gem_center    <- 0.737
gem_sd        <- 0.491

owyhee_center <- 1.285
owyhee_sd     <- 0.763

payette_center <- 0.811
payette_sd     <- 0.483

washington_center <- 0.731
washington_sd     <- 0.561



# Calculate the Exponentially Weighted Moving Average (EWMA) : Canyon
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Canyon[1]
levels <- levels %>%
  mutate(ewma_c = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_c[i] <- alpha * levels$ID_Canyon[i] +
    (1 - alpha) * levels$ewma_c[i - 1]
}



# Calculate the Exponentially Weighted Moving Average (EWMA) : Adams
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Adams[1]
levels <- levels %>%
  mutate(ewma_a = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_a[i] <- alpha * levels$ID_Adams[i] +
    (1 - alpha) * levels$ewma_a[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Gem
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Gem[1]
levels <- levels %>%
  mutate(ewma_g = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_g[i] <- alpha * levels$ID_Gem[i] +
    (1 - alpha) * levels$ewma_g[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Owyhee
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Owyhee[1]
levels <- levels %>%
  mutate(ewma_o = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_o[i] <- alpha * levels$ID_Owyhee[i] +
    (1 - alpha) * levels$ewma_o[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) :Payette
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Payette[1]
levels <- levels %>%
  mutate(ewma_p = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_p[i] <- alpha * levels$ID_Payette[i] +
    (1 - alpha) * levels$ewma_p[i - 1]
}


# Calculate the Exponentially Weighted Moving Average (EWMA) : Washington
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Washington[1]
levels <- levels %>%
  mutate(ewma_w = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_w[i] <- alpha * levels$ID_Washington[i] +
    (1 - alpha) * levels$ewma_w[i - 1]
}


# Define thresholds
c_2sd <- canyon_center + (2 * canyon_sd)
c_3sd <- canyon_center + (3 * canyon_sd)
c_4sd <- canyon_center + (4 * canyon_sd)
c_5sd <- canyon_center + (5 * canyon_sd)
c_7sd <- canyon_center + (7 * canyon_sd)
c_8sd <- canyon_center + (8 * canyon_sd)
c_9sd <- canyon_center + (9 * canyon_sd)
c_10sd <- canyon_center + (10 * canyon_sd)

# Define thresholds for Adams County
adams_2sd <- adams_center + (2 * adams_sd)
adams_3sd <- adams_center + (3 * adams_sd)
adams_4sd <- adams_center + (4 * adams_sd)
adams_5sd <- adams_center + (5 * adams_sd)
adams_7sd <- adams_center + (7 * adams_sd)
adams_8sd <- adams_center + (8 * adams_sd)
adams_9sd <- adams_center + (9 * adams_sd)
adams_10sd <- adams_center + (10 * adams_sd)

# Define thresholds for Gem County
gem_2sd <- gem_center + (2 * gem_sd)
gem_3sd <- gem_center + (3 * gem_sd)
gem_4sd <- gem_center + (4 * gem_sd)
gem_5sd <- gem_center + (5 * gem_sd)
gem_7sd <- gem_center + (7 * gem_sd)
gem_8sd <- gem_center + (8 * gem_sd)
gem_9sd <- gem_center + (9 * gem_sd)
gem_10sd <- gem_center + (10 * gem_sd)

# Define thresholds for Owyhee County
owyhee_2sd <- owyhee_center + (2 * owyhee_sd)
owyhee_3sd <- owyhee_center + (3 * owyhee_sd)
owyhee_4sd <- owyhee_center + (4 * owyhee_sd)
owyhee_5sd <- owyhee_center + (5 * owyhee_sd)
owyhee_7sd <- owyhee_center + (7 * owyhee_sd)
owyhee_8sd <- owyhee_center + (8 * owyhee_sd)
owyhee_9sd <- owyhee_center + (9 * owyhee_sd)
owyhee_10sd <- owyhee_center + (10 * owyhee_sd)

# Define thresholds for Payette County
payette_2sd <- payette_center + (2 * payette_sd)
payette_3sd <- payette_center + (3 * payette_sd)
payette_4sd <- payette_center + (4 * payette_sd)
payette_5sd <- payette_center + (5 * payette_sd)
payette_7sd <- payette_center + (7 * payette_sd)
payette_8sd <- payette_center + (8 * payette_sd)
payette_9sd <- payette_center + (9 * payette_sd)
payette_10sd <- payette_center + (10 * payette_sd)

# Define thresholds for Washington County
washington_2sd <- washington_center + (2 * washington_sd)
washington_3sd <- washington_center + (3 * washington_sd)
washington_4sd <- washington_center + (4 * washington_sd)
washington_5sd <- washington_center + (5 * washington_sd)
washington_7sd <- washington_center + (7 * washington_sd)
washington_8sd <- washington_center + (8 * washington_sd)
washington_9sd <- washington_center + (9 * washington_sd)
washington_10sd <- washington_center + (10 * washington_sd)



levels <- levels %>%
  # Create Activitys for Canyon County
  mutate(
    Activity_c = case_when(
      ewma_c <= c_2sd ~ 1,                               # SD ≤ 2
      ewma_c > c_2sd & ewma_c <= c_4sd ~ 2,              # SD = 3–4
      ewma_c > c_4sd & ewma_c <= c_7sd ~ 3,              # SD = 5–7
      ewma_c > c_7sd & ewma_c <= c_9sd ~ 4,              # SD = 8–9
      ewma_c > c_9sd ~ 5                                 # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Adams County
  mutate(
    Activity_a = case_when(
      ewma_a <= adams_2sd ~ 1,                           # SD ≤ 2
      ewma_a > adams_2sd & ewma_a <= adams_4sd ~ 2,      # SD = 3–4
      ewma_a > adams_4sd & ewma_a <= adams_7sd ~ 3,      # SD = 5–7
      ewma_a > adams_7sd & ewma_a <= adams_9sd ~ 4,      # SD = 8–9
      ewma_a > adams_9sd ~ 5                             # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Gem County
  mutate(
    Activity_g = case_when(
      ewma_g <= gem_2sd ~ 1,                             # SD ≤ 2
      ewma_g > gem_2sd & ewma_g <= gem_4sd ~ 2,          # SD = 3–4
      ewma_g > gem_4sd & ewma_g <= gem_7sd ~ 3,          # SD = 5–7
      ewma_g > gem_7sd & ewma_g <= gem_9sd ~ 4,          # SD = 8–9
      ewma_g > gem_9sd ~ 5                               # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Owyhee County
  mutate(
    Activity_o = case_when(
      ewma_o <= owyhee_2sd ~ 1,                          # SD ≤ 2
      ewma_o > owyhee_2sd & ewma_o <= owyhee_4sd ~ 2,    # SD = 3–4
      ewma_o > owyhee_4sd & ewma_o <= owyhee_7sd ~ 3,    # SD = 5–7
      ewma_o > owyhee_7sd & ewma_o <= owyhee_9sd ~ 4,    # SD = 8–9
      ewma_o > owyhee_9sd ~ 5                            # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Payette County
  mutate(
    Activity_p = case_when(
      ewma_p <= payette_2sd ~ 1,                         # SD ≤ 2
      ewma_p > payette_2sd & ewma_p <= payette_4sd ~ 2,  # SD = 3–4
      ewma_p > payette_4sd & ewma_p <= payette_7sd ~ 3,  # SD = 5–7
      ewma_p > payette_7sd & ewma_p <= payette_9sd ~ 4,  # SD = 8–9
      ewma_p > payette_9sd ~ 5                           # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Washington County
  mutate(
    Activity_w = case_when(
      ewma_w <= washington_2sd ~ 1,                      # SD ≤ 2
      ewma_w > washington_2sd & ewma_w <= washington_4sd ~ 2, # SD = 3–4
      ewma_w > washington_4sd & ewma_w <= washington_7sd ~ 3, # SD = 5–7
      ewma_w > washington_7sd & ewma_w <= washington_9sd ~ 4, # SD = 8–9
      ewma_w > washington_9sd ~ 5                        # SD ≥ 10
    )
  )



# Extract the last row of the dataset
last_row <- tail(levels, 1) %>%
  select(Activity_a, Activity_c, Activity_g, Activity_o, Activity_p, Activity_w)

# Display the last row
print(last_row)

# Pivot the data to long format
Activity <- last_row %>%
  pivot_longer(cols = everything(),  # Select all columns to pivot
               names_to = "NAME",  # The new column for the county names
               values_to = "Activity_level")  # The new column for Activity levels


# Load required libraries
library(sf)        # For spatial data
library(ggplot2)   # For plotting
library(tigris)
library(viridis)
library(reshape2)
library(leaflet)
library(htmltools)


# Load Idaho counties shapefile
#idaho_counties <- counties(state = "16", year = 2020, class = "sf")

# Rename counties in the "county" column
Activity <- Activity %>%
  mutate(
    NAME = case_when(
      NAME == "Activity_a" ~ "Adams",
      NAME == "Activity_c" ~ "Canyon",
      NAME == "Activity_g" ~ "Gem",
      NAME == "Activity_o" ~ "Owyhee",
      NAME == "Activity_p" ~ "Payette",
      NAME == "Activity_w" ~ "Washington",
      TRUE ~ NAME  # Keep other values unchanged
    )
  )

# Join the levels data with spatial data
map_data <- left_join(idaho_counties, Activity, by = "NAME")


# Filter for specific counties
map_data <- map_data %>%
  filter(NAME %in% c("Adams", "Canyon", "Gem", "Owyhee", "Payette", "Washington"))


# Ensure `map_data` is an sf object
map_data <- st_as_sf(map_data)


# Convert `Activity_level` to a factor and explicitly define all levels
map_data <- map_data %>%
  mutate(Activity_level = factor(Activity_level, levels = 1:5, labels = c("Minimal", "Low", "Moderate", "High", "Very High")))

# Reproject the spatial data to WGS84
map_data <- st_transform(map_data, crs = 4326)  # EPSG code 4326 corresponds to WGS84


# Define Activity level colors
Activity_colors <- c(
  "Minimal" = "green4",       # Soft green
  "Low" = "gold",   # Orange
  "Moderate" = "orangered",      # Darker purple
  "High" = "red3",      # Pink
  "Very High" = "purple4"  # Dark red
)

# Create a color palette for leaflet
Activity_palette <- colorFactor(
  palette = Activity_colors,
  levels = c("Minimal", "Low", "Moderate", "High", "Very High")
)

# Add popup information
map_data <- map_data %>%
  mutate(popup_info = paste0(
    "<strong>County:</strong> ", NAME, "<br>",
    "<strong>Activity Level:</strong> ", Activity_level
  ))




# Create the interactive map - Hover option
flu_map <- leaflet(data = map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~Activity_palette(Activity_level),  # Color based on Activity level
    weight = 1,                              # Border thickness
    color = "white",                         # Border color
    fillOpacity = 0.6,                       # Transparency of polygons
    label = ~lapply(
      sprintf(
        "<strong>County:</strong> %s<br><strong>Activity Level:</strong> %s",
        NAME, Activity_level
      ), 
      htmltools::HTML
    ),           
    labelOptions = labelOptions(
      style = list("font-weight" = "bold"),  # Bold text in label
      textsize = "12px",                     # Adjust text size
      direction = "auto"                     # Auto position labels
    )
  ) %>%
  addLegend(
    position = "topleft",                # Set legend to bottom left
    pal = Activity_palette,
    values = factor(c("Minimal", "Low", "Moderate", "High", "Very High"),
                    levels = c("Minimal", "Low", "Moderate", "High", "Very High")),
    title = "<div style='text-align:center;'>Activity Level</div>",  # Center the title
    opacity = 0.6
  ) %>%
  setView(lng = -116.7, lat = 43.5, zoom = 7)  # Center and zoom level


flu_map


levels <- read_excel("rsvlevels.xlsx")


#Filter out latest week
levels <- levels %>%
  slice(-n())

levels$ID_total <- (levels$ID_Adams + levels$ID_Canyon + levels$ID_Gem + levels$ID_Owyhee+
                      levels$ID_Payette + levels$ID_Washington)/6



#Regional baselines

canyon_center <- 0.044
canyon_sd <- 0.061

adams_center <- 0.010
adams_sd <- 0.027

gem_center <- 0.024
gem_sd <- 0.053

owyhee_center <- 0.023
owyhee_sd <- 0.051

payette_center <- 0.022
payette_sd <- 0.040

washington_center <- 0.016
washington_sd <- 0.041



# Calculate the Exponentially Weighted Moving Average (EWMA) : Canyon
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Canyon[1]
levels <- levels %>%
  mutate(ewma_c = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_c[i] <- alpha * levels$ID_Canyon[i] +
    (1 - alpha) * levels$ewma_c[i - 1]
}



# Calculate the Exponentially Weighted Moving Average (EWMA) : Adams
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Adams[1]
levels <- levels %>%
  mutate(ewma_a = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_a[i] <- alpha * levels$ID_Adams[i] +
    (1 - alpha) * levels$ewma_a[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Gem
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Gem[1]
levels <- levels %>%
  mutate(ewma_g = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_g[i] <- alpha * levels$ID_Gem[i] +
    (1 - alpha) * levels$ewma_g[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Owyhee
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Owyhee[1]
levels <- levels %>%
  mutate(ewma_o = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_o[i] <- alpha * levels$ID_Owyhee[i] +
    (1 - alpha) * levels$ewma_o[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) :Payette
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Payette[1]
levels <- levels %>%
  mutate(ewma_p = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_p[i] <- alpha * levels$ID_Payette[i] +
    (1 - alpha) * levels$ewma_p[i - 1]
}


# Calculate the Exponentially Weighted Moving Average (EWMA) : Washington
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Washington[1]
levels <- levels %>%
  mutate(ewma_w = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_w[i] <- alpha * levels$ID_Washington[i] +
    (1 - alpha) * levels$ewma_w[i - 1]
}


# Define thresholds
c_2sd <- canyon_center + (2 * canyon_sd)
c_3sd <- canyon_center + (3 * canyon_sd)
c_4sd <- canyon_center + (4 * canyon_sd)
c_5sd <- canyon_center + (5 * canyon_sd)
c_7sd <- canyon_center + (7 * canyon_sd)
c_8sd <- canyon_center + (8 * canyon_sd)
c_9sd <- canyon_center + (9 * canyon_sd)
c_10sd <- canyon_center + (10 * canyon_sd)

# Define thresholds for Adams County
adams_2sd <- adams_center + (2 * adams_sd)
adams_3sd <- adams_center + (3 * adams_sd)
adams_4sd <- adams_center + (4 * adams_sd)
adams_5sd <- adams_center + (5 * adams_sd)
adams_7sd <- adams_center + (7 * adams_sd)
adams_8sd <- adams_center + (8 * adams_sd)
adams_9sd <- adams_center + (9 * adams_sd)
adams_10sd <- adams_center + (10 * adams_sd)

# Define thresholds for Gem County
gem_2sd <- gem_center + (2 * gem_sd)
gem_3sd <- gem_center + (3 * gem_sd)
gem_4sd <- gem_center + (4 * gem_sd)
gem_5sd <- gem_center + (5 * gem_sd)
gem_7sd <- gem_center + (7 * gem_sd)
gem_8sd <- gem_center + (8 * gem_sd)
gem_9sd <- gem_center + (9 * gem_sd)
gem_10sd <- gem_center + (10 * gem_sd)

# Define thresholds for Owyhee County
owyhee_2sd <- owyhee_center + (2 * owyhee_sd)
owyhee_3sd <- owyhee_center + (3 * owyhee_sd)
owyhee_4sd <- owyhee_center + (4 * owyhee_sd)
owyhee_5sd <- owyhee_center + (5 * owyhee_sd)
owyhee_7sd <- owyhee_center + (7 * owyhee_sd)
owyhee_8sd <- owyhee_center + (8 * owyhee_sd)
owyhee_9sd <- owyhee_center + (9 * owyhee_sd)
owyhee_10sd <- owyhee_center + (10 * owyhee_sd)

# Define thresholds for Payette County
payette_2sd <- payette_center + (2 * payette_sd)
payette_3sd <- payette_center + (3 * payette_sd)
payette_4sd <- payette_center + (4 * payette_sd)
payette_5sd <- payette_center + (5 * payette_sd)
payette_7sd <- payette_center + (7 * payette_sd)
payette_8sd <- payette_center + (8 * payette_sd)
payette_9sd <- payette_center + (9 * payette_sd)
payette_10sd <- payette_center + (10 * payette_sd)

# Define thresholds for Washington County
washington_2sd <- washington_center + (2 * washington_sd)
washington_3sd <- washington_center + (3 * washington_sd)
washington_4sd <- washington_center + (4 * washington_sd)
washington_5sd <- washington_center + (5 * washington_sd)
washington_7sd <- washington_center + (7 * washington_sd)
washington_8sd <- washington_center + (8 * washington_sd)
washington_9sd <- washington_center + (9 * washington_sd)
washington_10sd <- washington_center + (10 * washington_sd)



levels <- levels %>%
  # Create Activitys for Canyon County
  mutate(
    Activity_c = case_when(
      ewma_c <= c_2sd ~ 1,                               # SD ≤ 2
      ewma_c > c_2sd & ewma_c <= c_4sd ~ 2,              # SD = 3–4
      ewma_c > c_4sd & ewma_c <= c_7sd ~ 3,              # SD = 5–7
      ewma_c > c_7sd & ewma_c <= c_9sd ~ 4,              # SD = 8–9
      ewma_c > c_9sd ~ 5                                 # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Adams County
  mutate(
    Activity_a = case_when(
      ewma_a <= adams_2sd ~ 1,                           # SD ≤ 2
      ewma_a > adams_2sd & ewma_a <= adams_4sd ~ 2,      # SD = 3–4
      ewma_a > adams_4sd & ewma_a <= adams_7sd ~ 3,      # SD = 5–7
      ewma_a > adams_7sd & ewma_a <= adams_9sd ~ 4,      # SD = 8–9
      ewma_a > adams_9sd ~ 5                             # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Gem County
  mutate(
    Activity_g = case_when(
      ewma_g <= gem_2sd ~ 1,                             # SD ≤ 2
      ewma_g > gem_2sd & ewma_g <= gem_4sd ~ 2,          # SD = 3–4
      ewma_g > gem_4sd & ewma_g <= gem_7sd ~ 3,          # SD = 5–7
      ewma_g > gem_7sd & ewma_g <= gem_9sd ~ 4,          # SD = 8–9
      ewma_g > gem_9sd ~ 5                               # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Owyhee County
  mutate(
    Activity_o = case_when(
      ewma_o <= owyhee_2sd ~ 1,                          # SD ≤ 2
      ewma_o > owyhee_2sd & ewma_o <= owyhee_4sd ~ 2,    # SD = 3–4
      ewma_o > owyhee_4sd & ewma_o <= owyhee_7sd ~ 3,    # SD = 5–7
      ewma_o > owyhee_7sd & ewma_o <= owyhee_9sd ~ 4,    # SD = 8–9
      ewma_o > owyhee_9sd ~ 5                            # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Payette County
  mutate(
    Activity_p = case_when(
      ewma_p <= payette_2sd ~ 1,                         # SD ≤ 2
      ewma_p > payette_2sd & ewma_p <= payette_4sd ~ 2,  # SD = 3–4
      ewma_p > payette_4sd & ewma_p <= payette_7sd ~ 3,  # SD = 5–7
      ewma_p > payette_7sd & ewma_p <= payette_9sd ~ 4,  # SD = 8–9
      ewma_p > payette_9sd ~ 5                           # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Washington County
  mutate(
    Activity_w = case_when(
      ewma_w <= washington_2sd ~ 1,                      # SD ≤ 2
      ewma_w > washington_2sd & ewma_w <= washington_4sd ~ 2, # SD = 3–4
      ewma_w > washington_4sd & ewma_w <= washington_7sd ~ 3, # SD = 5–7
      ewma_w > washington_7sd & ewma_w <= washington_9sd ~ 4, # SD = 8–9
      ewma_w > washington_9sd ~ 5                        # SD ≥ 10
    )
  )



# Extract the last row of the dataset
last_row <- tail(levels, 1) %>%
  select(Activity_a, Activity_c, Activity_g, Activity_o, Activity_p, Activity_w)

# Display the last row
print(last_row)

# Pivot the data to long format
Activity <- last_row %>%
  pivot_longer(cols = everything(),  # Select all columns to pivot
               names_to = "NAME",  # The new column for the county names
               values_to = "Activity_level")  # The new column for Activity levels


# Load required libraries
library(sf)        # For spatial data
library(ggplot2)   # For plotting
library(tigris)
library(viridis)
library(reshape2)
library(leaflet)
library(htmltools)


# Load Idaho counties shapefile
#idaho_counties <- counties(state = "16", year = 2020, class = "sf")

# Rename counties in the "county" column
Activity <- Activity %>%
  mutate(
    NAME = case_when(
      NAME == "Activity_a" ~ "Adams",
      NAME == "Activity_c" ~ "Canyon",
      NAME == "Activity_g" ~ "Gem",
      NAME == "Activity_o" ~ "Owyhee",
      NAME == "Activity_p" ~ "Payette",
      NAME == "Activity_w" ~ "Washington",
      TRUE ~ NAME  # Keep other values unchanged
    )
  )

# Join the levels data with spatial data
map_data <- left_join(idaho_counties, Activity, by = "NAME")


# Filter for specific counties
map_data <- map_data %>%
  filter(NAME %in% c("Adams", "Canyon", "Gem", "Owyhee", "Payette", "Washington"))


# Ensure `map_data` is an sf object
map_data <- st_as_sf(map_data)


# Convert `Activity_level` to a factor and explicitly define all levels
map_data <- map_data %>%
  mutate(Activity_level = factor(Activity_level, levels = 1:5, labels = c("Minimal", "Low", "Moderate", "High", "Very High")))

# Reproject the spatial data to WGS84
map_data <- st_transform(map_data, crs = 4326)  # EPSG code 4326 corresponds to WGS84

# Define Activity level colors
Activity_colors <- c(
  "Minimal" = "green4",       # Soft green
  "Low" = "gold",   # Orange
  "Moderate" = "orangered",      # Darker purple
  "High" = "red3",      # Pink
  "Very High" = "purple4"  # Dark red
)


# Create a color palette for leaflet
Activity_palette <- colorFactor(
  palette = Activity_colors,
  levels = c("Minimal", "Low", "Moderate", "High", "Very High")
)

# Add popup information
map_data <- map_data %>%
  mutate(popup_info = paste0(
    "<strong>County:</strong> ", NAME, "<br>",
    "<strong>Activity Level:</strong> ", Activity_level
  ))




# Create the interactive map - Hover option
rsv_map <- leaflet(data = map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~Activity_palette(Activity_level),  # Color based on Activity level
    weight = 1,                              # Border thickness
    color = "white",                         # Border color
    fillOpacity = 0.6,                       # Transparency of polygons
    label = ~lapply(
      sprintf(
        "<strong>County:</strong> %s<br><strong>Activity Level:</strong> %s",
        NAME, Activity_level
      ), 
      htmltools::HTML
    ),           
    labelOptions = labelOptions(
      style = list("font-weight" = "bold"),  # Bold text in label
      textsize = "12px",                     # Adjust text size
      direction = "auto"                     # Auto position labels
    )
  ) %>%
  addLegend(
    position = "topleft",                # Set legend to bottom left
    pal = Activity_palette,
    values = factor(c("Minimal", "Low", "Moderate", "High", "Very High"),
                    levels = c("Minimal", "Low", "Moderate", "High", "Very High")),
    title = "<div style='text-align:center;'>Activity Level</div>",  # Center the title
    opacity = 0.6
  ) %>%
  setView(lng = -116.7, lat = 43.5, zoom = 7)  # Center and zoom level


rsv_map


levels <- read_excel("pertussislevels.xlsx")

canyon_center <- 0.157
canyon_sd <- 0.202

adams_center <- 0.000
adams_sd <- 0.001

gem_center <- 0.000
gem_sd <- 0.002

owyhee_center <- 0.001
owyhee_sd <- 0.004

payette_center <- 0.001
payette_sd <- 0.004

washington_center <- 0.002
washington_sd <- 0.006

# Calculate the Exponentially Weighted Moving Average (EWMA) : Canyon
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Canyon[1]
levels <- levels %>%
  mutate(ewma_c = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_c[i] <- alpha * levels$ID_Canyon[i] +
    (1 - alpha) * levels$ewma_c[i - 1]
}



# Calculate the Exponentially Weighted Moving Average (EWMA) : Adams
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Adams[1]
levels <- levels %>%
  mutate(ewma_a = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_a[i] <- alpha * levels$ID_Adams[i] +
    (1 - alpha) * levels$ewma_a[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Gem
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Gem[1]
levels <- levels %>%
  mutate(ewma_g = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_g[i] <- alpha * levels$ID_Gem[i] +
    (1 - alpha) * levels$ewma_g[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) : Owyhee
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Owyhee[1]
levels <- levels %>%
  mutate(ewma_o = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_o[i] <- alpha * levels$ID_Owyhee[i] +
    (1 - alpha) * levels$ewma_o[i - 1]
}

# Calculate the Exponentially Weighted Moving Average (EWMA) :Payette
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Payette[1]
levels <- levels %>%
  mutate(ewma_p = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_p[i] <- alpha * levels$ID_Payette[i] +
    (1 - alpha) * levels$ewma_p[i - 1]
}


# Calculate the Exponentially Weighted Moving Average (EWMA) : Washington
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- levels$ID_Washington[1]
levels <- levels %>%
  mutate(ewma_w = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(levels)) {
  levels$ewma_w[i] <- alpha * levels$ID_Washington[i] +
    (1 - alpha) * levels$ewma_w[i - 1]
}


# Define thresholds
c_2sd <- canyon_center + (2 * canyon_sd)
c_3sd <- canyon_center + (3 * canyon_sd)
c_4sd <- canyon_center + (4 * canyon_sd)
c_5sd <- canyon_center + (5 * canyon_sd)
c_7sd <- canyon_center + (7 * canyon_sd)
c_8sd <- canyon_center + (8 * canyon_sd)
c_9sd <- canyon_center + (9 * canyon_sd)
c_10sd <- canyon_center + (10 * canyon_sd)

# Define thresholds for Adams County
adams_2sd <- adams_center + (2 * adams_sd)
adams_3sd <- adams_center + (3 * adams_sd)
adams_4sd <- adams_center + (4 * adams_sd)
adams_5sd <- adams_center + (5 * adams_sd)
adams_7sd <- adams_center + (7 * adams_sd)
adams_8sd <- adams_center + (8 * adams_sd)
adams_9sd <- adams_center + (9 * adams_sd)
adams_10sd <- adams_center + (10 * adams_sd)

# Define thresholds for Gem County
gem_2sd <- gem_center + (2 * gem_sd)
gem_3sd <- gem_center + (3 * gem_sd)
gem_4sd <- gem_center + (4 * gem_sd)
gem_5sd <- gem_center + (5 * gem_sd)
gem_7sd <- gem_center + (7 * gem_sd)
gem_8sd <- gem_center + (8 * gem_sd)
gem_9sd <- gem_center + (9 * gem_sd)
gem_10sd <- gem_center + (10 * gem_sd)

# Define thresholds for Owyhee County
owyhee_2sd <- owyhee_center + (2 * owyhee_sd)
owyhee_3sd <- owyhee_center + (3 * owyhee_sd)
owyhee_4sd <- owyhee_center + (4 * owyhee_sd)
owyhee_5sd <- owyhee_center + (5 * owyhee_sd)
owyhee_7sd <- owyhee_center + (7 * owyhee_sd)
owyhee_8sd <- owyhee_center + (8 * owyhee_sd)
owyhee_9sd <- owyhee_center + (9 * owyhee_sd)
owyhee_10sd <- owyhee_center + (10 * owyhee_sd)

# Define thresholds for Payette County
payette_2sd <- payette_center + (2 * payette_sd)
payette_3sd <- payette_center + (3 * payette_sd)
payette_4sd <- payette_center + (4 * payette_sd)
payette_5sd <- payette_center + (5 * payette_sd)
payette_7sd <- payette_center + (7 * payette_sd)
payette_8sd <- payette_center + (8 * payette_sd)
payette_9sd <- payette_center + (9 * payette_sd)
payette_10sd <- payette_center + (10 * payette_sd)

# Define thresholds for Washington County
washington_2sd <- washington_center + (2 * washington_sd)
washington_3sd <- washington_center + (3 * washington_sd)
washington_4sd <- washington_center + (4 * washington_sd)
washington_5sd <- washington_center + (5 * washington_sd)
washington_7sd <- washington_center + (7 * washington_sd)
washington_8sd <- washington_center + (8 * washington_sd)
washington_9sd <- washington_center + (9 * washington_sd)
washington_10sd <- washington_center + (10 * washington_sd)



levels <- levels %>%
  # Create Activitys for Canyon County
  mutate(
    Activity_c = case_when(
      ewma_c <= c_2sd ~ 1,                               # SD ≤ 2
      ewma_c > c_2sd & ewma_c <= c_4sd ~ 2,              # SD = 3–4
      ewma_c > c_4sd & ewma_c <= c_7sd ~ 3,              # SD = 5–7
      ewma_c > c_7sd & ewma_c <= c_9sd ~ 4,              # SD = 8–9
      ewma_c > c_9sd ~ 5                                 # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Adams County
  mutate(
    Activity_a = case_when(
      ewma_a <= adams_2sd ~ 1,                           # SD ≤ 2
      ewma_a > adams_2sd & ewma_a <= adams_4sd ~ 2,      # SD = 3–4
      ewma_a > adams_4sd & ewma_a <= adams_7sd ~ 3,      # SD = 5–7
      ewma_a > adams_7sd & ewma_a <= adams_9sd ~ 4,      # SD = 8–9
      ewma_a > adams_9sd ~ 5                             # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Gem County
  mutate(
    Activity_g = case_when(
      ewma_g <= gem_2sd ~ 1,                             # SD ≤ 2
      ewma_g > gem_2sd & ewma_g <= gem_4sd ~ 2,          # SD = 3–4
      ewma_g > gem_4sd & ewma_g <= gem_7sd ~ 3,          # SD = 5–7
      ewma_g > gem_7sd & ewma_g <= gem_9sd ~ 4,          # SD = 8–9
      ewma_g > gem_9sd ~ 5                               # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Owyhee County
  mutate(
    Activity_o = case_when(
      ewma_o <= owyhee_2sd ~ 1,                          # SD ≤ 2
      ewma_o > owyhee_2sd & ewma_o <= owyhee_4sd ~ 2,    # SD = 3–4
      ewma_o > owyhee_4sd & ewma_o <= owyhee_7sd ~ 3,    # SD = 5–7
      ewma_o > owyhee_7sd & ewma_o <= owyhee_9sd ~ 4,    # SD = 8–9
      ewma_o > owyhee_9sd ~ 5                            # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Payette County
  mutate(
    Activity_p = case_when(
      ewma_p <= payette_2sd ~ 1,                         # SD ≤ 2
      ewma_p > payette_2sd & ewma_p <= payette_4sd ~ 2,  # SD = 3–4
      ewma_p > payette_4sd & ewma_p <= payette_7sd ~ 3,  # SD = 5–7
      ewma_p > payette_7sd & ewma_p <= payette_9sd ~ 4,  # SD = 8–9
      ewma_p > payette_9sd ~ 5                           # SD ≥ 10
    )
  ) %>%
  
  # Create Activitys for Washington County
  mutate(
    Activity_w = case_when(
      ewma_w <= washington_2sd ~ 1,                      # SD ≤ 2
      ewma_w > washington_2sd & ewma_w <= washington_4sd ~ 2, # SD = 3–4
      ewma_w > washington_4sd & ewma_w <= washington_7sd ~ 3, # SD = 5–7
      ewma_w > washington_7sd & ewma_w <= washington_9sd ~ 4, # SD = 8–9
      ewma_w > washington_9sd ~ 5                        # SD ≥ 10
    )
  )



# Extract the last row of the dataset
last_row <- tail(levels, 1) %>%
  select(Activity_a, Activity_c, Activity_g, Activity_o, Activity_p, Activity_w)

# Display the last row
print(last_row)

# Pivot the data to long format
Activity <- last_row %>%
  pivot_longer(cols = everything(),  # Select all columns to pivot
               names_to = "NAME",  # The new column for the county names
               values_to = "Activity_level")  # The new column for Activity levels


# Load required libraries
library(sf)        # For spatial data
library(ggplot2)   # For plotting
library(tigris)
library(viridis)
library(reshape2)
library(leaflet)
library(htmltools)

options(tigris_use_cache = TRUE)

# Replace with your actual path
shapefile_path <- "tl_2020_us_county.shp"

# Read all US counties
all_counties <- st_read(shapefile_path)

# Filter for Idaho (STATEFP == "16")
idaho_counties <- all_counties[all_counties$STATEFP == "16", ]

# Load Idaho counties shapefile
#idaho_counties <- counties(state = "16", year = 2020, class = "sf")

# Rename counties in the "county" column
Activity <- Activity %>%
  mutate(
    NAME = case_when(
      NAME == "Activity_a" ~ "Adams",
      NAME == "Activity_c" ~ "Canyon",
      NAME == "Activity_g" ~ "Gem",
      NAME == "Activity_o" ~ "Owyhee",
      NAME == "Activity_p" ~ "Payette",
      NAME == "Activity_w" ~ "Washington",
      TRUE ~ NAME  # Keep other values unchanged
    )
  )

# Join the levels data with spatial data
map_data <- left_join(idaho_counties, Activity, by = "NAME")


# Filter for specific counties
map_data <- map_data %>%
  filter(NAME %in% c("Adams", "Canyon", "Gem", "Owyhee", "Payette", "Washington"))


# Ensure `map_data` is an sf object
map_data <- st_as_sf(map_data)


# Convert `Activity_level` to a factor and explicitly define all levels
map_data <- map_data %>%
  mutate(Activity_level = factor(Activity_level, levels = 1:5, labels = c("Minimal", "Low", "Moderate", "High", "Very High")))

# Reproject the spatial data to WGS84
map_data <- st_transform(map_data, crs = 4326)  # EPSG code 4326 corresponds to WGS84


# Define Activity level colors
Activity_colors <- c(
  "Minimal" = "green4",       # Soft green
  "Low" = "gold",   # Orange
  "Moderate" = "orangered",      # Darker purple
  "High" = "red3",      # Pink
  "Very High" = "purple4"  # Dark red
)


# Create a color palette for leaflet
Activity_palette <- colorFactor(
  palette = Activity_colors,
  levels = c("Minimal", "Low", "Moderate", "High", "Very High")
)

# Add popup information
map_data <- map_data %>%
  mutate(popup_info = paste0(
    "<strong>County:</strong> ", NAME, "<br>",
    "<strong>Activity Level:</strong> ", Activity_level
  ))




# Create the interactive map - Hover option
pertussis_map <- leaflet(data = map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~Activity_palette(Activity_level),  # Color based on Activity level
    weight = 1,                              # Border thickness
    color = "white",                         # Border color
    fillOpacity = 0.6,                       # Transparency of polygons
    label = ~lapply(
      sprintf(
        "<strong>County:</strong> %s<br><strong>Activity Level:</strong> %s",
        NAME, Activity_level
      ), 
      htmltools::HTML
    ),           
    labelOptions = labelOptions(
      style = list("font-weight" = "bold"),  # Bold text in label
      textsize = "12px",                     # Adjust text size
      direction = "auto"                     # Auto position labels
    )
  ) %>%
  addLegend(
    position = "topleft",                # Set legend to bottom left
    pal = Activity_palette,
    values = factor(c("Minimal", "Low", "Moderate", "High", "Very High"),
                    levels = c("Minimal", "Low", "Moderate", "High", "Very High")),
    title = "<div style='text-align:center;'> Activity Level</div>",  # Center the title
    opacity = 0.6
  ) %>%
  setView(lng = -116.7, lat = 43.5, zoom = 7)  # Center and zoom level


pertussis_map

library(manipulate)
library(shiny)
library(leaflet)
library(bslib)   # for accordion

ui <- page_fluid(
  theme = bs_theme(version = 5),  # Bootstrap 5 required for accordion
  
  # ---- Force fixed scaling + scroll ----
  tags$head(
    # prevent auto-zooming on small screens
    tags$meta(name = "viewport", content = "width=1200, initial-scale=1.0, maximum-scale=1.0"),
    
    # add scrollbars when screen is smaller
    tags$style(HTML("
      body { 
        overflow-x: auto;   /* horizontal scroll if too narrow */
        overflow-y: auto;   /* keep vertical scroll */
      }
      .container-fluid {
        min-width: 1200px;  /* lock layout width */
      }
    "))
  ),
  
  # ---- Header Row ----
  fluidRow(
    column(
      width = 2,
      div(
        style = "text-align: left; margin-top: 20px;",
        tags$img(
          src = "org_emblem.png",
          height = "60px",
          style = "opacity:0.9;"
        )
      )
    ),
    column(
      width = 8,
      div(
        style = "text-align: center;",
        div(
          style = "font-size: 28px; font-weight: bold; margin-bottom: 5px;",
          "Respiratory Illness Levels and What You Can Do"
        ),
        div(
          style = "font-size: 14px; margin-bottom: 20px;",
          strong("Data for the week ending 11/08/25")
        )
      )
    ),
    column(width = 2)
  ),
  
  # ---- Main Layout ----
  fluidRow(
    column(
      width = 5,
      
      # ---- Static Radio Buttons (above map) ----
      div(
        style = "background: white; padding: 10px; margin-bottom: 10px;
                 border-radius: 8px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2);
                 font-size: 12.5px;width:450px;height:150px;line-height: 1.1;",
        radioButtons(
          inputId = "map_choice",
          label = "Select a map:",
          choices = list(
            "Influenza (Flu)" = "flu", 
            "Pertussis (Whooping Cough)" = "pertussis",
            "Respiratory Syncitial Virus (RSV)" = "rsv",
            "SARS-CoV2 (Covid-19)" = "covid"
          ),
          selected = "flu"
        )
      ),
      
      # ---- Map ----
      div(
        style = "position:relative;",
        leafletOutput("map_display", height = 460)
      ),
      
      # ---- Static Info Panel (below map) ----
      div(
        style = "background: white; padding: 15px; margin-top: 10px;
                 border-radius: 8px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2);
                 font-size: 11px; line-height: 1.3;",
        HTML("
          <strong>HOW ARE THE ACTIVITY LEVELS DETERMINED</strong><br>
          &bull; Flu, Covid and RSV activity levels are based on the number of people who go to emergency rooms in our area because they have a respiratory illness. Pertussis levels are calculated based on the number of cases reported to the health district. We compare that number to the baseline, when not many people are sick due to respiratory illnesses.<br>
          &bull; The metric used to determine activity levels is based on standard deviation (SD), which shows how much the data values vary from the baseline. The higher the SD, the greater the risk of exposure and illness.<br>
          &nbsp;&nbsp;o <strong>Minimal:</strong> Activity is within 2 SD above the baseline.<br>
          &nbsp;&nbsp;o <strong>Low:</strong> Activity is between 2 and 5 SD above the baseline.<br>
          &nbsp;&nbsp;o <strong>Moderate:</strong> Activity is between 5 and 8 SD above the baseline.<br>
          &nbsp;&nbsp;o <strong>High:</strong> Activity is between 8 and 10 SD above the baseline.<br>
          &nbsp;&nbsp;o <strong>Very High:</strong> Activity is greater than 10 SD above the baseline.
        ")
      )
    ),
    
    column(
      width = 7,
      tags$div(
        style = "text-align:center; background:white; padding:10px; border-radius:8px; 
                 box-shadow: 2px 2px 6px rgba(0,0,0,0.2);",
        tags$img(
          src = "Activity_Levels_Table_1.png",
          style = "max-width:100%; height:auto; border:none;"
        )
      ),
      
      # ---- Accordion Section ----
      accordion(
        id = "info_panels",
        multiple = TRUE,
        open = character(0),  # ensures all panels are collapsed
        
        accordion_panel(
          "STEPS YOU CAN TAKE FOR CLEANER AIR",
          tags$ul(
            tags$li("Bring as much fresh air into your home as possible by opening doors and windows and/or using exhaust fans."),
            tags$li("If your home has central heating, ventilation, and air conditioning system (HVAC) that has a filter, set the fan to 'on' when you have visitors and use appropriate filters."),
            tags$li("You may use a portable high-efficiency particulate air (HEPA) cleaner."),
            tags$li("Move activities outdoors when the weather permits.")
          )
        ),
        
        accordion_panel(
          "HANDWASHING: HOW IT WORKS",
          tagList(
            p("Washing your hands is easy, and it’s one of the most effective ways to prevent the spread of germs. Follow these five steps every time."),
            tags$ul(
              tags$li("Wet your hands with clean, running water (warm or cold), turn off the tap, and apply soap."),
              tags$li("Lather your hands by rubbing them together with the soap."),
              tags$li("Scrub your hands for at least 20 seconds."),
              tags$li("Rinse your hands well under clean, running water."),
              tags$li("Dry your hands using a clean towel or an air dryer.")
            ),
            p("Use hand sanitizer when you can't use soap and water:"),
            tags$ul(
              tags$li("Washing hands with soap and water is the best way to get rid of germs in most situations. If soap and water are not readily available, you can use an alcohol-based hand sanitizer that contains at least 60% alcohol. You can tell if the sanitizer contains at least 60% alcohol by looking at the product label.")
            )
          )
        ),
        
        accordion_panel(
          "WHO IS AT HIGHER RISK FOR SEVERE ILLNESS FROM RESPIRATORY INFECTIONS?",
          tagList(
            p("If you or someone around you has one or more risk factors for severe illness, using prevention strategies is especially important. Individuals who are at high risk for severe illness include: "),
            tags$ul(
              tags$li("Older adults"),
              tags$li("Young children"),
              tags$li("People with weakened immune systems"),
              tags$li("People with disabilities"),
              tags$li("Pregnant women")
            )
          )
        ),
        
        accordion_panel(
          "WHEN YOU MAY HAVE A RESPIRATORY ILLNESS...",
          tagList(
            p("Stay home and away from others if you have respiratory virus symptoms.These symptoms can include fever, chills, fatigue, cough, runny nose, and headache, among others. Other symptoms may include but are not limited to chest discomfort, chills, cough, decrease in appetite, diarrhea, fatigue (tiredness), fever or feeling feverish, headache, muscle or body aches, new loss of taste or smell, runny or stuffy nose, sneezing, sore throat, vomiting, weakness, wheezing. "),
            p("You can go back to normal activities when, for at least 24 hours:"),
            tags$ul(
              tags$li("Your symptoms are improving."),
              tags$li("You have not had a fever without medication.")
            )
          )
        )
      )
    )
  )
)

input_task_button("close_panels", "Close All Panels")

server <- function(input, output) {
  output$map_display <- renderLeaflet({
    if (input$map_choice == "flu") {
      flu_map
    } else if (input$map_choice == "pertussis") {
      pertussis_map
    } else if (input$map_choice == "rsv") {
      rsv_map
    } else {
      covid_map
    }
  })
  
  observe({
    if (!is.null(input$info_panels)) {
      accordion_panel_close(id = "info_panels", values = input$info_panels)
    }
  }) |> bindEvent(input$close_panels)
}

shinyApp(ui = ui, server = server)


