


setwd("C:/Users/lekshmi.venugopal/OneDrive - Southwest District Health/Documents/Respiratory activity map")

library(tidyverse)
library(mgcv)
library(lubridate)
library(ISOweek)
library(readxl)
library(sf)
library(ggplot2)

#-----------------------------
# Load and filter data
#-----------------------------
levels <- read_excel("flulevels.xlsx")


# Convert year-week to Monday of the ISO week and filter for 2025+
levels_filtered <- levels %>%
  select(date, ID_Adams, ID_Canyon, ID_Owyhee, ID_Payette, ID_Gem, ID_Washington) %>%
  mutate(
    date = ISOweek2date(str_replace(date, "(\\d{4})-(\\d{2})", "\\1-W\\2-1"))
  ) %>%
  filter(year(date) >= 2023)

# Pivot longer
data_long <- levels_filtered %>%
  pivot_longer(
    cols = starts_with("ID_"),
    names_to = "county",
    values_to = "count"
  ) %>%
  mutate(
    county = str_replace(county, "ID_", ""),
    week_index = as.numeric(difftime(date, min(date), units = "weeks"))
  )



#-----------------------------
# Fit GAM and compute trend derivatives
#-----------------------------
trend_flags_weekly <- data_long %>%
  group_by(county) %>%
  group_modify(~{
    model <- gam(count ~ s(week_index, k = 20), data = ., family = nb())
    
    # Prediction on link scale
    week_seq <- seq(min(.$week_index), max(.$week_index), by = 1)
    Xp <- predict(model, newdata = tibble(week_index = week_seq), type = "lpmatrix")
    fit_link <- Xp %*% coef(model)
    
    # Finite difference derivative
    deriv_link <- c(NA, diff(fit_link))
    
    # Approximate standard error
    Vb <- vcov(model)
    se_link <- sqrt(rowSums((Xp %*% Vb) * Xp))
    se_deriv <- c(NA, sqrt(diff(se_link^2)))
    
    # z-score and p-value
    z <- deriv_link / se_deriv
    pval <- 2 * (1 - pnorm(abs(z)))
    
    tibble(
      week_index = week_seq,
      date = min(.$date) + weeks(week_seq),
      derivative = deriv_link,
      se_derivative = se_deriv,
      p_value = pval,
      trend = case_when(
        !is.na(deriv_link) & deriv_link > 0 & pval < 0.2 ~ "Significantly Increasing",
        !is.na(deriv_link) & deriv_link < 0 & pval < 0.2 ~ "Significantly Decreasing",
        TRUE ~ "Stable"
      )
    )
  }) %>%
  ungroup

# Extract last week's data for the six counties
latest_week_data <- trend_flags_weekly %>%
  filter(date == max(date)) 

 #--------------------------------------------------------------------#

#plotting map########################################################
#-------------------------------------------------------------------#

options(tigris_use_cache = TRUE)

# Replace with your actual path
shapefile_path <- "tl_2020_us_county.shp"

# Read all US counties
all_counties <- st_read(shapefile_path)

# Filter for Idaho (STATEFP == "16")
idaho_counties <- all_counties[all_counties$STATEFP == "16", ]


latest_week_data <- latest_week_data %>%
   rename(NAME = county)

# Join the levels data with spatial data
map_data <- left_join(idaho_counties, latest_week_data, by = "NAME")


# Filter for specific counties
map_data <- map_data %>%
  filter(NAME %in% c("Adams", "Canyon", "Gem", "Owyhee", "Payette", "Washington"))

# Ensure `map_data` is an sf object
map_data <- st_as_sf(map_data)


# Plot the map
ggplot(map_data) +
  geom_sf(aes(fill = trend)) +
  scale_fill_manual(
    values = c(
      "Significantly Increasing" = "red",
      "Significantly Decreasing" = "blue",
      "Stable" = "yellow"
    )
  ) +
  labs(
    title = "COVID Trend by County (Latest Week)",
    fill = "Trend"
  ) +
  theme_minimal()


#-----------------------------
# Aggregate counts across counties
#-----------------------------

data_total <- levels_filtered %>%
  mutate(
    total_count = rowSums(select(., starts_with("ID_")), na.rm = TRUE),
    week_index = as.numeric(difftime(date, min(date), units = "weeks"))
  ) %>%
  select(date, week_index, total_count)

#-----------------------------
# Fit GAM and compute trend derivatives for total
#-----------------------------
model_total <- gam(total_count ~ s(week_index, k = 20), data = data_total, family = nb())

week_seq <- seq(min(data_total$week_index), max(data_total$week_index), by = 1)
Xp <- predict(model_total, newdata = tibble(week_index = week_seq), type = "lpmatrix")
fit_link <- Xp %*% coef(model_total)

deriv_link <- c(NA, diff(fit_link))
Vb <- vcov(model_total)
se_link <- sqrt(rowSums((Xp %*% Vb) * Xp))
se_deriv <- c(NA, sqrt(diff(se_link^2)))

z <- deriv_link / se_deriv
pval <- 2 * (1 - pnorm(abs(z)))

trend_flags_total <- tibble(
  week_index = week_seq,
  date = min(data_total$date) + weeks(week_seq),
  derivative = deriv_link,
  se_derivative = se_deriv,
  p_value = pval,
  trend = case_when(
    !is.na(deriv_link) & deriv_link > 0 & pval < 0.2 ~ "Significantly Increasing",
    !is.na(deriv_link) & deriv_link < 0 & pval < 0.2 ~ "Significantly Decreasing",
    TRUE ~ "Stable"
  )
)

#-----------------------------
# GAM fit line for plotting
#-----------------------------
gam_trend_total <- tibble(
  date = min(data_total$date) + weeks(week_seq),
  fit = predict(model_total, newdata = tibble(week_index = week_seq), type = "response")
)

#-----------------------------
# Plot for total counts
#-----------------------------
ggplot() +
  geom_line(data = gam_trend_total, aes(x = date, y = fit), size = 1) +
  geom_rect(
    data = trend_flags_total,
    aes(
      xmin = date - 3.5,
      xmax = date + 3.5,
      ymin = -0.02 * max(data_total$total_count),
      ymax = 0,
      fill = trend
    ),
    inherit.aes = FALSE,
    alpha = 1
  ) +
  scale_fill_manual(
    values = c(
      "Significantly Increasing" = "red",
      "Significantly Decreasing" = "blue",
      "Stable" = "yellow"
    )
  ) +
  labs(
    title = "Total COVID Counts with GAM Trend and Weekly Trend Indicator (2025+)",
    x = "Week",
    y = "Total Count"
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off")

#-----------------------------
# GAM fit line for plotting
#-----------------------------
gam_trends_plot <- data_long %>%
  group_by(county) %>%
  group_modify(~{
    model <- gam(count ~ s(week_index, k = 20), data = ., family = nb())
    week_seq <- seq(min(.$week_index), max(.$week_index), by = 1)
    tibble(
      date = min(.$date) + weeks(week_seq),
      fit = predict(model, newdata = tibble(week_index = week_seq), type = "response"),
      county = unique(.$county)
    )
  }) %>% ungroup()

#-----------------------------
# Plot with solid, thin trend indicator bar
#-----------------------------
ggplot() +
  # Observed points
  #geom_point(data = data_long, aes(x = date, y = count), alpha = 0.4) +
  # GAM trend line
  geom_line(data = gam_trends_plot, aes(x = date, y = fit), size = 1) +
  # Solid, thin trend indicator bar
  geom_rect(
    data = trend_flags_weekly,
    aes(
      xmin = date - 3.5,  # week width
      xmax = date + 3.5,
      ymin = -0.02 * max(data_long$count),  # thin bar
      ymax = 0,
      fill = trend
    ),
    inherit.aes = FALSE,
    alpha = 1
  ) +
  facet_wrap(~county, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Significantly Increasing" = "red",
      "Significantly Decreasing" = "blue",
      "Stable" = "yellow"
    )
  ) +
  labs(
    title = "COVID Counts with GAM Trend and Weekly Trend Indicator (2025+)",
    x = "Week",
    y = "Count"
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off")

#-----------------------------
# Compute GAM trend without p-value
#-----------------------------
trend_flags_weekly_simple <- data_long %>%
  group_by(county) %>%
  group_modify(~{
    model <- gam(count ~ s(week_index, k = 20), data = ., family = nb())
    
    week_seq <- seq(min(.$week_index), max(.$week_index), by = 1)
    
    # Predicted values on response scale
    fit <- predict(model, newdata = tibble(week_index = week_seq), type = "response")
    
    # Compute simple derivative: diff of predicted counts
    deriv <- c(NA, diff(fit))
    
    tibble(
      week_index = week_seq,
      date = min(.$date) + weeks(week_seq),
      derivative = deriv,
      trend = case_when(
        deriv > 0 ~ "Increasing",
        deriv < 0 ~ "Decreasing",
        TRUE ~ "Stable"
      )
    )
  }) %>%
  ungroup()

#-----------------------------
# Plot with simplified trend indicator bar
#-----------------------------
ggplot() +
  geom_point(data = data_long, aes(x = date, y = count), alpha = 0.4) +
  geom_line(data = gam_trends_plot, aes(x = date, y = fit, color = county), size = 1) +
  geom_rect(
    data = trend_flags_weekly_simple,
    aes(
      xmin = date - 3.5,
      xmax = date + 3.5,
      ymin = -0.02 * max(data_long$count),
      ymax = 0,
      fill = trend
    ),
    inherit.aes = FALSE,
    alpha = 1
  ) +
  facet_wrap(~county, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Increasing" = "red",
      "Decreasing" = "blue",
      "Stable" = "yellow"
    )
  ) +
  labs(
    title = "COVID Counts with GAM Trend and Weekly Trend Indicator (Increasing/Decreasing)",
    x = "Week",
    y = "Count"
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off")

