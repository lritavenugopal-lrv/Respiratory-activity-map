

setwd("C:/Users/lekshmi.venugopal/OneDrive - Southwest District Health/Documents/Respiratory activity map")


# Load required libraries
library(readxl)
library(dplyr)
library(qcc)
library (zoo)
library(incidence2)
library(tidyr)

pdraft <-  read_excel("data.xlsx")

pdraft <- pdraft %>%
  filter (  Jurisdiction == "Health District 3")

unique (pdraft$Jurisdiction)
colnames (pdraft)

pdraft$date <- as.Date(pdraft$date, format = "%YYY/%MM/%DD")



# Step 1: Calculate daily incidence grouped by county
daily_cases <- incidence2::incidence(
  pdraft,
  date_index = "date",
  groups = "County"
)

# Step 2: Define full date range
all_dates <- seq.Date(
  from = as.Date("2023-10-15"),
  to   = as.Date("2025-10-11"),  # <-- if reversed, this will give an empty vector
  by   = "day"
)
# Fix reversed range automatically:
if (length(all_dates) == 0) {
  all_dates <- seq.Date(as.Date("2023-10-11"), as.Date("2023-10-15"), by = "day")
}

# Step 3: Complete the dataset for each County
daily_cases <- daily_cases %>%
  tidyr::complete(
    County,
    date_index = all_dates,
    fill = list(count = 0)
  ) %>%
  rename(I = count, date = date_index)

# Summarize daily cases into weekly incidence by county
pweek <- daily_cases %>%
  mutate(week = as.Date(cut(date, breaks = "week"))) %>%
  group_by(County, week) %>%
  summarize(case = sum(I), .groups = "drop") %>%
  rename(date = week)


# Step 1: Rename counties to desired format
pweek <- pweek %>%
  mutate(County = case_when(
    County == "Adams County" ~ "ID_Adams",
    County == "Canyon County" ~ "ID_Canyon",
    County == "Gem County" ~ "ID_Gem",
    County == "Owyhee County" ~ "ID_Owyhee",
    County == "Payette County" ~ "ID_Payette",
    County == "Washington County" ~ "ID_Washington",
    TRUE ~ County  # fallback in case of unexpected values
  ))

# Step 2: Pivot wider so counties become columns
pweek_wide <- pweek %>%
  pivot_wider(
    names_from = County,
    values_from = case,
    values_fill = 0  )

# Load library
library(writexl)
# Export to Excel
write_xlsx(pweek_wide, "pertussislevels.xlsx")
