# ================================
# GDP Growth Analysis: 1999–2019
# ================================

# Install packages if needed
install.packages(c("tidyverse", "lubridate", "scales", "knitr"))

library(tidyverse)
library(lubridate)
library(scales)
library(knitr)

# ----------------
# 1. Load the data
# ----------------

gdps <- read_csv("gdps.csv")

# Rename FRED series codes to country names
gdps <- gdps %>%
  rename(
    date = observation_date,
    Austria = NAEXKP01ATQ189S,
    Belgium = NAEXKP01BEQ189S,
    Finland = NAEXKP01FIQ189S,
    France = NAEXKP01FRQ189S,
    Germany = NAEXKP01DEQ189S,
    Ireland = NAEXKP01IEQ189S,
    Italy = NAEXKP01ITQ189S,
    Netherlands = NAEXKP01NLQ189S,
    Portugal = NAEXKP01PTQ652S,
    Spain = NAEXKP01ESQ652S
  ) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("1999-01-01"),
         date <= as.Date("2019-10-01"))

# -------------------------------
# 2. Convert to long/tidy format
# -------------------------------

gdp_long <- gdps %>%
  pivot_longer(
    cols = -date,
    names_to = "country",
    values_to = "real_gdp"
  )

# ------------------------------------------------------
# 3. Calculate year-on-year real GDP growth per country
# ------------------------------------------------------
# Formula:
# growth = ((GDP_t / GDP_t-4) - 1) * 100

gdp_growth <- gdp_long %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    yoy_growth = ((real_gdp / lag(real_gdp, 4)) - 1) * 100
  ) %>%
  ungroup()

# ----------------------------------------
# 4. Define Great Recession period
# ----------------------------------------

recession_start <- as.Date("2008-01-01")  # 2008Q1
recession_end   <- as.Date("2010-01-01")  # 2010Q1

gdp_growth <- gdp_growth %>%
  mutate(
    period = ifelse(
      date >= recession_start & date <= recession_end,
      "Great Recession",
      "Other years"
    )
  )

# -----------------------------------------------------
# 5. Chart: GDP growth for each country, separate plots
# -----------------------------------------------------

# Create a folder for the country charts
dir.create("country_gdp_growth_charts", showWarnings = FALSE)

# Get list of countries
countries <- unique(gdp_growth$country)

# Loop through each country and create/save one chart per country
for (c in countries) {
  
  country_data <- gdp_growth %>%
    filter(country == c)
  
  country_chart <- ggplot(country_data, aes(x = date, y = yoy_growth)) +
    geom_rect(
      aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = Inf),
      fill = "grey80",
      alpha = 0.4,
      inherit.aes = FALSE
    ) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = recession_start, linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = recession_end, linetype = "dashed", linewidth = 0.7) +
    labs(
      title = paste("Year-on-Year Real GDP Growth:", c),
      subtitle = "Vertical dashed lines mark 2008Q1 and 2010Q1",
      x = "Year",
      y = "Real GDP growth, year-on-year (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  # Print chart in RStudio
  print(country_chart)
  
  # Save chart as PNG
  ggsave(
    filename = paste0("country_gdp_growth_charts/", c, "_gdp_growth_chart.png"),
    plot = country_chart,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# -----------------------------------------------------
# 6. Average GDP growth across all 10 countries
# -----------------------------------------------------

average_growth <- gdp_growth %>%
  group_by(date) %>%
  summarise(
    average_yoy_growth = mean(yoy_growth, na.rm = TRUE),
    .groups = "drop"
  )

average_growth_chart <- ggplot(average_growth, aes(x = date, y = average_yoy_growth)) +
  geom_rect(
    aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = Inf),
    fill = "grey80",
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = recession_start, linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = recession_end, linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Average Year-on-Year Real GDP Growth Across 10 Countries",
    subtitle = "Average of Austria, Belgium, Finland, France, Germany, Ireland, Italy, Netherlands, Portugal, and Spain",
    x = "Year",
    y = "Average real GDP growth, year-on-year (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(average_growth_chart)

ggsave(
  "average_gdp_growth_chart.png",
  average_growth_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# -----------------------------------------------------
# 7. Summary table: recession vs other years
# -----------------------------------------------------

summary_table <- gdp_growth %>%
  filter(!is.na(yoy_growth)) %>%
  group_by(country, period) %>%
  summarise(
    average_growth = mean(yoy_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = average_growth
  ) %>%
  mutate(
    difference = `Great Recession` - `Other years`
  ) %>%
  arrange(difference)

# Add average row for all countries
average_row <- gdp_growth %>%
  filter(!is.na(yoy_growth)) %>%
  group_by(period) %>%
  summarise(
    average_growth = mean(yoy_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = average_growth
  ) %>%
  mutate(
    country = "Average of 10 countries",
    difference = `Great Recession` - `Other years`
  ) %>%
  select(country, `Great Recession`, `Other years`, difference)

summary_table_final <- bind_rows(summary_table, average_row) %>%
  mutate(
    across(
      c(`Great Recession`, `Other years`, difference),
      ~ round(.x, 2)
    )
  )

# Print normal table
print(summary_table_final)

# Print markdown table
kable(
  summary_table_final,
  format = "markdown",
  col.names = c(
    "Country",
    "Avg. growth during 2008Q3–2010Q1 (%)",
    "Avg. growth during other years (%)",
    "Difference, recession minus other years"
  )
)

# Save table as CSV
write_csv(summary_table_final, "gdp_growth_summary_table.csv")

# -----------------------------------------------------
# 8. Optional: save full growth dataset
# -----------------------------------------------------

write_csv(gdp_growth, "gdp_growth_full_dataset.csv")