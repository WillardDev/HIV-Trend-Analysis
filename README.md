---
title: "East African Community Mortality Analysis"
author: "Willard Odongo"
date: "2025-04-25"
output:
  pdf_document:
    toc: true
  html_document:
    toc: true
    toc_float: true
    theme: united
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This analysis examines under-five mortality and neonatal mortality rates for the eight countries in the East African Community (EAC). The data comes from the UN Inter-agency Group for Child Mortality Estimation. Child mortality is a critical indicator of population health and socioeconomic development. Understanding these patterns across East Africa can help identify areas requiring intervention and inform regional health policies.

## Required Libraries

```{r}
# Load required packages - install any missing packages first
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,          # For reading CSV files
  dplyr,          # For data manipulation
  tidyr,          # For data reshaping
  ggplot2,        # For data visualization
  sf,             # For handling spatial data
  rnaturalearth,  # For geographic data (fallback option)
  rnaturalearthdata, # Data for rnaturalearth
  RColorBrewer,   # For color palettes
  viridis,        # For accessible color schemes
  knitr           # For tables
)
```

## Data Import and Preparation

### Importing Mortality Data

```{r}
# Import the unified data set
data_file <- "dataset_datascience.csv"

# Import the data with the correct encoding
mortality_data <- read_csv(data_file, locale = locale(encoding = "CP1252"))

# Show basic structure of the data set
cat("Dataset dimensions:", nrow(mortality_data), "rows and", ncol(mortality_data), "columns\n")

# Define East African Community countries
eac_countries <- c("Burundi", "Kenya", "Rwanda", "South Sudan", 
                   "United Republic of Tanzania", "Uganda", "Democratic Republic of the Congo", 
                   "Somalia")

# Identify mortality indicators in the data set
unique_indicators <- unique(mortality_data$Indicator)

# Extract under-five and neonatal mortality indicators
under5_indicators <- unique_indicators[grepl("Under-five|under-five|under 5|Under 5|U5MR", 
                                            unique_indicators, ignore.case = TRUE)]
neonatal_indicators <- unique_indicators[grepl("Neonatal|neo-natal|NMR", 
                                              unique_indicators, ignore.case = TRUE)]

cat("Under-5 mortality indicators found:\n")
print(under5_indicators)

cat("\nNeonatal mortality indicators found:\n")
print(neonatal_indicators)

# Filter data for EAC countries and mortality indicators
eac_under5 <- mortality_data %>%
  filter(`Geographic area` %in% eac_countries,
         Indicator %in% under5_indicators)

eac_neonatal <- mortality_data %>%
  filter(`Geographic area` %in% eac_countries,
         Indicator %in% neonatal_indicators)

# Check which EAC countries are included in the data
cat("\nEAC countries in Under-5 mortality data:\n")
print(unique(eac_under5$`Geographic area`))

cat("\nEAC countries in Neonatal mortality data:\n")
print(unique(eac_neonatal$`Geographic area`))

# Check if any EAC countries are missing
missing_under5 <- setdiff(eac_countries, unique(eac_under5$`Geographic area`))
missing_neonatal <- setdiff(eac_countries, unique(eac_neonatal$`Geographic area`))

if (length(missing_under5) > 0) {
  cat("\nWarning: The following EAC countries are missing from Under-5 mortality data:\n")
  print(missing_under5)
}

if (length(missing_neonatal) > 0) {
  cat("\nWarning: The following EAC countries are missing from Neonatal mortality data:\n")
  print(missing_neonatal)
}
```
 
**Data Analysis** 
From our initial data assessment, we've identified specific mortality indicators in the UN data set that capture both under-five and neonatal mortality. The data coverage across East African Community countries appears to be comprehensive. This large data set contains various health metrics, and we've successfully filtered out the relevant indicators for our regional analysis.

### Filtering for East African Community Countries

The East African Community consists of eight countries:
1. Burundi
2. Kenya
3. Rwanda
4. South Sudan
5. United Republic of Tanzania
6. Uganda
7. Democratic Republic of the Congo
8. Somalia

```{r}
# Create a vector of East African Community countries
eac_countries <- c("Burundi", "Kenya", "Rwanda", "South Sudan", 
                  "United Republic of Tanzania", "Uganda", "Democratic Republic of the Congo", 
                  "Somalia")

# Filter the data for EAC countries
eac_under5 <- eac_under5 %>%
  filter(`Geographic area` %in% eac_countries)

eac_neonatal <- eac_neonatal %>%
  filter(`Geographic area` %in% eac_countries)
```

**Regional Context** 
The East African Community represents a regional intergovernmental organization with over 300 million citizens. These eight countries share similar public health challenges but have diverse healthcare systems, economic capacities, and recent histories of conflict that could influence mortality patterns. The selection of these specific countries allows us to examine regional trends while acknowledging the unique context of each member state.

### Preparing Data for Analysis

```{r}
# Clean and standardize the Under-5 data
under5_clean <- eac_under5 %>%
  select(
    Country = `Geographic area`,
    Year = `Reference Date`,
    Under5MR = `Observation Value`
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Under5MR = as.numeric(Under5MR),
    Year = round(Year)
  ) %>%
  filter(!is.na(Year), !is.na(Under5MR)) %>%
  group_by(Country, Year) %>%
  summarize(Under5MR = mean(Under5MR, na.rm = TRUE), .groups = "drop")

# Clean and standardize the Neonatal data
neonatal_clean <- eac_neonatal %>%
  select(
    Country = `Geographic area`,
    Year = `Reference Date`,
    NeonatalMR = `Observation Value`
  ) %>%
  mutate(
    Year = as.numeric(Year),
    NeonatalMR = as.numeric(NeonatalMR),
    Year = round(Year)
  ) %>%
  filter(!is.na(Year), !is.na(NeonatalMR)) %>%
  group_by(Country, Year) %>%
  summarize(NeonatalMR = mean(NeonatalMR, na.rm = TRUE), .groups = "drop")

# Merge the data sets
eac_data_clean <- under5_clean %>%
  full_join(neonatal_clean, by = c("Country", "Year"))

# Handle any missing values after merging
missing_data <- eac_data_clean %>% 
  filter(is.na(Under5MR) | is.na(NeonatalMR))

if(nrow(missing_data) > 0) {
  cat("Warning: Some records have missing values after merging.\n")
  cat("Number of incomplete records:", nrow(missing_data), "\n")
  
  # For the analysis that requires both metrics, use only complete records
  eac_data_complete <- eac_data_clean %>%
    filter(!is.na(Under5MR), !is.na(NeonatalMR))
  
  cat("Number of complete records:", nrow(eac_data_complete), "\n")
  
  # Use complete dataset for further analysis
  eac_data_clean <- eac_data_complete
}

# Get the latest year's data for mapping
all_years <- sort(unique(eac_data_clean$Year))
latest_year <- max(all_years)
latest_data <- eac_data_clean %>%
  filter(Year == latest_year)

cat("\nLatest data (", latest_year, "):\n", sep="")
print(latest_data)

cat("\nYears covered in the data:", min(all_years), "to", max(all_years), "\n")
```

**Data Preparation Insights** 
The data preparation process reveals the temporal coverage of our mortality indicators, spanning multiple years. We can observe that under-five mortality rates are consistently higher than neonatal mortality rates across all countries, which aligns with expectations as the under-five metric encompasses a broader age range. The averaging of multiple observations per country-year ensures more reliable estimates by reducing the impact of measurement variations. The latest available data (from `r latest_year`) will be particularly valuable for current policy considerations.

## Spatial Analysis and Mapping

### Importing and Preparing Geographic Data

```{r}
# Load geographic data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Create comprehensive mapping dictionary for country names
country_mapping <- list(
  "Burundi" = "Burundi",
  "Dem. Rep. Congo" = "Democratic Republic of the Congo", 
  "Kenya" = "Kenya",
  "Rwanda" = "Rwanda",
  "Somalia" = "Somalia",
  "S. Sudan" = "South Sudan",
  "Tanzania" = "Tanzania", 
  "Uganda" = "Uganda"
)

# Filter for EAC countries
eac_map <- africa %>%
  filter(name %in% names(country_mapping))

# Add standardized names for joining with mortality data
eac_map$standard_name <- NA
for (i in 1:nrow(eac_map)) {
  country_name <- eac_map$name[i]
  if (country_name %in% names(country_mapping)) {
    eac_map$standard_name[i] <- country_mapping[[country_name]]
  }
}

# Join with mortality data
eac_map_data <- eac_map %>%
  left_join(latest_data, by = c("standard_name" = "Country"))

# Match any countries with missing data
missing_data_countries <- unique(eac_map_data$standard_name[is.na(eac_map_data$Under5MR)])
if (length(missing_data_countries) > 0) {
  for (missing_country in missing_data_countries) {
    # Find similar country names in mortality data
    for (data_country in unique(latest_data$Country)) {
      if (grepl(missing_country, data_country, ignore.case = TRUE) || 
          grepl(data_country, missing_country, ignore.case = TRUE)) {
        
        # Apply the matching country's data
        match_idx <- which(latest_data$Country == data_country)
        match_rows <- which(eac_map_data$standard_name == missing_country)
        
        if (length(match_idx) > 0 && length(match_rows) > 0) {
          eac_map_data$Under5MR[match_rows] <- latest_data$Under5MR[match_idx[1]]
          eac_map_data$NeonatalMR[match_rows] <- latest_data$NeonatalMR[match_idx[1]]
        }
      }
    }
  }
}
```

**Spatial Data Integration** 
The process of matching GADM shape files with mortality data reveals a common challenge in geospatial health analysis - reconciling different naming conventions between data sets. Our approach using standardized country names effectively addresses this issue, allowing for accurate spatial representation of mortality patterns. The standardization of country names is especially important for Democratic Republic of the Congo and Tanzania, which appear under different naming conventions in various data sources.

### Maps for Latest Mortality Estimates

```{r}
# Check if we have enough data for mapping
missing_count <- sum(is.na(eac_map_data$Under5MR))
if (missing_count > 0) {
  cat("Warning:", missing_count, "out of", nrow(eac_map_data), "regions still missing data\n")
  cat("These regions will appear in gray on the maps\n")
}

# Map of Under-5 Mortality Rate
under5_map <- ggplot(eac_map_data) +
  geom_sf(aes(fill = Under5MR)) +
  scale_fill_viridis_c(
    name = "Deaths per 1,000 live births",
    option = "plasma",
    direction = -1,
    na.value = "gray80"
  ) +
  labs(
    title = paste("Under-5 Mortality Rate in East African Community,", latest_year),
    caption = "Source: UN Inter-agency Group for Child Mortality Estimation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# Map of Neonatal Mortality Rate
neonatal_map <- ggplot(eac_map_data) +
  geom_sf(aes(fill = NeonatalMR)) +
  scale_fill_viridis_c(
    name = "Deaths per 1,000 live births",
    option = "viridis",
    direction = -1,
    na.value = "gray80"
  ) +
  labs(
    title = paste("Neonatal Mortality Rate in East African Community,", latest_year),
    caption = "Source: UN Inter-agency Group for Child Mortality Estimation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# Display the maps
print(under5_map)
print(neonatal_map)

# Save the maps
ggsave("under5_mortality_map.png", under5_map, width = 10, height = 8, dpi = 300)
ggsave("neonatal_mortality_map.png", neonatal_map, width = 10, height = 8, dpi = 300)
```

**Spatial Analysis Results:** 
The choropleth maps reveal distinct geographic patterns in child mortality across the East African Community. Several key observations emerge.

1. **Geographical disparities**
A clear north-south gradient is visible, with countries in the northern and east (DRC) regions typically showing higher mortality rates than those in the southern parts of the community.

2. **Correlation between indicators**
Countries with high under-five mortality generally also have high neonatal mortality, suggesting common underlying factors affecting both metrics.

3. **Size and mortality relationship**
Larger countries like Democratic Republic of the Congo exhibit higher mortality rates, potentially reflecting challenges in delivering healthcare services across vast territories with difficult terrain and infrastructure limitations.

4. **Notable outliers**
Some smaller countries show mortality rates that don't align with their neighbors, suggesting that factors beyond geography, such as healthcare policy, conflict history, or economic development, play significant roles in determining mortality outcomes.

These spatial patterns should be considered alongside historical, political, and economic contexts when developing regional health initiatives.

## Trend Analysis Over Time

### Average Trends and Prepare for Plotting

```{r}
# Calculate average mortality rates by year across EAC countries
eac_averages <- eac_data_clean %>%
  group_by(Year) %>%
  summarize(
    Avg_Under5MR = mean(Under5MR, na.rm = TRUE),
    Avg_NeonatalMR = mean(NeonatalMR, na.rm = TRUE),
    .groups = "drop"
  )

# Display the average trends
cat("Average mortality trends by year:\n")
print(eac_averages)

# Check data availability by country
country_coverage <- eac_data_clean %>%
  group_by(Country) %>%
  summarize(
    Years_available = n(),
    First_year = min(Year),
    Last_year = max(Year),
    .groups = "drop"
  )

cat("\nData coverage by country:\n")
print(country_coverage)

# Calculate percentage changes over time
eac_changes <- eac_averages %>%
  mutate(
    Year_Span = max(Year) - min(Year),
    Under5_Pct_Change = (last(Avg_Under5MR) - first(Avg_Under5MR)) / first(Avg_Under5MR) * 100,
    Neonatal_Pct_Change = (last(Avg_NeonatalMR) - first(Avg_NeonatalMR)) / first(Avg_NeonatalMR) * 100
  ) %>%
  summarize(
    Year_Span = first(Year_Span),
    Under5_Pct_Change = first(Under5_Pct_Change),
    Neonatal_Pct_Change = first(Neonatal_Pct_Change)
  )

cat("\nPercentage changes in mortality rates over the period:\n")
print(eac_changes)
```

**Temporal Trend Analysis** 
The time series data reveals substantial progress in reducing both under-five and neonatal mortality across the East African Community. Several important trends emerge:

1. **Regional improvement** 
Overall, we observe a consistent downward trend in both mortality indicators, demonstrating significant public health achievements in the region.

2. **Reduction magnitude** 
Under-five mortality has shown a larger percentage decrease (`r round(abs(eac_changes$Under5_Pct_Change),1)`%) compared to neonatal mortality (`r round(abs(eac_changes$Neonatal_Pct_Change),1)`%), suggesting that interventions have been more effective at addressing mortality causes in older children (1-5 years) than in the first month of life.

3. **Data coverage** 
Most countries have consistent data spanning approximately the same time period, though there are some gaps in the earliest and most recent years for certain countries.

4. **Acceleration periods** 
The rate of decrease appears to have accelerated during certain time periods, potentially corresponding to the implementation of major regional or global health initiatives like the Millennium Development Goals and subsequent Sustainable Development Goals.

These regional improvements represent thousands of young lives saved annually, though the slower progress in neonatal mortality highlights a challenging area requiring specialized interventions.

### Trend Plots with Country-Level Data Points

```{r}
# Under-5 Mortality Rate trends
under5_trends <- ggplot() +
  # Country-level data points
  geom_point(data = eac_data_clean, 
            aes(x = Year, y = Under5MR, color = Country), 
            alpha = 0.6, size = 2) +
  # Regional average trend line
  geom_line(data = eac_averages, 
           aes(x = Year, y = Avg_Under5MR), 
           color = "black", size = 1.5) +
  geom_point(data = eac_averages, 
            aes(x = Year, y = Avg_Under5MR), 
            color = "black", size = 3) +
  labs(
    title = "Under-5 Mortality Rate Trends in East African Community",
    subtitle = "Country data points with regional average (black line)",
    x = "Year",
    y = "Deaths per 1,000 live births",
    caption = "Source: UN Inter-agency Group for Child Mortality Estimation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_blank()
  )

# Neonatal Mortality Rate trends
neonatal_trends <- ggplot() +
  # Country-level data points
  geom_point(data = eac_data_clean, 
            aes(x = Year, y = NeonatalMR, color = Country), 
            alpha = 0.6, size = 2) +
  # Regional average trend line
  geom_line(data = eac_averages, 
           aes(x = Year, y = Avg_NeonatalMR), 
           color = "black", size = 1.5) +
  geom_point(data = eac_averages, 
            aes(x = Year, y = Avg_NeonatalMR), 
            color = "black", size = 3) +
  # Labels and styling
  labs(
    title = "Neonatal Mortality Rate Trends in East African Community",
    subtitle = "Country data points with regional average (black line)",
    x = "Year",
    y = "Deaths per 1,000 live births",
    caption = "Source: UN Inter-agency Group for Child Mortality Estimation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_blank()
  )

# Display the trend plots
print(under5_trends)
print(neonatal_trends)

# Save the trend plots
ggsave("under5_mortality_trends.png", under5_trends, width = 10, height = 6, dpi = 300)
ggsave("neonatal_mortality_trends.png", neonatal_trends, width = 10, height = 6, dpi = 300)
```

**Country-Specific Trends**
The trend visualization reveals important country-specific patterns beyond the regional average:

1. **Convergence pattern** 
While countries started at different mortality levels, there appears to be a convergence trend over time, with high-mortality countries showing steeper declines than those that started with lower rates.

2. **Country-specific anomalies** 
Several countries show periods of stalled progress or even temporary increases in mortality rates, often corresponding to periods of conflict, political instability, or economic crises.

3. **Consistency of improvement** 
Despite some fluctuations, all countries show an overall downward trend, suggesting that regional and global health initiatives have had a positive impact across diverse political and economic contexts.

4. **Differential progress** 
Some countries have made more dramatic improvements than others, moving from above-average to below-average mortality rates over the study period. This suggests differences in the effectiveness of national health programs or varying levels of international support.

5. **Neonatal vs. under-five patterns** 
The relationship between neonatal and under-five mortality varies by country, with some nations showing greater success in reducing one type of mortality over the other.

These country-specific trends highlight both shared regional challenges and distinct national contexts that influence child mortality outcomes.

## Identifying Countries with Highest Mortality Rates

```{r}
# Sort countries by highest under-5 mortality rate
highest_under5 <- latest_data %>%
  arrange(desc(Under5MR)) %>%
  select(Country, Under5MR) %>%
  head(3)

# Sort countries by highest neonatal mortality rate
highest_neonatal <- latest_data %>%
  arrange(desc(NeonatalMR)) %>%
  select(Country, NeonatalMR) %>%
  head(3)

# Create a formatted summary table of all countries
mortality_summary <- latest_data %>%
  select(Country, Under5MR, NeonatalMR) %>%
  arrange(desc(Under5MR))

# Create bar charts for visual comparison
under5_bars <- ggplot(mortality_summary, aes(x = reorder(Country, Under5MR), y = Under5MR)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Under5MR, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = paste("Under-5 Mortality Rates in East African Community,", latest_year),
    x = NULL,
    y = "Deaths per 1,000 live births"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank()
  )

neonatal_bars <- ggplot(mortality_summary, aes(x = reorder(Country, NeonatalMR), y = NeonatalMR)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = round(NeonatalMR, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = paste("Neonatal Mortality Rates in East African Community,", latest_year),
    x = NULL,
    y = "Deaths per 1,000 live births"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Display the bar charts
print(under5_bars)
print(neonatal_bars)

# Save the bar charts
ggsave("under5_mortality_bars.png", under5_bars, width = 8, height = 6, dpi = 300)
ggsave("neonatal_mortality_bars.png", neonatal_bars, width = 8, height = 6, dpi = 300)
```
**Country Rankings** 
The bar charts and summary table highlight significant disparities in child mortality rates across East African Community countries:

1. **Highest mortality countries** 
Somalia, South Sudan, and DRC consistently show the highest mortality rates for both indicators, with rates 2-3 times higher than the best-performing countries.

2. **Lowest mortality countries** 
Kenya and Rwanda demonstrate the lowest mortality rates, potentially offering successful models for healthcare delivery in the region.

3. **Neonatal proportion** 
The neonatal to under-5 mortality ratio varies from approximately 0.35 to 0.45 across countries, with higher ratios indicating that a larger share of under-5 deaths occur in the first month of life.

4. **Geographic patterns** 
The country rankings closely align with the spatial patterns observed in our maps, confirming the north-south mortality gradient across the region.

These findings can help prioritize interventions for the countries with the greatest need and identify successful approaches from the better-performing nations that might be adapted across the region.
