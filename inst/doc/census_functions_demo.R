## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6
)

## ----setup--------------------------------------------------------------------
library(multigroup.vaccine)
library(socialmixr)

# Get the path to the included census data file
census_csv <- getCensusDataPath()

## ----state-fips---------------------------------------------------------------
# Get FIPS code for Utah
utah_fips <- getStateFIPS("Utah")
cat("Utah FIPS code:", utah_fips, "\n")

# You can also try other states
california_fips <- getStateFIPS("California")
cat("California FIPS code:", california_fips, "\n")

texas_fips <- getStateFIPS("Texas")
cat("Texas FIPS code:", texas_fips, "\n")

## ----list-counties------------------------------------------------------------
# List all counties in Utah
utah_counties <- listCounties(
  state_fips = utah_fips,
  csv_path = census_csv
)

cat("Counties in Utah:\n")
print(utah_counties)

cat("\nTotal number of counties:", length(utah_counties), "\n")

## ----get-census-data----------------------------------------------------------
# Define age groups for analysis
# These represent: 0-4, 5-11, 12-17, 18-24, 25-44, 45-64, 65+
age_limits <- c(0, 5, 12, 18, 25, 45, 65)

# Get data for Washington County, Utah
washington_data <- getCensusData(
  state_fips = utah_fips,
  county_name = "Washington County",
  year = 2024,
  age_groups = age_limits,
  csv_path = census_csv
)

# Display the results
cat("County:", washington_data$county, "\n")
cat("Year:", washington_data$year, "\n")
cat("Total population:", format(washington_data$total_pop, big.mark = ","), "\n\n")

cat("Age distribution:\n")
for (i in seq_along(washington_data$age_labels)) {
  pct <- 100 * washington_data$age_pops[i] / washington_data$total_pop
  cat(sprintf("  %s: %s (%.1f%%)\n",
              washington_data$age_labels[i],
              format(washington_data$age_pops[i], big.mark = ","),
              pct))
}

## ----plot-age-distribution, fig.alt="Bar chart showing age distribution percentages for Washington County, Utah. Each bar represents an age group with percentage of total population labeled above."----
# Create a bar plot of age distribution
age_percentages <- 100 * washington_data$age_pops / washington_data$total_pop

barplot(age_percentages,
        names.arg = washington_data$age_labels,
        main = paste("Age Distribution -", washington_data$county),
        xlab = "Age Group",
        ylab = "Percentage of Population",
        col = "steelblue",
        las = 2,
        ylim = c(0, max(age_percentages) * 1.1))

# Add percentage labels on top of bars
text(x = seq_along(age_percentages) * 1.2 - 0.5,
     y = age_percentages + 1,
     labels = sprintf("%.1f%%", age_percentages),
     pos = 3,
     cex = 0.8)

## ----compare-counties, fig.alt="Side-by-side bar chart comparing age distribution percentages across Salt Lake County (coral), Utah County (steel blue), and Washington County (light green). Each age group shows three bars representing the three counties."----
# Get data for three counties
counties_to_compare <- c("Salt Lake County", "Utah County", "Washington County")
county_data_list <- list()

for (county in counties_to_compare) {
  county_data_list[[county]] <- getCensusData(
    state_fips = utah_fips,
    county_name = county,
    year = 2024,
    age_groups = age_limits,
    csv_path = census_csv
  )
}

# Create comparison matrix
comparison_matrix <- matrix(0, nrow = length(counties_to_compare), 
                            ncol = length(age_limits))
colnames(comparison_matrix) <- washington_data$age_labels
rownames(comparison_matrix) <- c("Salt Lake", "Utah", "Washington")

for (i in seq_along(counties_to_compare)) {
  county_name <- counties_to_compare[i]
  data <- county_data_list[[county_name]]
  comparison_matrix[i, ] <- 100 * data$age_pops / data$total_pop
}

# Plot comparison
barplot(comparison_matrix,
        beside = TRUE,
        main = "Age Distribution Comparison Across Utah Counties",
        xlab = "Age Group",
        ylab = "Percentage of Population",
        col = c("coral", "steelblue", "lightgreen"),
        legend.text = rownames(comparison_matrix),
        args.legend = list(x = "topright", bty = "n"),
        las = 2)

## ----city-data-5year----------------------------------------------------------
# Get path to Hildale data
hildale_path <- system.file("extdata", "hildale_ut_2023.csv", package = "multigroup.vaccine")

# Load with default 5-year age groups (0-4, 5-9, 10-14, ...)
hildale_5yr <- getCityData(
  city_name = "Hildale city, Utah",
  csv_path = hildale_path
)

cat("Hildale, UT - 5-year Age Groups\n")
cat("================================\n")
cat("Total population:", format(hildale_5yr$total_pop, big.mark = ","), "\n\n")

cat("Age distribution:\n")
for (i in seq_along(hildale_5yr$age_labels)) {
  pct <- 100 * hildale_5yr$age_pops[i] / hildale_5yr$total_pop
  cat(sprintf("  %s: %s (%.1f%%)\n",
              hildale_5yr$age_labels[i],
              format(hildale_5yr$age_pops[i], big.mark = ","),
              pct))
}

## ----city-data-custom---------------------------------------------------------
# Define school-aligned age groups
school_age_groups <- c(0, 5, 12, 14, 18, 25, 45, 65)

hildale_school <- getCityData(
  city_name = "Hildale city, Utah",
  csv_path = hildale_path,
  age_groups = school_age_groups
)

cat("\nHildale, UT - School-Aligned Age Groups\n")
cat("========================================\n")
cat("Total population:", format(hildale_school$total_pop, big.mark = ","), "\n\n")

cat("Age distribution:\n")
for (i in seq_along(hildale_school$age_labels)) {
  pct <- 100 * hildale_school$age_pops[i] / hildale_school$total_pop
  cat(sprintf("  %s: %s (%.1f%%)\n",
              hildale_school$age_labels[i],
              format(hildale_school$age_pops[i], big.mark = ","),
              pct))
}

## ----compare-aggregations, fig.alt="Two side-by-side bar charts comparing age grouping methods for Hildale, UT. Left panel shows 5-year age groups in steel blue. Right panel shows school-aligned age groups in coral. Both display percentage of population on y-axis."----
# Create a comparison visualization
oldpar <- par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

# Plot 5-year groups
age_pct_5yr <- 100 * hildale_5yr$age_pops / hildale_5yr$total_pop
barplot(age_pct_5yr,
        names.arg = hildale_5yr$age_labels,
        main = "5-Year Age Groups",
        xlab = "Age Group",
        ylab = "% of Population",
        col = "steelblue",
        las = 2,
        cex.names = 0.7,
        ylim = c(0, max(age_pct_5yr) * 1.2))

# Plot school-aligned groups
age_pct_school <- 100 * hildale_school$age_pops / hildale_school$total_pop
barplot(age_pct_school,
        names.arg = hildale_school$age_labels,
        main = "School-Aligned Age Groups",
        xlab = "Age Group",
        ylab = "% of Population",
        col = "coral",
        las = 2,
        cex.names = 0.7,
        ylim = c(0, max(age_pct_school) * 1.2))

par(oldpar)

