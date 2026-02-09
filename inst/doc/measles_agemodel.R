## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(multigroup.vaccine)
library(socialmixr)

## -----------------------------------------------------------------------------
# under 1, 1-4, 5-11, 12-17, 18-24, 25-44, 45-69, 70 plus
age_limits <- c(0, 1, 5, 12, 18, 25, 45, 70)

## -----------------------------------------------------------------------------
# Get data for Washington County, Utah
washington_data <- getCensusData(
  state_fips = getStateFIPS("Utah"),
  county_name = "Washington County",
  year = 2024,
  age_groups = age_limits,
  csv_path = getCensusDataPath()
)

## -----------------------------------------------------------------------------
age_immunity <- c(0, 0.77, 0.83, 0.85, 0.87, 0.90, 0.92, 1)

## -----------------------------------------------------------------------------
popsize <- washington_data$age_pops

initV <- round(age_immunity * popsize)  # initially immune cases 

initI <- rep(0, length(popsize))  # initial infectious cases
initI[6] <- 1                     # assume 1 initial case in the 6th age group (25-44)

initR <- rep(0, length(popsize))  # no recent prior measles outbreaks

## -----------------------------------------------------------------------------
contactmatrix <- contactMatrixPolymod(age_limits, popsize)
relsusc <- rep(1, length(popsize))      # Assume no age differences in susceptibility
reltransm <- rep(1, length(popsize))    # or transmissibility per contact
R0 <- 10

## -----------------------------------------------------------------------------
fs <- finalsize(popsize, R0, contactmatrix, relsusc, reltransm, initR, initI, initV)
round(sum(fs))

## -----------------------------------------------------------------------------
names(fs) <- washington_data$age_labels
round(fs)

## -----------------------------------------------------------------------------
fs_hybrid <- finalsize(popsize, R0, contactmatrix, relsusc, reltransm, initR, initI, initV, method = "hybrid", nsims = 30)
colnames(fs_hybrid) <- washington_data$age_labels
fs_hybrid

