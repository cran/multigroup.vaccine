## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(multigroup.vaccine)
library(socialmixr)

## -----------------------------------------------------------------------------
# under 1, 1-4, 5-11, 12-13, 14-17, 18-24, 25-44, 45-69, 70 plus
agelims <- c(0, 1, 5, 12, 14, 18, 25, 45, 70)
agepops <- c(100, 400, 700, 200, 400, 700, 2000, 2400, 1000)

## -----------------------------------------------------------------------------
cmp <- contactMatrixPolymod(agelims, agepops)
knitr::kable(round(cmp, 2), format = "markdown")

## -----------------------------------------------------------------------------
knitr::kable(round(rowSums(cmp), 2), format = "markdown", col.names = "total")

## -----------------------------------------------------------------------------
knitr::kable(round(cmp/rowSums(cmp), 2), format = "markdown")

## -----------------------------------------------------------------------------
schoolagegroups <- c(3, 3, 4, 4, 5, 5)         #The indices of the age group for each school
schoolpops <- c(350, 350, 100, 100, 200, 200)  #The number of students in each school

## -----------------------------------------------------------------------------
cmAll <- suppressMessages(
    suppressWarnings(socialmixr::contact_matrix(socialmixr::polymod,
                                                age.limits = agelims)$matrix))
cmSchool <- suppressMessages(
    suppressWarnings(socialmixr::contact_matrix(socialmixr::polymod,
                                                age.limits = agelims,
                                                filter = list(cnt_school = 1))$matrix))

knitr::kable(round(cmAll, 2), format = "markdown")
knitr::kable(round(cmSchool, 2), format = "markdown")

## -----------------------------------------------------------------------------
schportion <- 0.70

## -----------------------------------------------------------------------------
cmps <- contactMatrixAgeSchool(agelims, agepops, schoolagegroups, schoolpops, schportion)

knitr::kable(round(cmp,2), format = "markdown")
knitr::kable(round(cmps,2), format = "markdown")

