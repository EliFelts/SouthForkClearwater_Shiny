library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(dataRetrieval)
library(bsicons)
library(viridis)
library(scales)
library(fontawesome)
library(readr)
library(ggokabeito)

conflicts_prefer(
  DT::renderDT,
  dplyr::filter,
  dplyr::lag,
  plotly::layout
)

# read in water data

water.dat <- read_rds("data/sfc_flow")

latest_discharge <- water.dat |>
  slice(which.max(date))
