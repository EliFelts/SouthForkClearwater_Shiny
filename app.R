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

flow.dat <- read_rds("data/sfc_flow")

latest_discharge <- flow.dat |>
  slice(which.max(date))

daily.dat <- readRDS("data/daily")

individuals.dat <- readRDS("data/individuals") %>%
  mutate(sfc_entry_final = as_date(as.POSIXct(sfc_entry_final)))

current_sy_key <- tibble(species = c("Chinook", "Bull Trout", "Pacific Lamprey", "Cutthroat Trout", "Steelhead")) %>%
  mutate(current_sy = case_when(
    yday(today()) >= 183 & species == "Steelhead" ~ year(today()) + 1,
    TRUE ~ year(today())
  ))

alldaily.dat <- readRDS("data/alldaily") |>
  filter(spawn_year > 2017) |>
  left_join(current_sy_key, by = c("species")) |>
  mutate(
    yr_category = ifelse(spawn_year == current_sy,
      "Current", "Previous"
    ),
    dummy_jday = yday(dummy_date)
  )

# calculate estimate of how much of the run is complete

run_stats <- readRDS("data/run_stats")

test_min <- alldaily.dat |>
  group_by(species, spawn_year) |>
  summarize(earliest = min(sfc_final_date, na.rm = T))

today_dummy <- as.Date(yday(today()) - 1,
  origin = "1976-01-01"
)

today_ref <- as.Date(format(Sys.Date(), "%Y-01-01")) - 1

spp_max <- run_stats |>
  filter(min_percentcomplete == 100) |>
  ungroup() |>
  group_by(species) |>
  slice(which.min(dummy_date)) |>
  mutate(current_max = as.Date(yday(dummy_date),
    origin = today_ref
  ))

today_run <- run_stats |>
  filter(dummy_date == today_dummy)

lf.dat <- individuals.dat %>%
  filter(
    release_lifestage == "Adult",
    !is.na(length_mm)
  ) %>%
  group_by(species) |>
  mutate(length_bin = floor(length_mm / 25) * 25) %>%
  mutate(
    mean_length = mean(length_mm),
    total_n = n()
  ) %>%
  group_by(species, length_bin) %>%
  summarize(
    freq = n(),
    total_sample = first(total_n),
    mean_length = first(mean_length),
    mean_length_in = mean_length / 25.4
  )

slider_min <- as_date(as.POSIXct(min(individuals.dat$sfc_entry_final)))

lastweek_detections <- individuals.dat %>%
  filter(sfc_entry_final >= today() - days(7))

lastweek_new <- individuals.dat %>%
  filter(sfc_entry_final >= today() - days(7))

# start building the UI

ui <- page_navbar(
  title = "South Fork Clearwater Fishery Tracker",
  theme = bs_theme(bootswatch = "lux"),
  sidebar = sidebar(
    width = 300,
    selectInput(
      inputId = "user_spp",
      label = "Choose Species",
      choices = c("Steelhead", "Chinook"),
      selected = "Steelhead",
      selectize = FALSE
    )
  ),
  nav_panel(
    title = NULL,
    layout_columns(
      value_box(
        title = "Unique Fish, Current Spawn Year",
        value = textOutput("ind_count_txt"),
        showcase = fa("fish-fins")
      ),
      value_box(
        title = "New iin the Last Week",
        value = textOutput("lastweek_count_txt"),
        showcase = bs_icon("graph-up-arrow")
      ),
      value_box(
        title = "Estimated Percent of Run Complete",
        value = textOutput("todayrun_median"),
        showcase = bs_icon("circle-half"),
        p(textOutput("todayrun_range"))
      )
    ),
    page_fillable(
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(card_header("Stream Discharge"),
          plotlyOutput("flow_plot"),
          full_screen = T
        ),
        card(card_header("Fish Lengths"),
          plotlyOutput("lf_plot"),
          full_screen = T
        ),
        card(card_header("Cumulative Entries"),
          plotlyOutput("comp_plot"),
          full_screen = T
        ),
        card(card_header("Daily Entries"),
          plotlyOutput("entry_plot"),
          full_screen = T
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # make current spawn year reactive to species selection

  sy_reactive <- reactive({
    req(input$user_spp)

    dat <- current_sy_key |>
      filter(species == input$user_spp)
  })

  # number of individuals, reactive to species selection

  ind.dat_reactive <- reactive({
    req(input$user_spp)

    dat <- individuals.dat %>%
      filter(species == input$user_spp)
  })

  # make an output of the number of individuals to
  # go to the value box

  output$ind_count_txt <- renderText({
    nrow(req(ind.dat_reactive()))
  })
}

shinyApp(ui, server)
