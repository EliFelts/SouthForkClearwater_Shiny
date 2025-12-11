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
  mutate(
    current_sy = case_when(
      yday(today()) >= 183 & species == "Steelhead" ~ year(today()) + 1,
      TRUE ~ year(today())
    ),
    today.jday = yday(today()),
    today.dummy = case_when(
      today.jday < 183 & species == "Steelhead" ~ as.Date(today.jday, origin = "1977-01-01"),
      TRUE ~ as.Date(today.jday, origin = "1976-01-01")
    )
  )

alldaily.dat <- readRDS("data/alldaily") |>
  filter(spawn_year > 2017) |>
  left_join(current_sy_key, by = c("species")) |>
  mutate(
    yr_category = ifelse(spawn_year == current_sy,
      "Current", "Previous"
    ),
    dummy_jday = yday(dummy_date)
  ) %>%
  filter(dummy_date <= today.dummy)

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

  # reactive for number of new in the last week

  lastweek_reactive <- reactive({
    req(input$user_spp)

    dat <- individuals.dat %>%
      filter(
        species == input$user_spp,
        sfc_entry_final >= today() - days(7)
      )
  })

  # make text output of the number of new in last week

  output$lastweek_count_txt <- renderText({
    nrow(req(lastweek_reactive()))
  })

  # reactive for percent of run complete as of today

  runstatus_reactive <- reactive({
    req(input$user_spp)

    dat <- today_run %>%
      filter(species == input$user_spp)
  })

  # output of median percent complete today

  output$todayrun_median <- renderText({
    dat <- runstatus_reactive() %>%
      pull(median_percentcomplete)

    str_c(dat, "%", sep = " ")
  })

  # output for range of percent complete today

  output$todayrun_range <- renderText({
    dat <- runstatus_reactive()

    str_c("Range:", dat$min_percentcomplete, "%",
      "-",
      dat$max_percentcomplete, "%",
      sep = " "
    )
  })

  # make the flow plot as a reactive

  flowplot_reactive <- reactive({
    flow.plot <- flow.dat %>%
      mutate(date = as_date(date)) %>%
      ggplot(aes(x = date, y = mean_discharge, group = group)) +
      geom_line(aes(text = str_c(" Date:", date,
        "<br>", "Mean Discharge (cfs): ", mean_discharge,
        sep = " "
      ))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "Mean Discharge at Stites Gaging Station")
  })

  # render the flow plot as a plotly object

  output$flow_plot <- renderPlotly({
    plot1 <- flowplot_reactive()

    ggplotly(plot1,
      tooltip = c("text")
    )
  })


  # make all daily data reactive to user selected species

  alldaily_reactive <- reactive({
    req(input$user_spp)

    sy.dat <- sy_reactive()

    alldaily.dat |>
      filter(species == input$user_spp) |>
      filter(spawn_year < sy.dat$current_sy |
        (spawn_year == sy.dat$current_sy &
          dummy_jday <= sy.dat$today.jday))
  })

  # make daily unique fish for current year filter by
  # user selected species

  daily_reactive <- reactive({
    req(input$user_spp)

    dat <- daily.dat |>
      filter(species == input$user_spp)
  })

  # make a reactive plot of cumulative numbers in

  compplot_reactive <- reactive({
    req(input$user_spp)

    dat <- alldaily_reactive()


    compplot <- dat %>%
      ggplot(aes(
        x = dummy_date, y = daily_cumulative_n,
        group = spawn_year, color = as.factor(yr_category)
      )) +
      geom_line(aes(text = str_c(" Date:", format(dummy_date, "%b %d"),
        "<br>",
        "Spawn Year:", spawn_year,
        "<br>",
        "Number In:", round(daily_cumulative_n),
        sep = " "
      ))) +
      theme_bw() +
      scale_color_manual(values = c("steelblue", "gray70")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Date entered South Fork Clearwater River",
        y = "# PIT Tags in, Year-To-Date",
        color = ""
      )
  })

  # Render the comp plot as a plotly object

  output$comp_plot <- renderPlotly({
    plot1 <- compplot_reactive()

    ggplotly(plot1,
      tooltip = c("text")
    )
  })
}

shinyApp(ui, server)
