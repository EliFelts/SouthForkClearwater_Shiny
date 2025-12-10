library(vroom)
library(dataRetrieval)
library(conflicted)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)


conflicts_prefer(
  vroom::locale,
  dplyr::filter
)

# set the timeout above the default of 60 seconds bc
# sometimes the API calls are slow

options(timeout = 600)

# need to create a data directory in the processing
# code so it gets stored in github actions

dir.create("data", recursive = TRUE, showWarnings = TRUE)

# read in data from web API where scheduled query
# of the PIT tag arrays in the South Fork
# Clearwater are stored

sfc_detections.dat <- vroom(
  file = "https://api.ptagis.org/reporting/reports/efelts60/file/SFC%20Detections.csv",
  delim = ",",
  locale = locale(encoding = "UTF-16LE")
)

current_sy_key <- tibble(species = c("Chinook", "Bull Trout", "Pacific Lamprey", "Cutthroat Trout", "Steelhead")) %>%
  mutate(current_sy = case_when(
    yday(today()) >= 183 & species == "Steelhead" ~ year(today()) + 1,
    TRUE ~ year(today())
  ))

sfc_detections.test <- sfc_detections.dat %>%
  mutate(
    observation_sitecode = word(`Site`, 1, sep = " "),
    release_sitecode = word(`Release Site`, 1, sep = " "),
    observation_datetime = as.POSIXct(`Obs Time`,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "America/Los_Angeles"
    ),
    observation_month = month(observation_datetime),
    observation_year = year(observation_datetime),
    spawn_year = ifelse(observation_month > 6, (observation_year + 1),
      observation_year
    ),
    release_datetime = mdy(`Release Date`),
    release_year = year(release_datetime),
    yrs_at_large = observation_year - release_year,
    species = `Species Name`
  ) %>%
  select(
    pit_id = Tag, rear_type = `Rear Type Code`,
    release_sitecode, release_lifestage = `Mark Life Stage`,
    release_datetime, release_year, observation_sitecode,
    observation_datetime, observation_month,
    observation_year, yrs_at_large, spawn_year,
    species,
    length_mm = `Mark Length`
  ) |>
  mutate(spawn_year = case_when(
    yday(observation_datetime) >= 183 & species == "Steelhead" ~ observation_year + 1,
    TRUE ~ observation_year
  )) |>
  mutate(most_recent = max(spawn_year, na.rm = T)) %>%
  left_join(current_sy_key, by = "species") %>%
  filter(spawn_year == current_sy)

# read in data from Web API for steelhead detected
# downstream

sfc_sthd_detections.dat <- vroom(
  file = "https://api.ptagis.org/reporting/reports/efelts60/file/SFC%20STHD%20Downstream.csv",
  delim = ",",
  locale = locale(encoding = "UTF-16LE")
)

sfc_others_detections.dat <- vroom(
  file = "https://api.ptagis.org/reporting/reports/efelts60/file/SFC%20Others%20Downstream.csv",
  delim = ",",
  locale = locale(encoding = "UTF-16LE")
)


sfc_downstream_detections.dat <- bind_rows(
  sfc_sthd_detections.dat,
  sfc_others_detections.dat
) %>%
  mutate(
    observation_sitecode = word(`Site`, 1, sep = " "),
    release_sitecode = word(`Release Site`, 1, sep = " "),
    observation_datetime = as.POSIXct(`Obs Time`,
      format = "%m/%d/%Y %I:%M:%S",
      tz = "America/Los_Angeles"
    )
  ) %>%
  group_by(Tag) %>%
  slice(which.max(observation_datetime)) %>%
  select(
    pit_id = Tag,
    latest_downstream = observation_datetime
  )


# pull out detections in SFC that were marked
# as juveniles and drop any that don't appear in the
# downstream detections prior to their latest detection
# in SFC

sfc_juvenile.filter <- sfc_detections.test %>%
  filter(release_lifestage == "Juvenile") %>%
  group_by(pit_id) %>%
  slice(which.max(observation_datetime)) %>%
  inner_join(sfc_downstream_detections.dat, by = "pit_id") %>%
  filter(latest_downstream < observation_datetime)

# now summarize releavant values and pull in marking location as well

dat.mark <- sfc_detections.test %>%
  group_by(pit_id) %>%
  summarize(
    species = first(species),
    spawn_year = first(spawn_year),
    release_sitecode = first(release_sitecode),
    release_datetime = first(release_datetime)
  )

sfc_individuals.summary <- sfc_detections.test %>%
  filter(pit_id %in% sfc_juvenile.filter$pit_id |
    release_lifestage == "Adult" |
    release_sitecode %in% c("LGRRBR", "LGRRRR")) %>%
  group_by(pit_id) %>%
  summarize(
    sfc_first = first(observation_datetime),
    sfc_entry_final = last(observation_datetime[observation_sitecode %in% c("SC1", "SC2")]),
    sfc_diff = as.numeric(sfc_entry_final - sfc_first, units = "days"),
    length_mm = mean(length_mm, na.rm = T),
    release_lifestage = first(release_lifestage)
  ) %>%
  left_join(dat.mark, by = "pit_id") %>%
  mutate(sfc_entry_final = ifelse(is.na(sfc_entry_final),
    sfc_first, sfc_entry_final
  ))

sfc_entry.summary <- sfc_individuals.summary %>%
  mutate(sfc_final_date = as.POSIXct(sfc_entry_final)) %>%
  group_by(sfc_final_date, spawn_year, species) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(spawn_year, species) %>%
  mutate(
    sy_total = sum(n),
    cumulative_total = cumsum(n),
    daily_prop = n / sy_total,
    daily_cumulative = cumsum(daily_prop)
  )


# now also grab the SFC water data from USGS gaging staion

stites.site <- "13338500"

# temp is coded as 00010, discharge as 00060,
# we'll go for both of those

parm.cd <- c("00010", "00060")

# put today's date into text format to
# feed into the query of daily water data

today.text <- as.character(today(tz = "America/Los_Angeles"))

sfc.daily <- readNWISdv(
  siteNumber = stites.site,
  parameterCd = parm.cd,
  startDate = "1990-01-01",
  endDate = today.text
) %>%
  select(
    date = Date,
    mean_temp = X_00010_00003,
    mean_discharge = X_00060_00003
  ) %>%
  mutate(date = as_date(date))

sfc.dat <- sfc.daily %>%
  filter(
    date >= min(sfc_entry.summary$sfc_final_date, na.rm = T),
    date <= today()
  ) %>%
  mutate(group = 1)

# save relevant files as RDS format

saveRDS(sfc.dat, "data/sfc_flow")

saveRDS(
  sfc_individuals.summary,
  "data/individuals"
)

saveRDS(
  sfc_entry.summary,
  "data/daily"
)

# helper df for limits on max for dummy

species_max_dates <- tibble(
  species = c(
    "Bull Trout", "Chinook", "Steelhead",
    "Pacific Lamprey", "Cutthroat Trout"
  ),
  max_date = as.Date(c(
    "1976-12-31", "1976-12-31",
    "1977-06-30", "1976-12-31",
    "1976-12-31"
  ))
)

# need to set minimum dates also so that each observed year
# spans all the potential dates, i.e. earliest
# that fish have been observed

species_min_dates <- readRDS("data/daily_completed") |>
  mutate(
    day_of_year = yday(sfc_final_date),
    dummy_date = case_when(
      species == "Steelhead" & day_of_year < 183 ~ as.Date(day_of_year, origin = "1977-01-01"),
      TRUE ~ as.Date(day_of_year - 1, origin = "1976-01-01")
    )
  ) |>
  group_by(species) |>
  summarize(min_dummy = min(dummy_date))

# bring in completed run year data to build
# projections for current year based on
# run-to-date numbers

complete_daily <- readRDS("data/daily_completed") %>%
  ungroup() %>%
  mutate(
    day_of_year = yday(sfc_final_date),
    dummy_date = case_when(
      species == "Steelhead" & day_of_year < 183 ~ as.Date(day_of_year, origin = "1977-01-01"),
      TRUE ~ as.Date(day_of_year - 1, origin = "1976-01-01")
    )
  ) %>%
  left_join(species_max_dates, by = "species") %>%
  left_join(species_min_dates, by = "species") %>%
  group_by(species, spawn_year) %>%
  complete(dummy_date = seq(min(min_dummy), max(max_date), by = "day")) %>%
  ungroup() %>%
  select(-c(min_dummy, max_date)) %>%
  mutate(across(n, ~ replace_na(.x, 0))) %>%
  mutate(across(daily_prop, ~ replace_na(.x, 0))) |>
  group_by(species, spawn_year) |>
  fill(c(
    "sy_total", "cumulative_total", "daily_prop",
    "daily_cumulative"
  ), .direction = "down")

# get estimates of how much of the run has been
# completed on a given day of the year for use
# in estimating what the total will be based
# on year-to-date numbers in given spawn year

complete_reference <- complete_daily %>%
  group_by(dummy_date, species) %>%
  summarize(
    median_cum = median(daily_cumulative, na.rm = T),
    min_cum = min(daily_cumulative, na.rm = T),
    max_cum = max(daily_cumulative, na.rm = T),
    min_dailyprop = min(daily_prop, na.rm = T),
    median_dailyprop = median(daily_prop, na.rm = T),
    max_daily_prop = max(daily_prop, na.rm = T)
  )

# get the complete set of dates for current spawn year data as well

complete_current <- sfc_entry.summary %>%
  mutate(
    day_of_year = yday(sfc_final_date),
    dummy_date = case_when(
      species == "Steelhead" & day_of_year < 183 ~ as.Date(day_of_year, origin = "1977-01-01"),
      TRUE ~ as.Date(day_of_year - 1, origin = "1976-01-01")
    )
  ) %>%
  left_join(species_max_dates, by = "species") |>
  group_by(species) |>
  mutate(min_date = min(dummy_date, na.rm = T)) |>
  complete(dummy_date = seq(min(min_date), max(max_date), by = "day")) |>
  ungroup() |>
  select(-c(min_date, max_date)) |>
  mutate(across(n, ~ replace_na(.x, 0))) |>
  mutate(across(daily_prop, ~ replace_na(.x, 0))) %>%
  group_by(species) |>
  fill(c(
    "sy_total", "cumulative_total", "daily_prop",
    "daily_cumulative", "spawn_year"
  ), .direction = "down")

alldaily <- complete_daily %>%
  select(spawn_year, sfc_final_date, n, dummy_date) %>%
  bind_rows(complete_current) |>
  group_by(spawn_year, species) |>
  mutate(daily_cumulative_n = cumsum(n))

run_stats <- alldaily |>
  left_join(current_sy_key, by = "species") |>
  filter(
    spawn_year > 2017,
    spawn_year < current_sy
  ) |>
  group_by(spawn_year, species) |>
  mutate(
    total_n = sum(n),
    prop_complete = daily_cumulative_n / total_n
  ) |>
  group_by(dummy_date, species) |>
  summarize(
    median_percentcomplete = round(median(prop_complete) * 100),
    min_percentcomplete = round(min(prop_complete) * 100),
    max_percentcomplete = round(max(prop_complete) * 100)
  )

# save additional parts to include in the shiny app

saveRDS(run_stats, "data/run_stats")
saveRDS(alldaily, "data/alldaily")
