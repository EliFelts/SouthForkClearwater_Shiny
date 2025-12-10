# script to build data from previous years and use for
# comparison with current run year

library(stringr)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(plotly)
library(lubridate)
library(dataRetrieval)


# Read in all detections on the South Fork array,
# drop any that were tagged as juveniles and have
# been at large for less than a year; right now just
# going back to 2018 bc that's about the time the current
# angler brood collected Steelhead program started

dat <- read_csv("data/SFC Detections_Comparisons.csv") %>%
  mutate(
    observation_sitecode = word(`Site Name`, 1, sep = " "),
    release_sitecode = word(`Release Site Name`, 1, sep = " "),
    observation_datetime = as.POSIXct(`Obs Time Value`,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "America/Los_Angeles"
    ),
    observation_month = month(observation_datetime),
    observation_year = year(observation_datetime),
    spawn_year = case_when(
      yday(observation_datetime) >= 183 & `Species Name` == "Steelhead" ~ observation_year + 1,
      TRUE ~ observation_year
    ),
    release_datetime = mdy(`Release Date MMDDYYYY`),
    release_year = year(release_datetime),
    yrs_at_large = observation_year - release_year
  ) %>%
  select(
    pit_id = `Tag Code`, species = `Species Name`,
    rear_type = `Rear Type Code`,
    run_type = `Run Name`,
    release_sitecode, release_lifestage = `Mark Life Stage Value`,
    release_datetime, release_year,
    observation_sitecode,
    observation_datetime, observation_month,
    observation_year, yrs_at_large,
    spawn_year,
    length_mm = `Mark Length mm`
  ) %>%
  mutate(release_lifestage = case_when(
    is.na(release_lifestage) & length_mm <= 300 &
      species %in% c("Steelhead", "Chinook") ~ "Juvenile",
    is.na(release_lifestage) & length_mm > 300 &
      species %in% c("Steelhead", "Chinook") ~ "Adult",
    release_lifestage == "Adult" & length_mm <= 300 &
      species %in% c("Steelhead", "Chinook") ~ "Juvenile",
    is.na(release_lifestage) & is.na(length_mm) &
      species %in% c("Steelhead", "Chinook") ~ "Juvenile",
    TRUE ~ release_lifestage
  )) %>%
  filter(!(yrs_at_large == 0 & release_lifestage == "Juvenile"))

# Bring in detection data from downstream, basically all the
# Snake and Columbia dam infrastructure and find the last
# detection thes had down there

sfc_downstream.filter1 <- read_csv("data/SFC STHD Downstream_Comparisons_18-21.csv")
sfc_downstream.filter2 <- read_csv("data/SFC STHD Downstream_Comparisons_14-17.csv")
sfc_downstream.filter3 <- read_csv("data/SFC STHD Downstream_Comparisons_22-25.csv")
sfc_downstream.filter4 <- read_csv("data/SFC Others Downstream_Comparisons_18-21.csv")
sfc_downstream.filter5 <- read_csv("data/SFC Others Downstream_Comparisons_14-17.csv")
sfc_downstream.filter6 <- read_csv("data/SFC Others Downstream_Comparisons_22-25.csv")

sfc_downstream.filter <- bind_rows(
  sfc_downstream.filter1, sfc_downstream.filter2,
  sfc_downstream.filter3, sfc_downstream.filter4,
  sfc_downstream.filter5, sfc_downstream.filter6
) %>%
  mutate(
    ds_datetime = as.POSIXct(`Obs Time Value`,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "America/Los_Angeles"
    ),
    ds_site = word(`Site Name`, 1, sep = " ")
  ) %>%
  select(
    pit_id = `Tag Code`,
    ds_datetime, ds_site
  ) %>%
  group_by(pit_id) %>%
  slice(which.max(ds_datetime))

# pull out detections in SFC that were marked as juveniles at
# any of the release sites that typically show up in there
# as juveniles, join to downstream detections and drop any
# that don't appear in th downstream detections query; this is
# to make sure to only get migratory fish

sfc_release_sites <- c(
  "NEWSOC", "AMERR", "CROTRP", "REDR",
  "MEAD2C", "CLWRSF", "SFCTRP", "LUGUAF",
  "CROOKR", "CLEARC"
)

sfc_juvenile.filter <- dat %>%
  filter(
    release_lifestage == "Juvenile",
    release_sitecode %in% sfc_release_sites
  ) %>%
  group_by(pit_id) %>%
  slice(which.max(observation_datetime)) %>%
  inner_join(sfc_downstream.filter, by = "pit_id") %>%
  filter(observation_datetime > ds_datetime) %>%
  ungroup() %>%
  select(-c(ds_datetime, ds_site))

# so now, ultimately, want to keep any that were marked as
# adults, those that passed the juvenile filter, and
# additional juveniles that were marked in the hydrosystem

summary.dat <- dat %>%
  filter(release_lifestage == "Adult" |
    release_sitecode %in% c("LGRRBR", "LGRRRR")) %>%
  bind_rows(sfc_juvenile.filter)

# Now summarize relevant values and pull in marking locations as well

dat.mark <- summary.dat %>%
  group_by(pit_id) %>%
  summarize(
    release_sitecode = first(release_sitecode),
    release_datetime = first(release_datetime)
  )

sfcindividuals_completedyrs <- summary.dat %>%
  group_by(pit_id, species, spawn_year) %>%
  summarize(
    sfc_first = first(observation_datetime),
    sfc_entry_final = last(observation_datetime[observation_sitecode %in% c("SC1", "SC2")]),
    sfc_diff = as.numeric(sfc_entry_final - sfc_first, units = "days"),
    length_mm = mean(length_mm, na.rm = T),
    release_lifestage = first(release_lifestage)
  ) %>%
  filter(!is.na(spawn_year)) %>%
  mutate(sfc_entry_final = if_else(is.na(sfc_entry_final),
    sfc_first, sfc_entry_final
  )) %>%
  left_join(dat.mark, by = "pit_id")

# now summarize the run progression by day

# make a key for current spawn year by species
# so that can be dropped bc this is only for completed
# years

current_sy_key <- tibble(species = c("Chinook", "Bull Trout", "Pacific Lamprey", "Cutthroat Trout", "Steelhead")) %>%
  mutate(current_sy = case_when(
    yday(today()) >= 183 & species == "Steelhead" ~ year(today()) + 1,
    TRUE ~ year(today())
  ))


sfc_entry_daily <- sfcindividuals_completedyrs %>%
  mutate(sfc_final_date = as_date(sfc_entry_final)) %>%
  group_by(sfc_final_date, spawn_year, species) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(spawn_year, species) %>%
  mutate(
    sy_total = sum(n),
    cumulative_total = cumsum(n),
    daily_prop = n / sy_total,
    daily_cumulative = cumsum(daily_prop)
  ) %>%
  left_join(current_sy_key, by = c("species")) %>%
  filter(!spawn_year == current_sy)

saveRDS(
  sfc_entry_daily,
  "data/daily_completed"
)
