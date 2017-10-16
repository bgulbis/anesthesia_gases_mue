library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)

dir_raw <- "data/raw"

demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

surgeries <- read_data(dir_raw, "surgeries") %>%
    as.surgeries() %>%
    left_join(id[c("pie.id", "millennium.id")], by = "pie.id")

gases <- c("")

events <- read_data(dir_raw, "events", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate_at("event", str_replace_all, pattern = "inspired | - anes", replacement = "")

gas_flow <- events %>%
    mutate_at("event.result", as.numeric) %>%
    select(millennium.id:event.result) %>%
    distinct() %>%
    group_by(millennium.id, event.datetime) %>%
    spread(event, event.result) %>%
    mutate(fresh_gas = sum(air, n2o, o2, na.rm = TRUE)) %>%
    left_join(surgeries[c("millennium.id", "surg.start.datetime", "surg.stop.datetime")], by = "millennium.id") %>%
    filter(event.datetime >= surg.start.datetime,
           event.datetime <= surg.stop.datetime) %>%
    group_by(millennium.id, surg.start.datetime) %>%
    mutate(run_time = as.numeric(difftime(event.datetime, first(event.datetime), units = "min")),
           time_group = case_when(run_time < 15 ~ 1,
                                  run_time < 30 ~ 2,
                                  run_time < 45 ~ 3,
                                  run_time < 60 ~ 4,
                                  TRUE ~ 4 + floor(run_time / 60))) %>%
    group_by(millennium.id, surg.start.datetime, time_group) %>%
    gather(gas, flow, desflurane, isoflurane, sevoflurane) %>%
    filter(!is.na(flow)) %>%
    group_by(millennium.id, surg.start.datetime, time_group, gas) %>%
    summarize_at("flow", mean, na.rm = TRUE)

diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis()

scr <- read_data(dir_raw, "labs-scr", FALSE) %>%
    as.labs() %>%
    tidy_data()

weight <- read_data(dir_raw, "weight", FALSE) %>%
    as.measures()

