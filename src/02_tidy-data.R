library(tidyverse)
library(edwr)

dir_raw <- "data/raw"

demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

events <- read_data(dir_raw, "events", FALSE) %>%
    as.events(order_var = FALSE)

gas_flow <- events %>%
    mutate_at("event.result", as.numeric) %>%
    group_by(millennium.id, event.datetime) %>%
    summarize_at("event.result", sum, na.rm = TRUE) %>%
    rename(flow_rate = event.result)



diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis()

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

scr <- read_data(dir_raw, "labs-scr", FALSE) %>%
    as.labs() %>%
    tidy_data()

weight <- read_data(dir_raw, "weight", FALSE) %>%
    as.measures()

surgeries <- read_data(dir_raw, "surgeries") %>%
    as.surgeries()
