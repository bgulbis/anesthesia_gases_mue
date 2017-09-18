library(tidyverse)
library(stringr)
library(edwr)

dir_raw <- "data/raw"

# run MBO query:
#   * Patients - by Clinical Event
#       - Clinical Event: Inspired Desflurane - Anes, Inspired Isoflurane -
#       Anes, Inspired Sevoflurne - Anes
#       - Date Only - Admit: 8/1/17 - 9/1/17

pts_mbo <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age >= 18)

pts_id <- concat_encounters(pts_mbo$millennium.id, 1000)

# Run MBO query:
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: Inspired Desflurane - Anes, Inspired Isoflurane -
#       Anes, Inspired Sevoflurne - Anes

gases <- read_data(dir_raw, "gas-type", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate_at("event", str_replace_all, pattern = "inspired | - anes", replacement = "") %>%
    distinct(millennium.id, event)

x <- count(gases, event)

# Run MBO query:
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: Inspired Desflurane - Anes, Inspired Isoflurane -
#       Anes, Inspired Sevoflurne - Anes, Expired Desflurane - Anes, Expired
#       Isoflurane - Anes, Expired Sevoflurne - Anes, N2O - Anes, Fi N2O - Anes,
#       Expired N2O - Anes
