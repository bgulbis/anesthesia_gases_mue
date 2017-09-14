library(tidyverse)
library(edwr)

dir_raw <- "data/raw"

# run MBO query:
#   * Patients - by Clinical Event
#       - Clinical Event: Inspired Desflurane - Anes, Inspired Isoflurane - Anes, Inspired Sevoflurne - Anes
#       - Date Only - Admit: 8/1/17 - 9/1/17

pts_mbo <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

