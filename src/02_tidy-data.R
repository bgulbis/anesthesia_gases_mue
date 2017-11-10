library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)
library(MESS)

dir_raw <- "data/raw"

demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

surgeries <- read_data(dir_raw, "surgeries") %>%
    as.surgeries() %>%
    left_join(id[c("pie.id", "millennium.id")], by = "pie.id")

surg_start <- surgeries %>%
    filter(primary.proc,
           surgery != "L&D Generic Procedure",
           !is.na(surg.stop.datetime)) %>%
    mutate(surgery_duration = difftime(surg.stop.datetime, surg.start.datetime, units = "hours"))

surg_num <- surg_start %>%
    distinct(millennium.id, surg.start.datetime) %>%
    count(millennium.id)

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
    mutate_at("fresh_gas", funs(if_else(. > 100, . / 100, .))) %>%
    left_join(surgeries[c("millennium.id", "surg.start.datetime", "surg.stop.datetime")], by = "millennium.id") %>%
    filter(event.datetime >= surg.start.datetime,
           event.datetime <= surg.stop.datetime) %>%
    distinct()

# gas_summary <- gas_flow %>%
#     group_by(millennium.id, surg.start.datetime) %>%
#     mutate(run_time = as.numeric(difftime(event.datetime, first(event.datetime), units = "min")),
#            time_group = case_when(run_time < 15 ~ 1,
#                                   run_time < 30 ~ 2,
#                                   run_time < 45 ~ 3,
#                                   run_time < 60 ~ 4,
#                                   TRUE ~ 4 + floor(run_time / 60))) %>%
#     group_by(millennium.id, surg.start.datetime, time_group) %>%
#     gather(gas, flow, desflurane, isoflurane, sevoflurane) %>%
#     filter(!is.na(flow)) %>%
#     group_by(millennium.id, surg.start.datetime, time_group, gas) %>%
#     summarize_at(c("flow", "fresh_gas"), mean, na.rm = TRUE) %>%
#     mutate(multiplier = case_when(gas == "desflurane" & time_group <= 4 ~ 0.437,
#                             gas == "desflurane" ~ 1.764,
#                             gas == "sevoflurane" & time_group <= 4 ~ 0.26,
#                             gas == "sevoflurane" ~ 1.061,
#                             gas == "isoflurane" & time_group <= 4 ~ 0.074,
#                             gas == "isoflurane" ~ 0.314))
#
# gas_cost <- gas_summary %>%
#     mutate(cost = flow * fresh_gas * multiplier) %>%
#     group_by(millennium.id, surg.start.datetime, gas) %>%
#     summarize_at("cost", sum, na.rm = TRUE)

gas_real_cost <- gas_flow %>%
    group_by(millennium.id, surg.start.datetime) %>%
    mutate(run_time = difftime(event.datetime, first(event.datetime), units = "min"),
           duration = difftime(lead(event.datetime), event.datetime, units = "min")) %>%
    mutate_at("duration", funs(coalesce(., difftime(surg.stop.datetime, event.datetime, units = "min")))) %>%
    group_by(millennium.id, surg.start.datetime) %>%
    gather(gas, flow, desflurane, isoflurane, sevoflurane) %>%
    filter(!is.na(flow)) %>%
    mutate(mol_wt = case_when(gas == "desflurane" ~ 168,
                              gas == "sevoflurane" ~ 184.5,
                              gas == "isoflurane" ~ 200.1),
           density = case_when(gas == "desflurane" ~ 1.465,
                               gas == "sevoflurane" ~ 1.496,
                               gas == "isoflurane" ~ 1.51),
           cost_ml = case_when(gas == "desflurane" ~ 0.62,
                               gas == "sevoflurane" ~ 0.10,
                               gas == "isoflurane" ~ 0.32),
           cost = (flow * fresh_gas * mol_wt * as.numeric(duration) * cost_ml) / (2412 * density))

gas_real_cost_total <- gas_real_cost %>%
    group_by(millennium.id, surg.start.datetime, gas) %>%
    summarize_at("cost", sum, na.rm = TRUE)

gas_auc <- gas_real_cost %>%
    mutate_at(c("run_time", "duration"), as.numeric) %>%
    group_by(millennium.id, surg.start.datetime, gas) %>%
    summarize(flow_auc = auc(x = run_time, y = flow),
              fresh_auc = auc(x = run_time, y = fresh_gas),
              total_duration = max(run_time, na.rm = TRUE) - min(run_time, na.rm = TRUE)) %>%
    mutate(flow_avg = flow_auc / total_duration,
           fresh_avg = fresh_auc / total_duration)

gas_num <- gas_auc %>%
    filter(flow_auc > 0) %>%
    count(millennium.id, surg.start.datetime)

gas_auc_group <- gas_real_cost %>%
    group_by(millennium.id, surg.start.datetime, gas) %>%
    mutate_at(c("run_time", "duration"), as.numeric) %>%
    mutate(time_group = case_when(run_time < 15 ~ 1,
                                  run_time < 30 ~ 2,
                                  run_time < 45 ~ 3,
                                  run_time < 60 ~ 4,
                                  TRUE ~ 4 + floor(run_time / 60))) %>%
    group_by(millennium.id, surg.start.datetime, gas, time_group) %>%
    summarize(flow_auc = auc(x = run_time, y = flow),
              fresh_auc = auc(x = run_time, y = fresh_gas),
              total_duration = max(run_time, na.rm = TRUE) - min(run_time, na.rm = TRUE)) %>%
    mutate(flow_avg = flow_auc / total_duration,
           fresh_avg = fresh_auc / total_duration)

diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis()

obesity <- filter(diagnosis, diag.type == "FINAL",
                  diag.code %in% c("E66.01", "E66.2")) %>%
    distinct(millennium.id) %>%
    mutate(obesity = TRUE)

osa <- filter(diagnosis, diag.type == "FINAL",
              diag.code %in% c("G47", "G47.3", "G47.33")) %>%
    distinct(millennium.id) %>%
    mutate(osa = TRUE)

scr <- read_data(dir_raw, "labs-scr", FALSE) %>%
    as.labs() %>%
    tidy_data()

scr_admit <- scr %>%
    group_by(millennium.id) %>%
    arrange(lab.datetime, .by_group = TRUE) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, scr_admit = lab.result)

surg_first <- surg_start %>%
    group_by(millennium.id) %>%
    arrange(surg.start.datetime, .by_group = TRUE) %>%
    distinct(millennium.id, .keep_all = TRUE)

scr_surg <- scr %>%
    left_join(surg_first, by = "millennium.id") %>%
    filter(lab.datetime <= surg.start.datetime) %>%
    mutate(scr_surg_diff = difftime(lab.datetime, surg.start.datetime, unit = "days")) %>%
    group_by(millennium.id) %>%
    filter(scr_surg_diff == min(scr_surg_diff)) %>%
    select(millennium.id, scr_prior_surg = lab.result)

weight <- read_data(dir_raw, "weight", FALSE) %>%
    as.measures()

data_patients <- demographics %>%
    select(millennium.id, age, gender) %>%
    left_join(obesity, by = "millennium.id") %>%
    left_join(osa, by = "millennium.id") %>%
    left_join(scr_admit, by = "millennium.id") %>%
    left_join(scr_surg, by = "millennium.id") %>%
    left_join(surg_num, by = "millennium.id") %>%
    rename(num_surgeries = n) %>%
    mutate_at(c("obesity", "osa"), funs(coalesce(., FALSE))) %>%
    filter(!is.na(num_surgeries)) %>%
    left_join(id[c("millennium.id", "fin")], by = "millennium.id") %>%
    select(millennium.id, fin, everything())

data_surgeries <- surg_start %>%
    left_join(gas_num, by = c("millennium.id", "surg.start.datetime")) %>%
    select(millennium.id, surg.type, surgery, surgery_duration, num_gases = n, surg.start.datetime) %>%
    mutate_at("num_gases", funs(coalesce(., 0L))) %>%
    semi_join(data_patients, by = "millennium.id")

data_gas <- gas_real_cost_total %>%
    left_join(gas_auc, by = c("millennium.id", "surg.start.datetime", "gas")) %>%
    select(-flow_auc, -fresh_auc, -total_duration) %>%
    semi_join(data_patients, by = "millennium.id")

data_gas_realtime <- gas_real_cost

data_gas_intervals <- gas_auc_group %>%
    select(-flow_auc, -fresh_auc, -total_duration) %>%
    semi_join(data_patients, by = "millennium.id")

data_surgery_types <- surg_start %>%
    semi_join(data_patients, by = "millennium.id") %>%
    count(surg.type, sort = TRUE)

write.csv(data_patients, "data/external/data_patients.csv", row.names = FALSE)
write.csv(data_surgeries, "data/external/data_surgeries.csv", row.names = FALSE)
write.csv(data_gas, "data/external/data_gas.csv", row.names = FALSE)
write.csv(data_gas_intervals, "data/external/data_gas_intervals.csv", row.names = FALSE)
write.csv(data_surgery_types, "data/external/data_surgery_types.csv", row.names = FALSE)
write.csv(data_gas_realtime, "data/external/data_gas_realtime.csv", row.names = FALSE)

gas_files <- list.files("data/external", pattern = "csv$", full.names = TRUE)

zip(paste0("data/external/mue_anes_gas_data.zip"), gas_files, flags = "-j")

dirr::save_rds("data/tidy", "data_")
