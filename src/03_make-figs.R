library(tidyverse)
library(themebg)
library(devEMF)
library(lazyeval)

x <- dirr::get_rds("data/tidy")
base <- "" #serif

fig1 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surg.type)) %>%
    mutate_at("surg.type", factor, levels = data_surgery_types$surg.type) %>%
    mutate_at("surg.type", fct_rev) %>%
    ggplot(aes(x = surg.type, fill = gas)) +
    geom_bar() +
    xlab("Surgery Category") +
    ylab("Patients") +
    # scale_fill_brewer("Gas Used", palette = "Dark2") +
    scale_fill_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    coord_flip() +
    theme_bg(yticks = FALSE, base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

fig2 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surgery_duration)) %>%
    mutate_at("surgery_duration", as.numeric) %>%
    ggplot(aes(x = gas, y = surgery_duration)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Surgery Duration (hours)") +
    coord_cartesian(ylim = c(0, 12)) +
    theme_bg(base_family = base, xticks = FALSE) +
    theme(axis.line = element_line(color = "grey35"))

df_cost <- data_gas_realtime %>%
    mutate(hour = ceiling((as.numeric(run_time) + 5) / 60)) %>%
    group_by(millennium.id, surg.start.datetime, gas, hour) %>%
    summarize_at("cost", sum, na.rm = TRUE)

fig3 <- df_cost %>%
    filter(cost > 0) %>%
    ggplot(aes(x = gas, y = cost)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Cost per Hour") +
    theme_bg(base_family = base, xticks = FALSE) +
    theme(axis.line = element_line(color = "grey35"))

fig4 <- df_cost %>%
    filter(cost > 0,
           gas != "desflurane") %>%
    ggplot(aes(x = gas, y = cost)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Cost per Hour") +
    theme_bg(base_family = base, xticks = FALSE) +
    theme(axis.line = element_line(color = "grey35"))

cost_medians <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surg.type)) %>%
    group_by(surg.type) %>%
    summarize_at("cost", median, na.rm = TRUE) %>%
    arrange(desc(cost))

fig5 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surg.type)) %>%
    mutate_at("surg.type", factor, levels = cost_medians$surg.type) %>%
    mutate_at("surg.type", fct_rev) %>%
    ggplot(aes(x = surg.type, y = cost)) +
    geom_boxplot() +
    xlab("Surgery Type") +
    ylab("Cost ($)") +
    coord_flip(ylim = c(0, 15)) +
    theme_bg(base_family = base, yticks = FALSE) +
    theme(axis.line = element_line(color = "grey35"))

fig6 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surgery_duration)) %>%
    mutate_at("surgery_duration", as.numeric) %>%
    ggplot(aes(x = surgery_duration, y = cost)) +
    geom_point(aes(shape = gas), color = "#636363") +
    geom_smooth(aes(linetype = gas), color = "black", se = FALSE, method = "lm") +
    xlab("Surgery Duration (hours)") +
    ylab("Total Gas Cost") +
    scale_linetype("Gas Used") +
    scale_shape_manual("Gas Used", values = c(0, 1, 2)) +
    # scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

fig7 <- fig6 +
    coord_cartesian(xlim = c(0, 7.5), ylim = c(0, 25))

fig8 <- data_gas %>%
    left_join(data_patients, by = "millennium.id") %>%
    filter(!is.na(scr_prior_surg)) %>%
    ggplot(aes(x = gas, y = scr_prior_surg)) +
    geom_hline(yintercept = 1.5, color = "grey50") +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Serum creatinine (mg/dL)") +
    coord_cartesian(ylim = c(0, 5)) +
    theme_bg(base_family = base, xticks = FALSE) +
    theme(axis.line = element_line(color = "grey35"))

fig9 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = flow, linetype = gas)) +
    geom_smooth(color = "black") +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 2)) +
    scale_y_continuous("Gas Concentration (%)", breaks = seq(0, 20, 2)) +
    scale_linetype("Gas Used") +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

fig10 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = flow, linetype = gas)) +
    geom_smooth(color = "black") +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 0.5)) +
    scale_y_continuous("Gas Concentration (%)", breaks = seq(0, 20, 1)) +
    scale_linetype("Gas Used") +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 6)) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

fig11 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = fresh_gas, linetype = gas)) +
    geom_smooth(color = "black") +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 2)) +
    scale_y_continuous("Fresh Flow Rate", breaks = seq(-20, 20, 0.5)) +
    scale_linetype("Gas Used") +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

fig12 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = fresh_gas, linetype = gas)) +
    geom_smooth(color = "black") +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 0.5)) +
    scale_y_continuous("Fresh Flow Rate", breaks = seq(0, 20, 0.5)) +
    # scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    scale_linetype("Gas Used") +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 4)) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"),
          axis.line = element_line(color = "grey35"))

save_all <- function(x, w = 6, h = 4) {
    nm <- sprintf("figs/fig_%02d.emf", x)
    emf(nm, width = w, height = h)
    y <- get(paste0("fig", x))
    print(y)
    dev.off()
}

map(1:12, save_all)
