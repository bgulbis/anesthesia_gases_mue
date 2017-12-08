library(tidyverse)
library(themebg)

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
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig1.jpg", fig1, device = "jpeg", width = 6, height = 4, units = "in")

fig2 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surgery_duration)) %>%
    mutate_at("surgery_duration", as.numeric) %>%
    ggplot(aes(x = gas, y = surgery_duration)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Surgery Duration (hours)") +
    coord_cartesian(ylim = c(0, 12)) +
    theme_bg(base_family = base, xticks = FALSE)

ggsave("figs/fig2.jpg", fig2, device = "jpeg", width = 6, height = 4, units = "in")

fig3 <- data_gas %>%
    ggplot(aes(x = gas, y = cost)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Cost") +
    theme_bg(base_family = base)

ggsave("figs/fig3.jpg", fig3, device = "jpeg", width = 6, height = 4, units = "in")

df_cost <- data_gas_realtime %>%
    mutate(hour = ceiling((as.numeric(run_time) + 5) / 60)) %>%
    group_by(millennium.id, surg.start.datetime, gas, hour) %>%
    summarize_at("cost", sum, na.rm = TRUE)

fig4 <- df_cost %>%
    filter(cost > 0) %>%
    ggplot(aes(x = gas, y = cost)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Cost per Hour") +
    theme_bg(base_family = base, xticks = FALSE)

ggsave("figs/fig4.jpg", fig4, device = "jpeg", width = 6, height = 4, units = "in")

fig5 <- df_cost %>%
    filter(cost > 0,
           gas != "desflurane") %>%
    ggplot(aes(x = gas, y = cost)) +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Cost per Hour") +
    theme_bg(base_family = base, xticks = FALSE)

ggsave("figs/fig5.jpg", fig5, device = "jpeg", width = 6, height = 4, units = "in")

cost_medians <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surg.type)) %>%
    group_by(surg.type) %>%
    summarize_at("cost", median, na.rm = TRUE) %>%
    arrange(desc(cost))

fig6 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surg.type)) %>%
    mutate_at("surg.type", factor, levels = cost_medians$surg.type) %>%
    mutate_at("surg.type", fct_rev) %>%
    ggplot(aes(x = surg.type, y = cost)) +
    geom_boxplot() +
    xlab("Surgery Type") +
    ylab("Cost ($)") +
    coord_flip(ylim = c(0, 15)) +
    theme_bg(base_family = base, yticks = FALSE)

ggsave("figs/fig6.jpg", fig6, device = "jpeg", width = 6, height = 4, units = "in")

fig7 <- data_gas %>%
    left_join(data_surgeries, by = c("millennium.id", "surg.start.datetime")) %>%
    filter(!is.na(surgery_duration)) %>%
    mutate_at("surgery_duration", as.numeric) %>%
    ggplot(aes(x = surgery_duration, y = cost, color = gas)) +
    geom_point(shape = 1) +
    geom_smooth(se = FALSE, method = "lm") +
    xlab("Surgery Duration (hours)") +
    ylab("Total Gas Cost") +
    scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig7.jpg", fig7, device = "jpeg", width = 6, height = 4, units = "in")

fig8 <- fig7 +
    coord_cartesian(xlim = c(0, 7.5), ylim = c(0, 25))

ggsave("figs/fig8.jpg", fig8, device = "jpeg", width = 6, height = 4, units = "in")

fig9 <- data_gas %>%
    left_join(data_patients, by = "millennium.id") %>%
    filter(!is.na(scr_prior_surg)) %>%
    ggplot(aes(x = gas, y = scr_prior_surg)) +
    geom_hline(yintercept = 1.5, color = "grey50") +
    geom_boxplot() +
    xlab("Gas Used") +
    ylab("Serum creatinine (mg/dL)") +
    coord_cartesian(ylim = c(0, 5)) +
    theme_bg(base_family = base, xticks = FALSE)

ggsave("figs/fig9.jpg", fig9, device = "jpeg", width = 6, height = 4, units = "in")

fig10 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = flow, color = gas)) +
    geom_smooth() +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 2)) +
    scale_y_continuous("Gas Concentration (%)", breaks = seq(0, 20, 2)) +
    scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig10.jpg", fig10, device = "jpeg", width = 6, height = 4, units = "in")

fig11 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = flow, color = gas)) +
    geom_smooth() +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 0.5)) +
    scale_y_continuous("Gas Concentration (%)", breaks = seq(0, 20, 1)) +
    scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 6)) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig11.jpg", fig11, device = "jpeg", width = 6, height = 4, units = "in")

fig12 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = fresh_gas, color = gas)) +
    geom_smooth() +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 2)) +
    scale_y_continuous("Fresh Flow Rate", breaks = seq(-20, 20, 2)) +
    scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig12.jpg", fig12, device = "jpeg", width = 6, height = 4, units = "in")

fig13 <- data_gas_realtime %>%
    mutate_at("run_time", as.numeric) %>%
    mutate_at("run_time", funs(. / 60)) %>%
    ggplot(aes(x = run_time, y = fresh_gas, color = gas)) +
    geom_smooth() +
    scale_x_continuous("Surgery Duration (hours)", breaks = seq(0, 20, 0.5)) +
    scale_y_continuous("Fresh Flow Rate", breaks = seq(0, 20, 1)) +
    scale_color_manual("Gas Used", values = c("#252525", "#636363", "#969696")) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 8)) +
    theme_bg(base_family = base) +
    theme(legend.title = element_text(color = "grey35"))

ggsave("figs/fig13.jpg", fig13, device = "jpeg", width = 6, height = 4, units = "in")
