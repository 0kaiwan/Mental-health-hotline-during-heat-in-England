##----plot meta analysis results----
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)

var_name <- c("tas", "ssrd")
var_name_lab <- c("temperature", "solar radiation")
RR_file <- c(
  tas = "RR_tas-lag3-cen16.4_ssrd-lag3-cen100_tas.csv",
  ssrd = "RR_tas-lag3-cen16.4_ssrd-lag3-cen100_ssrd.csv"
)

var_full <- c("Temperature", "Solar radiation")

season <- c("warm", "cold")
month_season <- c("May to Sep", "Nov to Mar")
cen <- c(16.4, 100)
p_id <- c("(a)", "(b)")
## ----Figure 1: main model----
# (a) the association between temperature and hotline call, solar radiation controlled
# (b) the association between solar radiation and hotline call, temperature controlled
# ERF
s <- 1 # warm season

p_meta_ls <- list()

for (v in 1:2) { # tas, ssrd
  RR_s <- read.csv(paste0("output/", season[s], " months/", RR_file[v]))
  RR_s$type <- "trust"

  RR_meta <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_", var_name[v], "-cen", cen[v], ".csv"))
  RR_meta$MMT <- NULL

  RR_meta$type <- "meta"
  # RR in individual trusts
  RR_var <- subset(RR_s, var == var_name[v])
  RR_var$cen_ssrd <- NULL
  RR_var$cen_tas <- NULL
  RR_var$tas_lag <- NULL
  RR_var$ssrd_lag <- NULL

  RR_var <- rbind(RR_meta, RR_var)

  RR_var$trust <- factor(RR_var$trust, levels = unique(RR_var$trust))
  # Example plotting code
  p_meta_ls[[v]] <- ggplot(RR_var, aes(x = var_value, y = RR, group = trust)) +
    # Add thin lines for individual trusts
    geom_line(
      data = filter(RR_var, type == "trust"),
      linewidth = 0.3, # thin lines
      color = "blue",
      linetype = 2
    ) +
    # Add ribbon for meta CI
    geom_ribbon(
      data = filter(RR_var, type == "meta"),
      aes(ymin = RR_CIl, ymax = RR_CIh),
      fill = "grey",
      alpha = 0.7
    ) +
    # Add thick line for meta
    geom_line(
      data = filter(RR_var, type == "meta"),
      linewidth = 1.2, # thicker line
      color = "black"
    ) +
    geom_hline(yintercept = 1) +
    theme_minimal(base_size = 14) +
    labs(
      x = paste0(var_name_lab[v], " percentile"), y = "relative risk",
      title = paste0(p_id[v], " Daily ", var_full[v], " and mental health call numbers, ", month_season[s])
    ) +
    theme(text = element_text(size = 16))
}
p_meta <- plot_grid(p_meta_ls[[1]], p_meta_ls[[2]], ncol = 1)

ggsave(
  filename = paste0("ERF_meta_May-Sep.jpeg"),
  path = paste0("./output/", season[s], " months/"),
  plot = p_meta, width = 9, height = 9, limitsize = FALSE
)
ggsave(
  filename = paste0("ERF_meta_May-Sep.pdf"),
  path = paste0("./output/", season[s], " months/"),
  plot = p_meta, width = 9, height = 9, limitsize = FALSE
)
## ----Figure 2. sensitivity exc ssrd----
# the association between temperature and hotline call with and without the control for solar radiation
s <- 1
v <- 1
RR_meta_excssrd <- read.csv(paste0("output/", season[s], " months/sensitivity_analysis/meta_tas-lag3_ssrd-exc_tas-cen", 25.7, ".csv"))
RR_meta_excssrd$trust <- NULL
RR_meta_excssrd$tas_lag <- NULL
RR_meta_incssrd <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_", var_name[v], ".csv"))
RR_meta_incssrd$ssrd <- "inc-ssrd"
RR_meta_incssrd$trust <- NULL
RR_meta <- rbind(RR_meta_incssrd, RR_meta_excssrd)

p_meta_ssrd_sensitivity <- ggplot(RR_meta, aes(x = var_value, y = RR)) +
  # Add thin lines for individual trusts
  geom_line(
    linewidth = 1,
    aes(color = ssrd)
  ) +
  # Add ribbon for meta CI
  geom_ribbon(
    aes(ymin = RR_CIl, ymax = RR_CIh, fill = ssrd),
    alpha = 0.4
  ) +
  scale_color_manual(
    name = "Solar radiation adjustment",
    values = c("inc-ssrd" = "#E69F00", "exc-ssrd" = "#0072B2"),
    breaks = c("inc-ssrd", "exc-ssrd"),
    labels = c("Adjusted", "Not adjusted")
  ) +
  scale_fill_manual(
    name = "Solar radiation adjustment",
    values = c("inc-ssrd" = "#E69F00", "exc-ssrd" = "#0072B2"),
    breaks = c("inc-ssrd", "exc-ssrd"),
    labels = c("Adjusted", "Not adjusted")
  ) +
  geom_hline(yintercept = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0(var_name_lab[v], " percentile"), y = "relative risk",
    title = paste0("Temperature and mental health calls, May to September\nwith and without solar radiation adjustment")
  ) +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggsave(
  filename = paste0("Figure2_ERF_meta_May-Sep_inc-exc-ssrd.jpeg"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_ssrd_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)
ggsave(
  filename = paste0("Figure2_ERF_meta_May-Sep_inc-exc-ssrd.pdf"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_ssrd_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)

##----Figure S2: sensitivity exc ssrd trusts----
RR_tas_inc_ssrd <- read.csv("./output/warm months/RR_tas-lag3-cen16.4_ssrd-lag3-cen100_tas.csv")
RR_tas_exc_ssrd <- read.csv("./output/warm months/sensitivity_analysis/RR_tas-lag3-cen16.4_exc-ssrd_tas.csv")
RR_tas_inc_ssrd <- RR_tas_inc_ssrd[, -c(6, 7)]
RR_tas_all <- rbind(RR_tas_inc_ssrd, RR_tas_exc_ssrd)

lookup_trust <- read.csv("input/lookup_trust_NHSRegion.csv")
RR_tas_all <- left_join(RR_tas_all, lookup_trust[, c(1, 5)])
colnames(RR_tas_all)[c(3, 5)] <- c("sun", "tas")
RR_tas_all$var <- NULL

RR_tas_all <- arrange(RR_tas_all, NHSER24NM, trust, tas)
# Assign unique IDs to each group
RR_tas_all <- RR_tas_all %>%
  group_by(NHSER24NM) %>%
  mutate(trust_id = dense_rank(trust)) %>%
  ungroup()

RR_tas_all$region_trust <- paste0(RR_tas_all$NHSER24NM, "_", RR_tas_all$trust_id)

RR_tas_all$sun <- ifelse(RR_tas_all$sun == 3, "Adjusted", "Not adjusted")


p_tas <- ggplot(data = RR_tas_all) +
  geom_line(aes(tas, RR, colour = sun), linewidth = 1) +
  geom_ribbon(aes(x = tas, ymin = RR_CIl, ymax = RR_CIh, fill = sun), alpha = 0.4) +
  facet_wrap(~region_trust) +
  scale_color_manual(
    name = "Solar radiation adjustment",
    values = c("Adjusted" = "#E69F00", "Not adjusted" = "#0072B2"),
  ) +
  scale_fill_manual(
    name = "Solar radiation adjustment",
    values = c("Adjusted" = "#E69F00", "Not adjusted" = "#0072B2"),
  ) +
  labs(y = "relative risk", x = "temperature percentile",
       title = paste0("Temperature and mental health calls, May to September\nwith and without solar radiation adjustment")
  ) +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_tas,
  filename = "FigureS2_ERF_25trust_tas_inc-exc-ssrd_regionid.jpeg", path = "./output/warm months/sensitivity_analysis/",
  device = "jpeg", width = 10, height = 10, dpi = 600
)
ggsave(
  plot = p_tas,
  filename = "FigureS2_ERF_25trust_tas_inc-exc-ssrd_regionid.pdf", path = "./output/warm months/sensitivity_analysis/",
  device = "pdf", width = 10, height = 10, dpi = 600
)
## ----Figure 3: sensitivity lag 0-3----
# ERF
# warm only
s <- 1
RR_meta <- read.csv(paste0("output/", season[s], " months/sensitivity_analysis/meta_tas-lag0-3_ssrd-lag0-3_cen-MMT.csv"))
colnames(RR_meta)[9] <- "type"
RR_meta$lag_tas <- as.character(RR_meta$lag_tas)
RR_meta$lag_ssrd <- as.character(RR_meta$lag_ssrd)

for (v in 1:2) {
  variable_n <- var_name[v]
  RR_s <- subset(RR_meta, var == variable_n)
  colnames(RR_s)[8 - v] <- "lag_confound"
  colnames(RR_s)[5 + v] <- "lag"
  
  RR_s <- subset(RR_s, lag_confound == "3")
  # Example plotting code
  p_meta <- ggplot(RR_s, aes(x = var_value, y = RR)) +
    # Add thin lines for individual trusts
    geom_line(
      linewidth = 1, # thin lines
      aes(color = lag)
    ) +
    geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = lag)) +
    scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#A52A2A")) +
    scale_fill_manual(values = alpha(c("#56B4E9", "#009E73", "#E69F00", "#A52A2A"), 0.3)) +
    geom_hline(yintercept = 1) +
    theme_minimal(base_size = 14) +
    labs(
      x = paste0(var_name_lab[v], " percentile"), y = "relative risk",
      title = paste0(var_full[v], " and mental health call, May to September")
    ) +
    theme(
      text = element_text(size = 16),
      legend.position = "bottom"
    )
  ggsave(
    filename = paste0("Figure3_ERF_meta_May-Sep_", variable_n, "-lag0-3.jpeg"),
    path = paste0("./output/", season[s], " months/"),
    plot = p_meta, width = 9, height = 5, limitsize = FALSE
  )
  ggsave(
    filename = paste0("Figure3_ERF_meta_May-Sep_", variable_n, "-lag0-3.pdf"),
    path = paste0("./output/", season[s], " months/"),
    plot = p_meta, width = 9, height = 5, limitsize = FALSE
  )
}
## ----Figure 4: sensitivity COVD19----
# the association between temperature and hotline call in the whole period
# vs. in the post-COVID19 period
# warm only
s <- 1
v <- 1
RR_meta <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_tas-cen16.4.csv"))
RR_meta$trust <- NULL
RR_meta$tas_lag <- NULL
RR_meta$MMT <- NULL
RR_meta$period <- "including COVID19 period"

RR_meta_post <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_tas-cen16.4_postCOVID.csv"))
RR_meta_post$trust <- NULL
RR_meta_post$cen <- NULL
RR_meta_post$period <- "excluding COVID19 period"

RR_meta <- rbind(RR_meta, RR_meta_post)

p_meta_COVID_sensitivity <- ggplot(RR_meta, aes(x = var_value, y = RR)) +
  # Add thin lines for individual trusts
  geom_line(
    linewidth = 1,
    aes(color = period)
  ) +
  # Add ribbon for meta CI
  geom_ribbon(
    aes(ymin = RR_CIl, ymax = RR_CIh, fill = period),
    alpha = 0.4
  ) +
  scale_color_manual(values = c("including COVID19 period" = "#E69F00", "excluding COVID19 period" = "#0072B2")) +
  scale_fill_manual(values = c("including COVID19 period" = "#E69F00", "excluding COVID19 period" = "#0072B2")) +
  geom_hline(yintercept = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0(var_full[v], " percentile"), y = "relative risk",
    title = paste0(var_full[v], " and mental health call, ", month_season[s])
  ) +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggsave(
  filename = paste0("Figure4_ERF_meta_May-Sep_COVID_sensitivity.jpeg"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_COVID_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)
ggsave(
  filename = paste0("Figure4_ERF_meta_May-Sep_COVID_sensitivity.pdf"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_COVID_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)
## ----RR along lags----
# ERF
# warm only
s <- 1
RR_meta <- read.csv(paste0("output/", season[s], " months/sensitivity_analysis/meta_tas-lag0-3_ssrd-lag0-3_cen-MMT.csv"))
colnames(RR_meta)[9] <- "type"
# at 90,95,99th temp percentile
RR_99 <- subset(RR_meta, var == "tas" & var_value %in% c(90, 95, 99))
# ssrd lag 3
RR_99 <- subset(RR_99, lag_ssrd == 3)

RR_99$lag_ssrd <- as.character(RR_99$lag_ssrd)
RR_99$var_value <- as.character(RR_99$var_value)
p_lag <- ggplot(RR_99, aes(x = lag_tas, y = RR)) +
  geom_point(aes(x = lag_tas, y = RR, color = var_value)) +
  geom_errorbar(aes(
    x = lag_tas, y = RR, ymin = RR_CIl, ymax = RR_CIh,
    color = var_value
  )) +
  scale_color_manual(values = c("#FFD700", "#FF7F24", "#8B2323"))
theme_minimal(base_size = 14) +
  labs(
    x = "maximum lag", y = "relative risk",
    title = "Temperature and mental health call, ", month_season[s]
  ) +
  theme(text = element_text(size = 16))
# var at 99th percentile tas
RR_99 <- subset(RR_99, var_value == 99)
p_lag_tas99_l <- ggplot(RR_99, aes(x = lag_tas, y = RR)) +
  geom_line(aes(x = lag_tas, y = RR), colour = "#FF7F50") +
  geom_ribbon(aes(x = lag_tas, y = RR, ymin = RR_CIl, ymax = RR_CIh), fill = alpha("#FF7F50", alpha = 0.3)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "maximum lag", y = "cumulative relative risk",
    title = paste0("The RR of mental health call at the 99th percentile\n temperature in the warm season along various lags")
  ) +
  theme(text = element_text(size = 16))
p_lag_tas99_p <- ggplot(RR_99, aes(x = lag_tas, y = RR)) +
  geom_point(aes(x = lag_tas, y = RR), colour = "#FF7F50") +
  geom_errorbar(aes(x = lag_tas, y = RR, ymin = RR_CIl, ymax = RR_CIh), colour = alpha("#FF7F50")) +
  geom_hline(yintercept = 1) +
  theme_minimal(base_size = 12) +
  labs(
    x = "maximum lag of temperature", y = "cumulative relative risk",
    title = paste0("The RR of mental health call at the 99th percentile\n temperature in the warm season along various lags")
  )
ggsave(
  filename = paste0("RR_tas-lag_tas99_ssrd-lag3.jpeg"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_lag_tas99_p, width = 6, height = 3, limitsize = FALSE
)
## ----Figure S3: sensitivity exc. vs. inc rh----
# the association between temperature and hotline call with and without the control for relative humidity
# warm only
var_name <- c("tas", "rh")
s <- 1
v <- 1
RR_file <- c(
  tas = "meta_tas-lag3_rh-lag1_tas-cen23.4.csv",
  rh = "meta_tas-lag3_rh-lag1_rh-cen0.csv"
)

var_full <- c("Temperature", "Relative humidity")
RR_meta_excrh <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_tas-cen16.4.csv"))
RR_meta_excrh$trust <- NULL
RR_meta_excrh$tas_lag <- NULL
RR_meta_excrh$rh <- "Not adjusted"

RR_meta_incrh <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_rh-lag3_", var_name[v], "-cen17.4.csv"))
RR_meta_incrh$rh <- "Adjusted"
RR_meta_incrh$trust <- NULL
RR_meta <- rbind(RR_meta_incrh, RR_meta_excrh)

p_meta_rh_sensitivity <- ggplot(RR_meta, aes(x = var_value, y = RR)) +
  # Add thin lines for individual trusts
  geom_line(
    linewidth = 1,
    aes(color = rh)
  ) +
  # Add ribbon for meta CI
  geom_ribbon(
    aes(ymin = RR_CIl, ymax = RR_CIh, fill = rh),
    alpha = 0.4
  ) +
  scale_color_manual(
    name = "Relative humidity adjustment",
    values = c("Adjusted" = "#E69F00", "Not adjusted" = "#0072B2")) +
  scale_fill_manual(
    name = "Relative humidity adjustment",
    values = c("Adjusted" = "#E69F00", "Not adjusted" = "#0072B2")) +
  geom_hline(yintercept = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0(var_full[v], " percentile"), y = "relative risk",
    title = paste0(var_full[v], " and mental health call, May to September\nwith and without relative humidity adjustment")) +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggsave(
  filename = paste0("FigureS3_ERF_meta_May-Sep_inc-exc-rh_rh-lag3.jpeg"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_rh_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)
ggsave(
  filename = paste0("FigureS3_ERF_meta_May-Sep_inc-exc-rh_rh-lag3.pdf"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_rh_sensitivity, width = 7, height = 5.5, limitsize = FALSE
)

## rh effect
v <- 2
RR_meta_incrh <- read.csv(paste0("output/", season[s], " months/meta_tas-lag3_rh-lag3_", var_name[v], "-cen0.csv"))

p_meta_rh <- ggplot(RR_meta_incrh, aes(x = var_value, y = RR)) +
  # Add thin lines for individual trusts
  geom_line(linewidth = 1) +
  # Add ribbon for meta CI
  geom_ribbon(
    aes(ymin = RR_CIl, ymax = RR_CIh),
    alpha = 0.4
  ) +
  geom_hline(yintercept = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0(var_full[v], " percentile"), y = "relative risk",
    title = paste0(var_full[v], " and mental health call, ", month_season[s], "\n controlled for temperature and solar radiation")
  ) +
  theme(text = element_text(size = 16))
ggsave(
  filename = paste0("ERF_meta_May-Sep_rh-lag3.jpeg"),
  path = paste0("./output/", season[s], " months/sensitivity_analysis"),
  plot = p_meta_rh, width = 9, height = 4.5, limitsize = FALSE
)
