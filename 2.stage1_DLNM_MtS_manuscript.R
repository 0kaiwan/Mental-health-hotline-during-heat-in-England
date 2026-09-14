library(dplyr)
library(tidyr)
library(lubridate)
library(splines)
library(dlnm)
library(data.table)
library(mixmeta)

options(na.action = na.exclude)

data_MtS <- read.csv("output/warm months/hotline_tas_ssrd_cfc_d2m_rh_outlier-lower-NA_MtS.csv")
data_MtS$wday <- factor(data_MtS$wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# re-scale data to percentiles
# Compute Empirical Cumulative Distribution Function
# Percentile for each value
percentile_function <- function(x) {
  ecdf_x <- ecdf(x)
  ecdf_x(x) * 100
}

data_MtS_percentile <- data_MtS %>%
  group_by(trust_id, trust) %>%
  reframe(across(6:12, percentile_function))

data_MtS_percentile <- cbind(data_MtS[c(1:7, 15:19)], data_MtS_percentile[-c(1:2)])

trust_25 <- unique(data_MtS_percentile$trust)

## ----model selection and check residual PACF and AIC----
res_pacf <- function(model, t) {
  res_1 <- residuals(model, type = "deviance")
  pacf(res_1, na.action = na.pass, main = paste0(trust_25[t], ", dlm_", i))
}

# ns of time and lag of hotline call
t <- 1
AIC_ls <- list()
pdf(file = "output/warm months/pacf_25trust_temporal-control.pdf", width = 12, height = 80, onefile = T)
par(mfrow = c(25, 4), mar = c(3, 3, 3, 1))
for (t in 1:25) {
  data_t <- subset(data_MtS_percentile, trust == trust_25[t])
  # one day lag
  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  data_t <- data_t[-1, ]
  cb_tas <- crossbasis(data_t$tas,
    lag = 3, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_ssrd_lag3 <- crossbasis(data_t$ssrd,
    lag = 3, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )

  data_t$year <- as.character(data_t$year)
  if (length(unique(data_t$year)) > 1) {
    dlm_1 <- glm(hotline_count ~ cb_tas + cb_ssrd_lag3 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_1 <- glm(hotline_count ~ cb_tas + cb_ssrd_lag3 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }
  # include a lag1 of hotline call to control for autocorrelation
  if (length(unique(data_t$year)) > 1) {
    dlm_1_lag <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd_lag3 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_1_lag <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd_lag3 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }
  # stronger seasonal control
  if (length(unique(data_t$year)) > 1) {
    dlm_2 <- glm(hotline_count ~ cb_tas + cb_ssrd_lag3 + ns(doy, df = 4) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_2 <- glm(hotline_count ~ cb_tas + cb_ssrd_lag3 + ns(doy, df = 4) + wday,
      data_t,
      family = poisson
    )
  }

  if (length(unique(data_t$year)) > 1) {
    dlm_2_lag <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd_lag3 + ns(doy, df = 4) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_2_lag <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd_lag3 + ns(doy, df = 4) + wday,
      data_t,
      family = poisson
    )
  }

  # if (length(unique(data_t$year)) > 1) {
  #   dlm_3 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=5)*year + wday,
  #                data_t, family = poisson)
  # } else {
  #   dlm_3 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=5) + wday,
  #                data_t, family = poisson)
  # }
  #
  # if (length(unique(data_t$year)) > 1) {
  #   dlm_3_1 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=5):year + wday,
  #                data_t, family = poisson)
  # } else {
  #   dlm_3_1 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=5) + wday,
  #                data_t, family = poisson)
  # }
  #
  # if (length(unique(data_t$year)) > 1) {
  #   dlm_4 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=6)*year + wday,
  #                data_t, family = poisson)
  # } else {
  #   dlm_4 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=6) + wday,
  #                data_t, family = poisson)
  # }
  #
  # if (length(unique(data_t$year)) > 1) {
  #   dlm_5 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=7)*year + wday,
  #                data_t, family = poisson)
  # } else {
  #   dlm_5 <- glm(hotline_count ~ cb_tas + ns(ssrd,df=3) + ns(doy,df=7) + wday,
  #                data_t, family = poisson)
  # }
  #

  AIC_df <- data.frame(
    AIC = AIC(dlm_1, dlm_1_lag, dlm_2, dlm_2_lag),
    model = c("dlm_1", "dlm_1_lag", "dlm_2", "dlm_2_lag"),
    trust = unique(data_t$trust)
  )
  AIC_ls[[t]] <- AIC_df

  dlm_all <- list(dlm_1, dlm_1_lag, dlm_2, dlm_2_lag)

  for (i in 1:4) {
    dlm_i <- dlm_all[[i]]

    res_pacf(dlm_i, t)
  }
}
dev.off()

AIC_df_1 <- rbindlist(AIC_ls)
# write.csv(AIC_df_1,row.names = F,"output/warm months/DLNM_AIC_25trust_1.csv")

# lag of solar exposure and temperature
t <- 1
AIC_ls <- list()
pdf(file = "output/warm months/pacf_25trust_lag.pdf", width = 12, height = 80, onefile = T)
par(mfrow = c(25, 4), mar = c(3, 3, 3, 1))
for (t in 1:25) {
  data_t <- subset(data_MtS_percentile, trust == trust_25[t])
  # one day lag
  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  data_t <- data_t[-1, ]
  cb_tas_lag3 <- crossbasis(data_t$tas,
    lag = 3, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_tas_lag7 <- crossbasis(data_t$tas,
    lag = 7, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )


  cb_ssrd_lag3 <- crossbasis(data_t$ssrd,
    lag = 3, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_ssrd_lag7 <- crossbasis(data_t$ssrd,
    lag = 7, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )

  data_t$year <- as.character(data_t$year)


  if (length(unique(data_t$year)) > 1) {
    dlm_taslag3_ssrdlag3 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag3 + cb_ssrd_lag3 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_taslag3_ssrdlag3 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag3 + cb_ssrd_lag3 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }


  if (length(unique(data_t$year)) > 1) {
    dlm_taslag7_ssrdlag3 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag7 + cb_ssrd_lag3 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_taslag7_ssrdlag3 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag7 + cb_ssrd_lag3 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }

  if (length(unique(data_t$year)) > 1) {
    dlm_taslag3_ssrdlag7 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag3 + cb_ssrd_lag7 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_taslag3_ssrdlag7 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag3 + cb_ssrd_lag7 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }

  if (length(unique(data_t$year)) > 1) {
    dlm_taslag7_ssrdlag7 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag7 + cb_ssrd_lag7 + ns(doy, df = 3) * year + wday,
      data_t,
      family = poisson
    )
  } else {
    dlm_taslag7_ssrdlag7 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas_lag7 + cb_ssrd_lag7 + ns(doy, df = 3) + wday,
      data_t,
      family = poisson
    )
  }

  AIC_df <- data.frame(
    AIC = AIC(dlm_taslag3_ssrdlag3, dlm_taslag7_ssrdlag3, dlm_taslag3_ssrdlag7, dlm_taslag7_ssrdlag7),
    model = c("dlm_taslag3_ssrdlag3", "dlm_taslag7_ssrdlag3", "dlm_taslag3_ssrdlag7", "dlm_taslag7_ssrdlag7"),
    trust = unique(data_t$trust)
  )
  AIC_ls[[t]] <- AIC_df

  dlm_all <- list(dlm_taslag3_ssrdlag3, dlm_taslag7_ssrdlag3, dlm_taslag3_ssrdlag7, dlm_taslag7_ssrdlag7)

  for (i in 1:4) {
    dlm_i <- dlm_all[[i]]

    res_pacf(dlm_i, t)
  }
}
dev.off()

AIC_df_2 <- rbindlist(AIC_ls)
# write.csv(AIC_df_2,row.names = F,"output/warm months/DLNM_AIC_25trust_lag.csv")

# check ERF of alternative models specifications
library(paletteer)
crl_8 <- paletteer_d("ggsci::category10_d3", n = 8)
crl_8_a <- alpha(crl_8, 0.1)

RR_sa <- read.csv("output/warm months/RR_sensitivity_analyses.csv")
lookup_trust <- read.csv("input/lookup_trust_NHSRegion.csv")
RR_sa <- left_join(RR_sa, lookup_trust[, c(1, 5)])

# Assign unique IDs to each group
RR_sa <- RR_sa %>%
  group_by(NHSER24NM) %>%
  mutate(trust_id = dense_rank(trust)) %>%
  ungroup()

RR_sa$region_trust <- paste0(RR_sa$NHSER24NM, "_", RR_sa$trust_id)
RR_sa$sensitivity <- paste0("tas-lag", RR_sa$tas_lag, "_ssrd-lag", RR_sa$ssrd_lag, "_time-df", RR_sa$df_doy)
library(ggplot2)
# tas
p_tas_s_ori_CI <- ggplot(RR_sa[RR_sa$var == "tas", ]) +
  geom_line(aes(var_value, RR, colour = sensitivity)) +
  geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = sensitivity), alpha = 0.2) +
  facet_wrap(~trust, scales = "free_y") +
  scale_colour_manual(values = crl_8) +
  scale_fill_manual(values = crl_8_a) +
  labs(y = "relative risk", x = "temperature percentile (May-Sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_tas_s_ori_CI,
  filename = "ERF_25trust_tas_lag-time-sensitivity_CI_Mts.jpeg", path = "./output/warm months",
  device = "jpeg", width = 12, height = 12, dpi = 600
)
# no CI
p_tas_s_ori <- ggplot(RR_sa[RR_sa$var == "tas", ]) +
  geom_line(aes(var_value, RR, colour = sensitivity)) +
  # geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = sensitivity), alpha = 0.2) +
  facet_wrap(~trust, scales = "free_y") +
  scale_colour_manual(values = crl_8) +
  labs(y = "relative risk", x = "temperature percentile (May_sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_tas_s_ori,
  filename = "ERF_25trust_tas_lag-time-sensitivity_MtS.jpeg", path = "./output/warm months",
  device = "jpeg", width = 12, height = 12, dpi = 600
)

## ssrd
p_ssrd_s_ori_CI <- ggplot(RR_sa[RR_sa$var == "ssrd", ]) +
  geom_line(aes(var_value, RR, colour = sensitivity)) +
  geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = sensitivity), alpha = 0.2) +
  facet_wrap(~trust, scales = "free_y") +
  scale_colour_manual(values = crl_8) +
  scale_fill_manual(values = crl_8_a) +
  labs(y = "relative risk", x = "solar radiation percentile (May-Sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_ssrd_s_ori_CI,
  filename = "ERF_25trust_ssrd_lag-time-sensitivity_CI_Mts.jpeg", path = "./output/warm months",
  device = "jpeg", width = 12, height = 12, dpi = 600
)
# no CI
p_ssrd_s_ori <- ggplot(RR_sa[RR_sa$var == "ssrd", ]) +
  geom_line(aes(var_value, RR, colour = sensitivity)) +
  # geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = sensitivity), alpha = 0.2) +
  facet_wrap(~trust, scales = "free_y") +
  scale_colour_manual(values = crl_8) +
  labs(y = "relative risk", x = "solar radiation percentile (May_sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_ssrd_s_ori,
  filename = "ERF_25trust_ssrd_lag-time-sensitivity_MtS.jpeg", path = "./output/warm months",
  device = "jpeg", width = 12, height = 12, dpi = 600
)

## ----main model: with tas and ssrd----
var_name <- c("tas", "ssrd")
# overall ERF
pdf(file = "output/warm months/ERF_25trust_tas-lag3_ssrd-lag3_tas.pdf", width = 20, height = 20, onefile = T)
pdf(file = "output/warm months/ERF_25trust_tas-lag3_ssrd-lag3_ssrd.pdf", width = 20, height = 20, onefile = T)
# lag-outcome
pdf(file = "output/warm months/ERF_25trust_tas-lag0-3_ssrd-lag3_tas.pdf", width = 20, height = 20, onefile = T)
pdf(file = "output/warm months/ERF_25trust_tas-lag3_ssrd-lag0-3_ssrd.pdf", width = 20, height = 20, onefile = T)

data <- data_MtS_percentile
lag_tas <- 3
lag_ssrd <- 3

cen_tas <- 50
cen_ssrd <- 50

# MMT
cen_tas <- 16.4
cen_ssrd <- 100
v <- 1 # tas
v <- 2 # ssrd
par(mfrow = c(5, 5), mar = c(4, 4, 3, 1))
coef_ls <- list()
RR_ls <- list()
coef_vcov_tas_ls <- list()
coef_vcov_ssrd_ls <- list()
for (t in 1:25) {
  data_t <- subset(data, trust == trust_25[t])

  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  # # heat
  cb_tas <- crossbasis(data_t$tas,
    lag = lag_tas, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_ssrd <- crossbasis(data_t$ssrd,
    lag = lag_ssrd, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )


  data_t$year <- as.character(data_t$year)

  if (length(unique(data_t$year)) > 1) {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) * year + wday,
      data_t,
      family = quasipoisson
    )
  } else {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) + wday,
      data_t,
      family = quasipoisson
    )
  }

  # coefficients
  model_summary <- summary(dlm_1)
  coef_all <- as.data.frame(coef(model_summary))
  colnames(coef_all)[4] <- "p_value"
  coef_all$p_value <- ifelse(coef_all$p_value < 0.001, "<0.001",
    ifelse(coef_all$p_value < 0.01, "<0.01",
      ifelse(coef_all$p_value < 0.05, "<0.01",
        ifelse(coef_all$p_value < 0.1, "<0.1",
          "not significant"
        )
      )
    )
  )
  coef_all <- coef_all %>% mutate(
    trust = trust_25[t],
    sun = "ssrd"
  )
  coef_all$variable <- rownames(coef_all)
  coef_ls[[t]] <- coef_all

  if (v == 1) {
    pred <- crosspred(cb_tas, dlm_1, cumul = T, by = 0.1, cen = cen_tas)
  } else {
    pred <- crosspred(cb_ssrd, dlm_1, cumul = T, by = 0.1, cen = cen_ssrd)
  }

  # plot ERF of tas
  plot(pred, "overall",
    xlab = "temperature pecentile in May to Sep", ylab = "relative risk",
    main = paste0(trust_25[t])
  )
  lag - outcome
  tas
  plot(pred, "slices",
    var = pred$predvar[949], xlab = "lag", ylab = "relative risk",
    main = paste0(trust_25[t])
  )
  # plot ERF of ssrd
  overal
  plot(pred, "overall",
    xlab = "ssrd pecentile in May to Sep", ylab = "relative risk",
    main = paste0(trust_25[t])
  )
  # lag-outcome
  plot(pred, "slices",
    var = pred$predvar[949], xlab = "lag", ylab = "relative risk",
    main = paste0(trust_25[t])
  )

  # extract RR
  RR_ls[[t]] <- data.frame(
    trust = trust_25[t],
    tas_lag = lag_tas,
    ssrd_lag = lag_ssrd,
    var = var_name[v],
    var_value = pred$predvar,
    cen_tas = cen_tas,
    cen_ssrd = cen_ssrd,
    RR = pred$allRRfit,
    RR_CIl = pred$allRRlow,
    RR_CIh = pred$allRRhigh
  )

  # coef and vcov of cb_tas
  pred <- crossreduce(cb_tas, dlm_1, type = "overall")
  coef <- data.frame(t(pred$coefficients))
  vcov <- as.data.frame(t(vechMat(pred$vcov)))
  coef_vcov <- cbind(coef, vcov)
  coef_vcov$trust <- trust_25[t]
  coef_vcov$variable <- "tas"
  coef_vcov_tas_ls[[t]] <- coef_vcov

  # coef and vcov of cb_ssrd
  pred <- crossreduce(cb_ssrd, dlm_1, type = "overall")
  coef <- data.frame(t(pred$coefficients))
  vcov <- as.data.frame(t(vechMat(pred$vcov)))
  coef_vcov <- cbind(coef, vcov)
  coef_vcov$trust <- trust_25[t]
  coef_vcov$variable <- "ssrd"
  coef_vcov_ssrd_ls[[t]] <- coef_vcov
}
dev.off()
coef_all_1 <- rbindlist(coef_ls)
RR_tas_inc_ssrd <- rbindlist(RR_ls)
coef_vcov_tas <- rbindlist(coef_vcov_tas_ls)
coef_vcov_ssrd <- rbindlist(coef_vcov_ssrd_ls)
write.csv(coef_all_1, row.names = F, "./output/warm months/coef_sig_tas-lag3_ssrd-lag3_nsdoy-3df.csv")

write.csv(RR_tas_inc_ssrd, row.names = F, "./output/warm months/RR_tas-lag3-cen16.4_ssrd-lag3-cen100_ssrd.csv")

write.csv(coef_vcov_tas, row.names = F, "./output/warm months/coef_vcov_tas-lag3_ssrd-lag3_tas.csv")
write.csv(coef_vcov_ssrd, row.names = F, "./output/warm months/coef_vcov_tas-lag3_ssrd-lag3_ssrd.csv")

# week of day
coef_all_1 <- read.csv("./output/warm months/coef_sig_tas-lag3_ssrd-lag3_nsdoy-3df.csv")
coef_wday_1 <- coef_all_1[substr(coef_all_1$variable, 1, 4) == "wday", ]
# RR and CI
colnames(coef_wday_1)[2] <- "SE"
RR_wday <- coef_wday_1 %>% mutate(
  RR_wday = exp(Estimate),
  RR_wday_CIl = exp(Estimate - 1.96 * SE),
  RR_wday_CIh = exp(Estimate + 1.96 * SE)
)
RR_wday$variable <- rep(c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 25)
RR_wday$variable <- factor(RR_wday$variable, levels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
library(ggplot2)
p_wday_RR <- ggplot(RR_wday) +
  geom_point(aes(x = variable, y = RR_wday)) +
  geom_errorbar(aes(x = variable, ymin = RR_wday_CIl, ymax = RR_wday_CIh)) +
  scale_y_continuous(breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
  labs(main = "relative risk of hotline calls by day of week, compared to Sunday", x = "day of week", y = "RR") +
  facet_wrap(~trust, scales = "free_x") +
  theme_light() +
  theme(text = element_text(size = 14))
# ggsave(filename = paste0("hotline_wday_dlnm-RR_25trust.jpeg"),
#        path = "./output/warm months/",
#        plot=p_wday_RR,width = 15, height = 15,limitsize = FALSE)

lookup_trust <- read.csv("input/lookup_trust_NHSRegion.csv")
RR_wday <- left_join(RR_wday, lookup_trust[, c(1, 5)])

# Assign unique IDs to each group
RR_wday <- RR_wday %>%
  group_by(NHSER24NM) %>%
  mutate(trust_id = dense_rank(trust)) %>%
  ungroup()

RR_wday$region_trust <- paste0(RR_wday$NHSER24NM, "_", RR_wday$trust_id)

p_wday_CI_regionID <- ggplot(RR_wday, aes(x = variable, y = RR_wday)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = RR_wday_CIl, ymax = RR_wday_CIh),
    width = 0.2
  ) +
  facet_wrap(~region_trust, ncol = 5, scales = "free") +
  theme_light(base_size = 12) +
  labs(
    x = "day of week",
    y = "Relative risk (95% CI)",
    title = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave(
  filename = paste0("hotline_wday_dlnm-RR_25trust_regionID.jpeg"),
  path = "./output/warm months/",
  plot = p_wday_CI_regionID, width = 12, height = 10, limitsize = FALSE
)

data_MtS_wday <- data_MtS %>%
  group_by(wday, trust) %>%
  summarise(
    hotline_count_mean = mean(hotline_count, na.rm = T),
    hotline_count_median = median(hotline_count, na.rm = T),
    hotline_count_Q1 = quantile(hotline_count, 0.25, na.rm = T),
    hotline_count_Q3 = quantile(hotline_count, 0.75, na.rm = T)
  )
# write.csv(data_MtS_wday, row.names = F, "./output/warm months/hotline_count_wday_25trust_IQR.csv")

p_wday <- ggplot(data_MtS) +
  geom_boxplot(aes(x = wday, y = hotline_count)) +
  facet_wrap(~trust, scales = "free", ncol = 5) +
  theme_light() +
  theme(text = element_text(size = 14))
# ggsave(filename = paste0("hotline_count_wday_25trust_IQR.jpeg"),
#        path = "./output/warm months/",
#        plot=p_wday,width = 15, height = 15,limitsize = FALSE)

## confidence interval
data_MtS_wday_CI <- data_MtS %>%
  group_by(trust, wday) %>%
  summarise(
    n = n(),
    mean_log = mean(log(hotline_count), na.rm = TRUE),
    sd_log = sd(log(hotline_count), na.rm = TRUE)
  ) %>%
  mutate(
    se_log = sd_log / sqrt(n),
    # 95% CI on log scale
    CI_low_log = mean_log - 1.96 * se_log,
    CI_high_log = mean_log + 1.96 * se_log,
    # Back-transform to original scale
    mean = exp(mean_log),
    CI_low = exp(CI_low_log),
    CI_high = exp(CI_high_log)
  )
# write.csv(data_MtS_wday_CI, row.names = F, "./output/warm months/hotline_count_wday_25trust_CI.csv")
p_wday_CI <- ggplot(data_MtS_wday_CI, aes(x = wday, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.2
  ) +
  facet_wrap(~trust, ncol = 5, scales = "free") +
  theme_light(base_size = 14) +
  labs(
    x = "day of week",
    y = "Geometric Mean of Daily Calls (95% CI)",
    title = "Mean Mental Health Hotline Calls"
  )
# ggsave(filename = paste0("hotline_count_wday_25trust_CI.jpeg"),
#        path = "./output/warm months/",
#        plot=p_wday_CI,width = 15, height = 15,limitsize = FALSE)



## ----sensitivity analysis: model without ssrd----
RR_ls <- list()
for (t in 1:25) {
  data_t <- subset(data_MtS_percentile, trust == trust_25[t])
  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  cb_tas <- crossbasis(data_t$tas,
    lag = lag_tas, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )

  data_t$year <- as.character(data_t$year)

  if (length(unique(data_t$year)) > 1) {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + ns(doy, df = 3) * year + wday,
      data_t,
      family = quasipoisson
    )
  } else {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + ns(doy, df = 3) + wday,
      data_t,
      family = quasipoisson
    )
  }


  pred <- crosspred(cb_tas, dlm_1, cumul = T, by = 0.1, cen = cen_tas)

  # extract RR
  RR_ls[[t]] <- data.frame(
    trust = trust_25[t],
    tas_lag = lag_tas,
    ssrd_lag = "exc sun",
    var = "tas",
    var_value = pred$predvar,
    RR = pred$allRRfit,
    RR_CIl = pred$allRRlow,
    RR_CIh = pred$allRRhigh
  )
}
RR_tas_exc_ssrd <- rbindlist(RR_ls)
# write.csv(RR_tas_exc_ssrd, row.names = F, paste0("./output/warm months/RR_tas-lag3-cen",cen_tas,"_exc-ssrd_tas.csv"))

# plot with and without ssrd on the same graph
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

RR_tas_all$sun <- ifelse(RR_tas_all$sun == 3, "inc solar radiation", "exc solar radiation")


p_tas <- ggplot(data = RR_tas_all) +
  geom_line(aes(tas, RR, colour = sun), linewidth = 1) +
  geom_ribbon(aes(x = tas, ymin = RR_CIl, ymax = RR_CIh, fill = sun), alpha = 0.2) +
  facet_wrap(~region_trust) +
  labs(y = "relative risk", x = "temperature percentile (May to Sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
ggsave(
  plot = p_tas,
  filename = "ERF_25trust_tas_inc-exc-ssrd_regionid.jpeg", path = "./output/warm months/sensitivity_analysis/",
  device = "jpeg", width = 10, height = 10, dpi = 600
)
p_tas_ori <- ggplot(data = RR_tas_all) +
  geom_line(aes(tas, RR, colour = sun), linewidth = 1) +
  geom_ribbon(aes(x = tas, ymin = RR_CIl, ymax = RR_CIh, fill = sun), alpha = 0.2) +
  facet_wrap(~trust) +
  labs(y = "relative risk", x = "temperature percentile (May to Sep)") +
  theme_light() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
# ggsave(plot=p_tas_ori,
#        filename = "ERF_25trust_tas_inc-exc-ssrd_trust.jpeg", path = "./output/warm months/sensitivity_analysis/",
#        device = "jpeg", width = 10, height = 10, dpi = 600)


## coef_vcov for meta of this sensitivity analysis
data <- data_MtS_percentile
lag_tas <- 3

coef_vcov_tas_ls <- list()
for (t in 1:25) {
  data_t <- subset(data_MtS_percentile, trust == trust_25[t])
  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  cb_tas <- crossbasis(data_t$tas,
    lag = lag_tas,
    argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )

  data_t$year <- as.character(data_t$year)

  if (length(unique(data_t$year)) > 1) {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + ns(doy, df = 3) * year + wday,
      data_t,
      family = quasipoisson
    )
  } else {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + ns(doy, df = 3) + wday,
      data_t,
      family = quasipoisson
    )
  }


  # coef and vcov of cb_tas
  pred <- crossreduce(cb_tas, dlm_1, type = "overall")
  coef <- data.frame(t(pred$coefficients))
  vcov <- as.data.frame(t(vechMat(pred$vcov)))
  coef_vcov <- cbind(coef, vcov)
  coef_vcov$trust <- trust_25[t]
  coef_vcov$variable <- "tas"
  coef_vcov_tas_ls[[t]] <- coef_vcov
}

coef_vcov_tas <- rbindlist(coef_vcov_tas_ls)

# write.csv(coef_vcov_tas, row.names = F, "./output/warm months/coef_vcov_tas-lag3_ssrd-exc.csv")

## ----sensitivity: lag 0-3----
data <- data_MtS_percentile
var_name <- c("tas", "ssrd")
cen_tas <- 50
cen_ssrd <- 50

lag_menu <- 0:3
data <- data_MtS_percentile

k <- 1
RR_ls <- list()
coef_vcov_ls <- list()

for (v in 1:2) { # tas, ssrd
  for (t in 1:25) { # trusts
    data_t <- subset(data, trust == trust_25[t])

    data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)

    for (lag_ssrd in lag_menu) { # lags
      cb_ssrd <- crossbasis(data_t$ssrd,
        lag = lag_ssrd, argvar = list(fun = "ns", knots = c(30, 70)),
        arglag = list(fun = "integer")
      )
      for (lag_tas in lag_menu) {
        cb_tas <- crossbasis(data_t$tas,
          lag = lag_tas, argvar = list(fun = "ns", knots = c(30, 70)),
          arglag = list(fun = "integer")
        )


        data_t$year <- as.character(data_t$year)

        if (length(unique(data_t$year)) > 1) {
          dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) * year + wday,
            data_t,
            family = quasipoisson
          )
        } else {
          dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) + wday,
            data_t,
            family = quasipoisson
          )
        }


        if (v == 1) {
          pred <- crosspred(cb_tas, dlm_1, cumul = TRUE, by = 0.1, cen = cen_tas)
        } else if (v == 2) {
          pred <- crosspred(cb_ssrd, dlm_1, cumul = TRUE, by = 0.1, cen = cen_ssrd)
        }

        # extract RR
        RR_ls[[k]] <- data.frame(
          trust = trust_25[t],
          tas_lag = lag_tas,
          ssrd_lag = lag_ssrd,
          var = var_name[v],
          var_value = pred$predvar,
          cen_tas = cen_tas,
          cen_ssrd = cen_ssrd,
          RR = pred$allRRfit,
          RR_CIl = pred$allRRlow,
          RR_CIh = pred$allRRhigh
        )

        # coef and vcov of cb_tas
        if (v == 1) {
          pred <- crossreduce(cb_tas, dlm_1, type = "overall")
        } else if (v == 2) {
          pred <- crossreduce(cb_ssrd, dlm_1, type = "overall")
        }
        coef <- data.frame(t(pred$coefficients))
        vcov <- as.data.frame(t(vechMat(pred$vcov)))
        coef_vcov <- cbind(coef, vcov)
        coef_vcov$trust <- trust_25[t]
        coef_vcov$variable <- var_name[v]
        coef_vcov$lag_tas <- lag_tas
        coef_vcov$lag_ssrd <- lag_ssrd
        coef_vcov_ls[[k]] <- coef_vcov

        k <- k + 1
      }
    }
  }
}
RR_tas_ssrd <- rbindlist(RR_ls)
coef_vcov_all <- rbindlist(coef_vcov_ls)

# write.csv(RR_tas_ssrd, row.names = F, "./output/warm months/RR_tas-lag0-3-cen50_ssrd-lag0-3-cen50.csv")
# write.csv(coef_vcov_all, row.names = F, "./output/warm months/coef_vcov_tas-lag0-3_ssrd-lag0-3.csv")


## ----sensitivity: inc and exc COVID19 year (2021)----
# post covid period only
data_MtS_percentile_pc <- subset(data_MtS_percentile, year > 2021, )
data <- data_MtS_percentile_pc

lag_tas <- 3
lag_ssrd <- 3

cen_tas <- 16.4
cen_ssrd <- 100

trust_pc <- unique(data_MtS_percentile_pc$trust) # 24 trusts
RR_ls <- list()
for (v in 1:2) {
  print(var_name[v], flush.console = T)
  RR_ls <- list()
  coef_vcov_tas_ls <- list()
  for (t in 1:24) {
    data_t <- subset(data, trust == trust_pc[t])

    data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
    # # heat
    cb_tas <- crossbasis(data_t$tas,
      lag = lag_tas, argvar = list(fun = "ns", knots = c(30, 70)),
      arglag = list(fun = "integer")
    )
    cb_ssrd <- crossbasis(data_t$ssrd,
      lag = lag_ssrd, argvar = list(fun = "ns", knots = c(30, 70)),
      arglag = list(fun = "integer")
    )


    data_t$year <- as.character(data_t$year)

    if (length(unique(data_t$year)) > 1) {
      dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) * year + wday,
        data_t,
        family = quasipoisson
      )
    } else {
      dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + ns(doy, df = 3) + wday,
        data_t,
        family = quasipoisson
      )
    }

    if (v == 1) {
      pred <- crosspred(cb_tas, dlm_1, cumul = TRUE, by = 0.1, cen = cen_tas)
    } else if (v == 2) {
      pred <- crosspred(cb_ssrd, dlm_1, cumul = TRUE, by = 0.1, cen = cen_ssrd)
    }

    # extract RR
    RR_ls[[t]] <- data.frame(
      trust = trust_pc[t],
      period = "post-COVID19",
      tas_lag = lag_tas,
      ssrd_lag = lag_ssrd,
      var = var_name[v],
      var_value = pred$predvar,
      cen_tas = cen_tas,
      cen_ssrd = cen_ssrd,
      RR = pred$allRRfit,
      RR_CIl = pred$allRRlow,
      RR_CIh = pred$allRRhigh
    )
    # coef and vcov of cb_tas
    pred <- crossreduce(cb_tas, dlm_1, type = "overall")
    coef <- data.frame(t(pred$coefficients))
    vcov <- as.data.frame(t(vechMat(pred$vcov)))
    coef_vcov <- cbind(coef, vcov)
    coef_vcov$trust <- trust_25[t]
    coef_vcov$variable <- var_name[v]
    coef_vcov_tas_ls[[t]] <- coef_vcov
  }
  RR_tas_s <- rbindlist(RR_ls)
  coef_vcov <- rbindlist(coef_vcov_tas_ls)
  # write.csv(coef_vcov, row.names = F, paste0("./output/warm months/coef_vcov_tas-lag3_ssrd-lag3_",var_name[v],"_postCOVID.csv"))
  # write.csv(RR_tas_s, row.names = F, paste0("./output/warm months/sensitivity_analysis/RR_tas-lag3-cen16.4_ssrd-lag3-cen100_",var_name[v],"_postCOVID.csv"))
}

# plot main and sensitivity on the same graph
lookup_trust <- read.csv("input/lookup_trust_NHSRegion.csv")

for (v in 2:2) {
  print(var_name[v], flush.console = T)
  RR_tas_m <- read.csv(paste0("./output/warm months/RR_tas-lag3-cen16.4_ssrd-lag3-cen100_", var_name[v], ".csv"))
  RR_tas_m$period <- "inc-COVID19"

  RR_tas_s1 <- read.csv(paste0("./output/warm months/sensitivity_analysis/RR_tas-lag3-cen16.4_ssrd-lag3-cen100_", var_name[v], "_postCOVID.csv"))
  RR_tas_all <- rbind(RR_tas_m, RR_tas_s1)


  RR_tas_all <- left_join(RR_tas_all, lookup_trust[, c(1, 5)])
  levels(RR_tas_all$period) <- c("inc-COVID19", "exc-COVID19")
  # Assign unique IDs to each group
  RR_tas_all <- RR_tas_all %>%
    group_by(NHSER24NM) %>%
    mutate(trust_id = dense_rank(trust)) %>%
    ungroup()

  RR_tas_all$region_trust <- paste0(RR_tas_all$NHSER24NM, "_", RR_tas_all$trust_id)

  p_s <- ggplot(data = RR_tas_all) +
    geom_line(aes(var_value, RR, colour = period), linewidth = 1) +
    geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = period), alpha = 0.2) +
    facet_wrap(~region_trust) +
    labs(y = "relative risk", x = paste0(var_name_lab[v], " percentile (May to Sep)")) +
    theme_light() +
    theme(
      text = element_text(size = 12),
      legend.position = "bottom"
    )
  ggsave(
    plot = p_s,
    filename = paste0("ERF_25trust_s_regionid_", var_name[v], "_inc-exc-COVID.jpeg"),
    path = "./output/warm months/sensitivity_analysis",
    device = "jpeg", width = 10, height = 10, dpi = 600
  )
  p_s_ori <- ggplot(data = RR_tas_all) +
    geom_line(aes(var_value, RR, colour = period), linewidth = 1) +
    geom_ribbon(aes(x = var_value, ymin = RR_CIl, ymax = RR_CIh, fill = period), alpha = 0.2) +
    facet_wrap(~trust) +
    labs(y = "relative risk", x = paste0(var_name_lab[v], " percentile (May to Sep)")) +
    theme_light() +
    theme(
      text = element_text(size = 12),
      legend.position = "bottom"
    )
  ggsave(
    plot = p_s_ori,
    filename = paste0("ERF_25trust_s_", var_name[v], "_incl-exl-COVID.jpeg"),
    path = "./output/warm months/sensitivity_analysis",
    device = "jpeg", width = 10, height = 10, dpi = 600
  )
}

## ----sensitivity: relative humidity----
var_name <- c("tas", "rh")

data <- data_MtS_percentile
lag_tas <- 3
lag_ssrd <- 3
lag_rh <- 3

cen_tas <- 50
cen_rh <- 50

v <- 1 # tas
v <- 2 # rh
RR_ls <- list()
coef_vcov_tas_ls <- list()
coef_vcov_rh_ls <- list()
for (t in 1:25) {
  data_t <- subset(data, trust == trust_25[t])

  data_t$hotline_count_lag1 <- lag(data_t$hotline_count, 1)
  # # heat
  cb_tas <- crossbasis(data_t$tas,
    lag = lag_tas, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_ssrd <- crossbasis(data_t$ssrd,
    lag = lag_ssrd, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  cb_rh <- crossbasis(data_t$rh,
    lag = lag_rh, argvar = list(fun = "ns", knots = c(30, 70)),
    arglag = list(fun = "integer")
  )
  data_t$year <- as.character(data_t$year)

  if (length(unique(data_t$year)) > 1) {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + cb_rh + ns(doy, df = 3) * year + wday,
      data_t,
      family = quasipoisson
    )
  } else {
    dlm_1 <- glm(hotline_count ~ hotline_count_lag1 + cb_tas + cb_ssrd + cb_rh + ns(doy, df = 3) + wday,
      data_t,
      family = quasipoisson
    )
  }


  if (v == 1) {
    pred <- crosspred(cb_tas, dlm_1, cumul = T, by = 0.1, cen = cen_tas)
  } else {
    pred <- crosspred(cb_rh, dlm_1, cumul = T, by = 0.1, cen = cen_rh)
  }



  # coef and vcov of cb_tas
  pred <- crossreduce(cb_tas, dlm_1, type = "overall")
  coef <- data.frame(t(pred$coefficients))
  vcov <- as.data.frame(t(vechMat(pred$vcov)))
  coef_vcov <- cbind(coef, vcov)
  coef_vcov$trust <- trust_25[t]
  coef_vcov$variable <- "tas"
  coef_vcov_tas_ls[[t]] <- coef_vcov

  # coef and vcov of cb_rh
  pred <- crossreduce(cb_rh, dlm_1, type = "overall")
  coef <- data.frame(t(pred$coefficients))
  vcov <- as.data.frame(t(vechMat(pred$vcov)))
  coef_vcov <- cbind(coef, vcov)
  coef_vcov$trust <- trust_25[t]
  coef_vcov$variable <- "rh"
  coef_vcov_rh_ls[[t]] <- coef_vcov
}
dev.off()
coef_vcov_tas <- rbindlist(coef_vcov_tas_ls)
coef_vcov_rh <- rbindlist(coef_vcov_rh_ls)


write.csv(coef_vcov_tas, row.names = F, paste0("./output/warm months/coef_vcov_tas-lag3_ssrd-lag3_rh-lag", lag_rh, "_tas.csv"))
write.csv(coef_vcov_rh, row.names = F, paste0("./output/warm months/coef_vcov_tas-lag3_ssrd-lag3_rh-lag", lag_rh, "_rh.csv"))
