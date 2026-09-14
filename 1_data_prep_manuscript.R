# preparing data for the analysis
library(dplyr)
library(tidyr)
library(lubridate)
hotline_ori <- read.csv("input/combined_data_daily_hotlines.csv")
# columns on Date, and individual columns for Trusts

# total number of days with data for individual trusts
hotline_duration <- hotline_ori[-1] %>% summarise_all(funs(sum(!is.na(.))))
hotline_duration <- as.data.frame(t(hotline_duration))
colnames(hotline_duration) <- c("duration")
hotline_duration$trust <- rownames(hotline_duration)
hotline_duration <- arrange(hotline_duration, duration)

hotline <- pivot_longer(hotline_ori, 2:28, names_to = "trust", values_to = "hotline_count")

# Isle_weight <- hotline_ori[c("Date","IsleOfWight")]
# Isle_weight <- Isle_weight[!is.na(hotline_ori$IsleOfWight),]
# 5 months' data between 2022-07-22 and 2022-12-31

# daily mean temperature
tasmax <- read.csv("input/weighted_met_variables/tasmax.csv")
tasmin <- read.csv("input/weighted_met_variables/tasmin.csv")
tas <- cbind(tasmax, tasmin[3])
tas$tas <- (tas$tasmax + tas$tasmin) / 2
rm(tasmax, tasmin)
tas$time <- substr(tas$time, 1, 10)
tas$time <- as.Date(tas$time)
colnames(tas)[c(1, 2)] <- c("date", "NHSER24NM")
# solar radiation
ssrd <- read.csv("input/weighted_met_variables/ssrd.csv")
ssrd$date <- as.Date(ssrd$date)
colnames(ssrd)[2] <- "NHSER24NM"
# relative humidity
rh <- read.csv("input/weighted_met_variables/rh.csv")
rh$X <- NULL
rh$date <- as.Date(rh$date)
colnames(rh)[2] <- "NHSER24NM"

# met
met <- merge(tas, ssrd)
met <- merge(met, rh)
# lookup between NHS trusts and regions
lookup_1 <- read.csv("input/nhs-trusts-to-regions-lookup/etr.csv", header = F)
lookup_2 <- read.csv("input/nhs-trusts-to-regions-lookup/NHS_England_(Region)_(2024)_Names_and_Codes_in_England.csv")
lookup_2$ObjectId <- NULL
lookup_1 <- lookup_1[, c(1, 2, 3, 8)]
# keep the trusts with mental health hotline data available
trust_df <- data.frame(
  trust = colnames(hotline_ori)[-1], trust_id = 1:27,
  NHSER24CDH = c(
    "Y58", "Y59", "Y60", "Y60", "Y62",
    "Y58", "Y58", "Y56", "Y61", "Y60",
    "Y59", "Y60", "Y60", "Y62", "Y60",
    "Y61", "Y60", "Y56", "Y60", "Y63",
    "Y60", "Y56", "Y62", "Y59", "Y56",
    "Y59", "Y63"
  )
)


lookup_trust <- left_join(trust_df, lookup_2)
# write.csv(lookup_trust, row.names=F,"input/lookup_trust_NHSRegion.csv")
tas_trust <- merge(lookup_trust, met, all = T)
tas_trust <- arrange(tas_trust, trust_id, date)

# merge hotline and meteorological variables
colnames(hotline)[1] <- "date"
hotline$date <- as.Date(hotline$date)
data_all <- left_join(hotline, tas_trust)
# write.csv(data_all,row.names = F,"input/hotline_tas_ssrd_cfc_d2m_rh.csv")

## ----descriptive_stats----
data_ori <- read.csv("input/hotline_tas_ssrd_cfc_d2m_rh.csv")
data_ori$date <- as.Date(data_ori$date)
data_ori <- data_ori %>% mutate(
  year = year(date),
  month = month(date),
  doy = yday(date),
  wday = as.factor(lubridate::wday(date, label = T, week_start = 7)) # Mon is the first day of week and used as the reference
)
# remove na rows
data_ori <- data_ori[!is.na(data_ori$hotline_count), ]
# remove Birmingham, Isle of Wright for their low numbers and short duration
data_all <- data_ori[!(data_ori$trust %in% c("Birmingham", "IsleOfWight")), ]
# quick check of the time series
# time series: whole year
p_count <- ggplot(data_ori, aes(x = date, y = hotline_count)) +
  geom_line() +
  facet_wrap(~trust, scales = "free", ncol = 1) +
  theme_minimal() +
  theme(text = element_text(size = 16))
ggsave(
  filename = paste0("TS_daily_HotlineCount_27trusts_Wholeyear.pdf"),
  path = "./output/",
  plot = p_count, width = 18, height = 60, limitsize = FALSE
)

# subset data in May-Sep
data_MtS <- data_all[data_all$month %in% 5:9, ]
data_MtS <- arrange(data_MtS, trust_id, date)
# check data in NorfolkSuffolk
data_NorfolkSuffolk <- data_MtS[data_MtS$trust == "NorfolkSuffolk", ]
# remove values after 26 Sep 2023 (inc)
data_NorfolkSuffolk <- subset(data_NorfolkSuffolk, date < as.Date("2024-05-01"))
data_MtS <- subset(data_MtS, trust != "NorfolkSuffolk")
data_MtS <- rbind(data_MtS, data_NorfolkSuffolk)
data_MtS <- arrange(data_MtS, trust)
# check data in SouthWestLondonStGeorge
data_SouthWestLondonStGeorge <- data_MtS[data_MtS$trust == "SouthWestLondonStGeorge", ]
# remove values before 30 Sep 2020 (inc)
data_SouthWestLondonStGeorge <- subset(data_SouthWestLondonStGeorge, date > as.Date("2020-09-30"))
data_MtS <- subset(data_MtS, trust != "SouthWestLondonStGeorge")
data_MtS <- rbind(data_MtS, data_SouthWestLondonStGeorge)
data_MtS <- arrange(data_MtS, trust)
# BlackCountry
data_BlackCountry <- data_MtS[data_MtS$trust == "BlackCountry", ]
# remove values before 02 May 2021 (inc)
data_BlackCountry <- subset(data_BlackCountry, date > as.Date("2021-05-02"))
data_MtS <- subset(data_MtS, trust != "BlackCountry")
data_MtS <- rbind(data_MtS, data_BlackCountry)
data_MtS <- arrange(data_MtS, trust)
data_MtS$label <- format(data_MtS$date, "%Y-%m-%d")
data_MtS <- ungroup(data_MtS)
data_MtS$year <- as.character(data_MtS$year)
# time series: May to Sep
p_count_MtS <- ggplot(data = data_MtS, aes(x = label, y = hotline_count, color = year)) +
  geom_point() +
  facet_wrap(~trust, scales = "free", ncol = 1) +
  scale_x_discrete(
    breaks = c(
      "2020-05-01", "2020-07-01", "2020-09-01",
      "2021-05-01", "2021-07-01", "2021-09-01",
      "2022-05-01", "2022-07-01", "2022-09-01",
      "2023-05-01", "2023-07-01", "2023-09-01",
      "2024-05-01", "2024-07-01", "2024-09-01"
    ) # show a label every month
  ) +
  labs(x = "date") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    legend.position = ""
  )
# ggsave(filename = paste0("TS_daily_HotlineCount_25trusts_MtS.pdf"),
#        path = "./output/warm months",
#        plot=p_count_MtS,width = 22, height = 60,limitsize = FALSE)
# ggsave(filename = paste0("TS_daily_HotlineCount_25trusts_MtS.jpeg"),
#        path = "./output/warm months",
#        plot=p_count_MtS,width = 22, height = 60,limitsize = FALSE)

# outliers
# Q1-1.5*IQR and Q3+1.5*IQR
data_MtS_outlier <- data_MtS %>%
  group_by(trust) %>%
  mutate(
    Q1 = quantile(hotline_count, 0.25, na.rm = TRUE),
    Q3 = quantile(hotline_count, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_30IQR = Q1 - 3 * IQR,
    lower_15IQR = Q1 - 1.5 * IQR,
    upper_15IQR = Q3 + 1.5 * IQR,
    upper_30IQR = Q3 + 3 * IQR,
    is_outlier_15IQR = hotline_count < lower_15IQR | hotline_count > upper_15IQR,
    is_outlier_30IQR = hotline_count < lower_30IQR | hotline_count > upper_30IQR
  ) %>%
  ungroup() %>%
  filter(is_outlier_30IQR | is_outlier_15IQR) %>%
  select(trust, date, hotline_count, lower_15IQR, lower_30IQR, upper_15IQR, upper_30IQR, is_outlier_30IQR, is_outlier_15IQR, tas)
# write.csv(data_MtS_outlier,row.names = F,"output/hotline_tas_ssrd_cfc_outlier_lower_upper_MtS.csv")
data_MtS_outlier <- data_MtS %>%
  group_by(trust) %>%
  mutate(
    Q1 = quantile(hotline_count, 0.25, na.rm = TRUE),
    Q3 = quantile(hotline_count, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_30IQR = Q1 - 2 * IQR,
    is_outlier = hotline_count < lower_30IQR
  ) %>%
  ungroup() %>%
  filter(is_outlier) %>%
  select(trust, date, hotline_count, lower_30IQR, is_outlier, tas)

# exclude days with hotline count below Q1-3*IQR
data_MtS_outlier_lower <- data_MtS %>%
  group_by(trust) %>%
  mutate(
    Q1 = quantile(hotline_count, 0.25, na.rm = TRUE),
    Q3 = quantile(hotline_count, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR,
    is_outlier = hotline_count < lower
  ) %>%
  ungroup() %>%
  filter(is_outlier) %>%
  select(trust, date, tas, hotline_count, lower, upper, is_outlier)
# write.csv(data_MtS_outlier_lower, row.names = F, "output/warm months/MtS_outlier_Q1-3IQR.csv")
data_MtS_no_outlier_lower <- data_MtS %>%
  group_by(trust) %>%
  mutate(
    Q1 = quantile(hotline_count, 0.25, na.rm = TRUE),
    Q3 = quantile(hotline_count, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 3 * IQR,
    upper = Q3 + 3 * IQR,
    is_outlier = hotline_count < lower,
    hotline_count = ifelse(is_outlier, NA, hotline_count)
  ) %>%
  ungroup()
data_MtS_no_outlier_lower <- data_MtS_no_outlier_lower[, c(5, 2, 4, 6, 1, 18, 3, 8:12, 14, 13, 15:17, 19:24)]
# write.csv(data_MtS_no_outlier_lower,row.names = F,"output/hotline_tas_ssrd_cfc_d2m_rh_outlier-lower-NA_MtS.csv")
data_MtS_no_outlier_lower$year <- as.character(data_MtS_no_outlier_lower$year)

p_count_MtS <- ggplot(
  data = data_MtS_no_outlier_lower,
  aes(x = label, y = hotline_count, color = year)
) +
  geom_point() +
  facet_wrap(~trust, scales = "free", ncol = 1) +
  scale_x_discrete(
    breaks = c(
      "2020-05-01", "2020-07-01", "2020-09-01",
      "2021-05-01", "2021-07-01", "2021-09-01",
      "2022-05-01", "2022-07-01", "2022-09-01",
      "2023-05-01", "2023-07-01", "2023-09-01",
      "2024-05-01", "2024-07-01", "2024-09-01"
    ) # show a label every month
  ) +
  labs(x = "date") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    legend.position = ""
  )
# ggsave(filename = paste0("TS_daily_HotlineCount_25trusts_MtS_outlier-NA.pdf"),
#        path = "./output/warm months",
#        plot=p_count_MtS,width = 18, height = 60,limitsize = FALSE)


summary_stats_fun <- function(x, probs = c(0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99)) {
  c(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    quantile(x, probs = probs, na.rm = TRUE)
  )
}

data_MtS_no_outlier_lower <- read.csv("output/warm months/hotline_tas_ssrd_cfc_d2m_rh_outlier-lower-NA_MtS.csv")

data_summary_MtS <- data_MtS_no_outlier_lower %>%
  group_by(trust_id, trust) %>%
  reframe(across(5:12, summary_stats_fun))
data_summary_MtS$stat <- rep(
  c("min", "max", "mean", "median", "sd", paste0("p", c(0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99))),
  25
)
# write.csv(data_summary_MtS,row.names = F,"output/hotline_tas_ssrd_cfc_d2m_rh_summary_hotline-outlier-NA_MtS.csv")


# total count by trust
hotline_sum <- data_MtS_no_outlier_lower %>%
  group_by(trust_id, trust) %>%
  summarise(
    hotline_count = sum(hotline_count, na.rm = T)
  )
# write.csv(hotline_sum,row.names = F,"output/hotline_MtS_sum_25trust_outlier-NA.csv")

## ----hotline call summary----
data_MtS_no_outlier_lower <- read.csv("output/warm months/hotline_tas_ssrd_cfc_d2m_rh_outlier-lower-NA_MtS.csv")

data_MtS_call_s <- data_MtS_no_outlier_lower %>%
  group_by(trust_id, trust) %>%
  summarise(
    count = round(mean(hotline_count, na.rm = T), 0),
    sd = round(sd(hotline_count, na.rm = T), 2),
    total_calls = sum(hotline_count, na.rm = T),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )
data_MtS_call_s$hotline_count <- paste0(data_MtS_call_s$count, " (", data_MtS_call_s$sd, ")")
data_MtS_call_s$period <- paste0(data_MtS_call_s$start_date, "-", data_MtS_call_s$end_date)
data_MtS_call_s <- data_MtS_call_s[c(2, 1, 8, 5, 9)]

data_MtS_call_s_region <- data_MtS_no_outlier_lower %>%
  group_by(NHSER24NM) %>%
  summarise(
    count = round(mean(hotline_count, na.rm = T), 0),
    sd = round(sd(hotline_count, na.rm = T), 2),
    total_calls = sum(hotline_count, na.rm = T)
  )
data_MtS_call_s_region$hotline_count <- paste0(data_MtS_call_s_region$count, " (", data_MtS_call_s_region$sd, ")")
data_MtS_call_s_region <- data_MtS_call_s_region[c(1, 5, 4)]
## ----mean among all regions----
data_ori <- read.csv("input/hotline_tas_ssrd_cfc_d2m_rh.csv")
tas_ssrd_region <- data_ori[, c(1, 4, 10, 11, 13, 14)]
tas_ssrd_region <- distinct(tas_ssrd_region)
tas_ssrd_region$date <- as.Date(tas_ssrd_region$date)
tas_ssrd_region$month <- month(tas_ssrd_region$date)
# May to Sep
tas_ssrd_region_MtS <- subset(tas_ssrd_region, month %in% 5:9)
tas_ssrd_region_MtS <- tas_ssrd_region_MtS %>%
  group_by(NHSER24NM) %>%
  summarise(
    mean_tas = mean(tas, na.rm = TRUE),
    min_tas = min(tas, na.rm = TRUE),
    p164_tas = quantile(tas, probs = 0.164, na.rm = TRUE),
    p50_tas = quantile(tas, probs = 0.50, na.rm = TRUE),
    p90_tas = quantile(tas, probs = 0.90, na.rm = TRUE),
    p95_tas = quantile(tas, probs = 0.95, na.rm = TRUE),
    p99_tas = quantile(tas, probs = 0.99, na.rm = TRUE),
    max_tas = max(tas, na.rm = TRUE),
    mean_ssrd = mean(ssrd, na.rm = TRUE),
    min_ssrd = min(ssrd, na.rm = TRUE),
    p50_ssrd = quantile(ssrd, probs = 0.50, na.rm = TRUE),
    p90_ssrd = quantile(ssrd, probs = 0.90, na.rm = TRUE),
    p95_ssrd = quantile(ssrd, probs = 0.95, na.rm = TRUE),
    p99_ssrd = quantile(ssrd, probs = 0.99, na.rm = TRUE),
    max_ssrd = max(ssrd, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    min_rh = min(rh, na.rm = TRUE),
    p50_rh = quantile(rh, probs = 0.50, na.rm = TRUE),
    p90_rh = quantile(rh, probs = 0.90, na.rm = TRUE),
    p95_rh = quantile(rh, probs = 0.95, na.rm = TRUE),
    p99_rh = quantile(rh, probs = 0.99, na.rm = TRUE),
    max_rh = max(rh, na.rm = TRUE),
    .groups = "drop"
  )
# population weighted temperature among NHS regions
pop <- read.csv("C:/Users/lshkw9/OneDrive - London School of Hygiene and Tropical Medicine/data/population/NHS health regions/population_total_NHS-regions_2024.csv")
tas_ssrd_region_MtS <- merge(tas_ssrd_region_MtS, pop)

tas_ssrd_region_MtS <- tas_ssrd_region_MtS %>% summarise(
  tas_mean = weighted.mean(x = mean_tas, w = population24),
  tas_min = weighted.mean(x = min_tas, w = population24),
  tas_max = weighted.mean(x = max_tas, w = population24),
  tas_164 = weighted.mean(x = p164_tas, w = population24),
  tas_P50 = weighted.mean(x = p50_tas, w = population24),
  tas_P90 = weighted.mean(x = p90_tas, w = population24),
  tas_P95 = weighted.mean(x = p95_tas, w = population24),
  tas_P99 = weighted.mean(x = p99_tas, w = population24),
  ssrd_mean = weighted.mean(x = mean_ssrd, w = population24),
  ssrd_min = weighted.mean(x = min_ssrd, w = population24),
  ssrd_max = weighted.mean(x = max_ssrd, w = population24),
  ssrd_P50 = weighted.mean(x = p50_ssrd, w = population24),
  ssrd_P90 = weighted.mean(x = p90_ssrd, w = population24),
  ssrd_P95 = weighted.mean(x = p95_ssrd, w = population24),
  ssrd_P99 = weighted.mean(x = p99_ssrd, w = population24),
  rh_mean = weighted.mean(x = mean_rh, w = population24),
  rh_min = weighted.mean(x = min_rh, w = population24),
  rh_max = weighted.mean(x = max_rh, w = population24),
  rh_P50 = weighted.mean(x = p50_rh, w = population24),
  rh_P90 = weighted.mean(x = p90_rh, w = population24),
  rh_P95 = weighted.mean(x = p95_rh, w = population24),
  rh_P99 = weighted.mean(x = p99_rh, w = population24)
)
colnames(tas_ssrd_region_MtS)
tas_ssrd_region_MtS_t <- tas_ssrd_region_MtS %>%
  pivot_longer(
    cols = everything(),
    names_to = c("met", "metric"),
    names_pattern = "^(tas|ssrd|rh)_(.*)$"
  ) %>%
  mutate(
    metric = case_when(
      metric == "164" ~ "p164",
      TRUE ~ tolower(metric)
    ),
    metric = factor(
      metric,
      levels = c("mean", "min", "max", "p164", "p50", "p90", "p95", "p99")
    )
  ) %>%
  pivot_wider(
    names_from = met,
    values_from = value
  ) %>%
  arrange(metric)

tas_ssrd_region_MtS_t[, 2:3] <- round(tas_ssrd_region_MtS_t[, 2:3], 1)
tas_ssrd_region_MtS_t$rh <- round(tas_ssrd_region_MtS_t$rh, 3)
# write.csv(tas_ssrd_region_MtS_t, row.names = F, "output/warm months/MtS_tas_ssrd_rh_pop-wtd_summary.csv")
## ----post-covid----
# exclude 2021
data_MtS_no_outlier_lower_pc <- subset(data_MtS_no_outlier_lower, year > 2021)
data_summary_MtS_pc <- data_MtS_no_outlier_lower_pc %>%
  group_by(trust_id, trust) %>%
  reframe(across(5:12, summary_stats_fun))
data_summary_MtS_pc$stat <- rep(
  c("min", "max", "mean", "median", "sd", paste0("p", c(0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99))),
  24
)
# write.csv(data_summary_MtS_pc,row.names = F,"output/warm months/hotline_tas_ssrd_cfc_d2m_rh_summary_hotline-outlier-NA_MtS_post-covid.csv")
data_summary_MtS_pc$period <- "post-COVID19"
data_summary_MtS$period <- "inc-COVID19"
data_summary_MtS_all <- rbind(data_summary_MtS, data_summary_MtS_pc)

# median hotline count only
hotline_median <- data_summary_MtS_all[, c(1:3, 9, 10)]
hotline_median <- hotline_median[hotline_median$stat == "median", ]
hotline_median <- pivot_wider(hotline_median, names_from = period, values_from = hotline_count)
# write.csv(hotline_median,row.names = F,"output/warm months/hotline_median_hotline-outlier-NA_MtS_inc-exc-covid.csv")

# total count by trust total count by trust
hotline_sum <- data_MtS_no_outlier_lower %>%
  group_by(trust_id, trust) %>%
  summarise(
    hotline_count = sum(hotline_count, na.rm = T)
  )
hotline_sum_pc <- data_MtS_no_outlier_lower_pc %>%
  group_by(trust_id, trust) %>%
  summarise(
    hotline_count = sum(hotline_count, na.rm = T)
  )
hotline_sum$period <- "inc-COVID19"
hotline_sum_pc$period <- "post-COVID19"
hotline_sum_all <- rbind(hotline_sum, hotline_sum_pc)
hotline_sum_all <- pivot_wider(hotline_sum_all, names_from = period, values_from = hotline_count)
# write.csv(hotline_sum_all,row.names = F,"output/hotline_MtS_sum_25trust_outlier-NA.csv")

# check data distribution of meteorological variables
hist(data_MtS$tas,
  breaks = 15, col = "lightblue",
  main = "daily mean temperature", xlab = "temperature,°C"
)
hist(data_MtS$ssrd,
  breaks = 15, col = "lightblue",
  main = "daily mean downward solar radiation at he surface", xlab = "solar radiation, J m-2"
)
hist(data_MtS$rh,
  breaks = 15, col = "lightblue",
  main = "relative humidity", xlab = "relative humidity"
)

## ----cor_tas_ssrd----
# just for explorative purpose
# daily values are temporally autocorrelated so p values are not meaningful.
cor(data_MtS$ssrd, data_MtS$tas, use = "pairwise.complete.obs") # 0.1636431
plot(data_MtS$tas, data_MtS$ssrd,
  xlab = "daily mean temperature, °C", ylab = "surface solar radiation downwards, J/m2",
  main = "Scatter plot of temperature and solar radiation in May to Sep"
)

# whole year data is confounded by seasonality (higher ssrd in summer than winter in general)
cor(data_all$ssrd, data_all$tas, use = "pairwise.complete.obs") # 0.5977075
plot(data_all$tas, data_all$ssrd)
# scatter plot of tas and ssrd in individual months
# jpeg(filename = "output/tas_ssrd.jpeg",width = 2400,height = 1700,res = 200)
# par(mfrow=c(4,3))
for (m in 1:12) {
  data_m <- data_all[data_all$month == m, ]
  plot(data_m$tas, (data_m$ssrd) / 1000,
    xlab = "daily mean temperature, °C", ylab = "ssrd, kJ/m2",
    main = paste0("month ", m)
  )
  # Compute LOWESS fit
  lowess <- lowess(data_m$tas, (data_m$ssrd) / 1000, f = 1 / 2) # f is the smoothing parameter (0<f<1)
  # Add the LOWESS line
  lines(lowess, col = "red", lwd = 2)
}
dev.off()
# scatter plot of tas and ssrd in MtS
jpeg(filename = "output/tas_ssrd.jpeg", width = 800, height = 1700, res = 200)
par(mfrow = c(3, 1))
plot(data_MtS$tas, (data_MtS$ssrd) / 1000,
  xlab = "daily mean temperature, °C", ylab = "ssrd, kJ/m2",
  main = "May to September"
)
# Compute LOWESS fit
lowess_MtS <- lowess(data_MtS$tas, (data_MtS$ssrd) / 1000, f = 1 / 2) # f is the smoothing parameter (0<f<1)
# Add the LOWESS line
lines(lowess_MtS, col = "red", lwd = 2)

## ----cor_tas_d2m_rh----
cor(data_MtS$tas, data_MtS$rh, use = "pairwise.complete.obs") # -0.2588246

cor_tas_rh <- c()
for (m in 1:12) {
  data_m <- data_all[data_all$month == m, ]
  cor_tas_rh <- round(c(cor_tas_rh, cor(data_m$tas, data_m$rh, use = "pairwise.complete.obs")), 2)
}
cor_tas_rh <- data.frame(month = as.character(1:12), cor_tas_rh)
# write.csv(cor_tas_rh, row.names = F, "output/cor_tas_rh_month.csv")
# > cor_tas_rh
# month cor_tas_rh
# 1      1       0.03
# 2      2       0.32
# 3      3      -0.08
# 4      4       0.31
# 5      5      -0.20
# 6      6      -0.33
# 7      7      -0.53
# 8      8      -0.44
# 9      9      -0.08
# 10    10       0.04
# 11    11      -0.05
# 12    12      -0.19
# stronger negative correlation in warmer months (July and Aug)
# stronger positive correlation in Feb and Apr
