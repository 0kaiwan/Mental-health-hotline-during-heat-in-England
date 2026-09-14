library(lubridate)
library(tidyr)
library(dplyr)
library(data.table)
library(splines)
library(dlnm)
library(mixmeta)
var_name <- c("tas", "ssrd")
season <- c("warm", "cold")
## ----meta: main model----
s <- 1
v <- 1
for (s in 1:1) { # warm/cold season
  for (v in 1:2) {
    coef_vcov <- read.csv(paste0("./output/", season[s], " months/coef_vcov_tas-lag3_ssrd-lag3_", var_name[v], ".csv"))

    coef <- as.matrix(coef_vcov[, grep("^b", names(coef_vcov))])
    vcov <- as.matrix(coef_vcov[, grep("^V", names(coef_vcov))])
    # random effects of trust
    metatmean <- mixmeta(coef, vcov, data = coef_vcov, random = ~ 1 | trust, bscov = "diag")
    # summary(metatmean) check Q and I-square
    # PREDICT COEF/VCOV FOR THOSE COMBINATIONS
    pred <- predict(metatmean, newdata = data.frame(trust = c("AvonWiltshire")), vcov = T)
    # PREDICT ASSOCIATIONS (IN A LIST)
    ik <- c(30, 70) # ik <-as.numeric(colMeans(ik_all[-1]))
    var_pred <- seq(0, 100, by = 0.1)
    bvar <- onebasis(var_pred, fun = "ns", knots = ik)
    cen <- 50
    cp <- crosspred(bvar,
      coef = pred$fit, vcov = pred$vcov,
      model.link = "log", by = 0.1, cen = cen
    )
    # # cen  the actual MMT
    RR <- cp$allRRfit
    RR_var <- data.frame(RR = RR, var = cp$predvar)
    RR_m <- min(RR_var$RR)

    MMT <- RR_var[RR_var$RR == RR_m, "var"]
    cen <- MMT
    # re-run pred using the new cen
    cp <- crosspred(bvar,
      coef = pred$fit, vcov = pred$vcov,
      model.link = "log", by = 0.1, cen = cen
    )

    RR <- data.frame(
      RR = cp$allRRfit,
      RR_CIl = cp$allRRlow,
      RR_CIh = cp$allRRhigh,
      var = var_name[v],
      var_value = var_pred,
      MMT = MMT,
      trust = "meta"
    )
    # write.csv(RR,row.names = F,paste0("output/",season[s]," months/meta_tas-lag3_ssrd-lag3_",var_name[v],"-cen",cen,".csv"))
  }
}
## ----meta: lag 0-3----
# warm only
s <- 1

lag_menu <- 0:3
coef_vcov_all <- read.csv("./output/warm months/coef_vcov_tas-lag0-3_ssrd-lag0-3.csv")
k <- 1
RR_list <- list()
for (variable_n in var_name) {
  for (l_ssrd in lag_menu) {
    for (l_tas in lag_menu) {
      print(paste(variable_n, "| ssrd lag", l_ssrd, "| tas lag ", l_tas), flush.console = T)
      coef_vcov <- subset(coef_vcov_all, variable == variable_n & lag_tas == l_tas & lag_ssrd == l_ssrd)
      coef <- as.matrix(coef_vcov[, grep("^b", names(coef_vcov))])
      vcov <- as.matrix(coef_vcov[, grep("^V", names(coef_vcov))])
      # random effects of trust
      metatmean <- mixmeta(coef, vcov, data = coef_vcov, random = ~ 1 | trust, bscov = "diag")
      # summary(metatmean) check Q and I-square
      # PREDICT COEF/VCOV FOR THOSE COMBINATIONS
      pred <- predict(metatmean, newdata = data.frame(trust = c("AvonWiltshire")), vcov = T)
      # PREDICT ASSOCIATIONS (IN A LIST)
      ik <- c(30, 70) # ik <-as.numeric(colMeans(ik_all[-1]))
      var_pred <- seq(0, 100, by = 0.1)
      bvar <- onebasis(var_pred, fun = "ns", knots = ik)
      cen <- 50
      cp <- crosspred(bvar,
        coef = pred$fit, vcov = pred$vcov,
        model.link = "log", by = 0.1, cen = cen
      )
      # # cen  the actual MMT
      RR <- cp$allRRfit
      RR_var <- data.frame(RR = RR, var = cp$predvar)
      # if there are two MMTs, select bigger one
      RR_min <- min(RR_var$RR)
      MMT <- max(RR_var$var[abs(RR_var$RR - RR_min) < 1e-8])

      cen <- MMT
      # re-run pred using the new cen
      cp <- crosspred(bvar,
        coef = pred$fit, vcov = pred$vcov,
        model.link = "log", by = 0.1, cen = cen
      )

      RR_list[[k]] <- data.frame(
        RR = cp$allRRfit,
        RR_CIl = cp$allRRlow,
        RR_CIh = cp$allRRhigh,
        var = variable_n,
        var_value = var_pred,
        lag_tas = l_tas,
        lag_ssrd = l_ssrd,
        MMT = MMT,
        trust = "meta"
      )
      k <- k + 1
    }
  }
}
RR_meta <- rbindlist(RR_list)
# write.csv(RR_meta,row.names = F,paste0("output/",season[s]," months/sensitivity_analysis/meta_tas-lag0-3_ssrd-lag0-3_cen-MMT.csv"))

## ----meta_exc-ssrd----
coef_vcov <- read.csv("./output/warm months/coef_vcov_tas-lag3_ssrd-exc.csv")
# warm season only
s <- 1
# tas only
v <- 1

coef <- as.matrix(coef_vcov[, grep("^b", names(coef_vcov))])
vcov <- as.matrix(coef_vcov[, grep("^V", names(coef_vcov))])
# random effects of trust
metatmean <- mixmeta(coef, vcov, data = coef_vcov, random = ~ 1 | trust, bscov = "diag")
# summary(metatmean) check Q and I-square
# PREDICT COEF/VCOV FOR THOSE COMBINATIONS
pred <- predict(metatmean, newdata = data.frame(trust = c("AvonWiltshire")), vcov = T)
# PREDICT ASSOCIATIONS (IN A LIST)
ik <- c(30, 70) # ik <-as.numeric(colMeans(ik_all[-1]))
var_pred <- seq(0, 100, by = 0.1)
bvar <- onebasis(var_pred, fun = "ns", knots = ik)
cen <- 50
cp <- crosspred(bvar,
  coef = pred$fit, vcov = pred$vcov,
  model.link = "log", by = 0.1, cen = cen
)
# # cen  the actual MMT
RR <- cp$allRRfit
RR_var <- data.frame(RR = RR, var = cp$predvar)

# if there are two MMTs, select bigger one
RR_min <- min(RR_var$RR)
MMT <- max(RR_var$var[abs(RR_var$RR - RR_min) < 1e-8])

cen <- MMT
# re-run pred using the new cen
cp <- crosspred(bvar,
  coef = pred$fit, vcov = pred$vcov,
  model.link = "log", by = 0.1, cen = cen
)

RR <- data.frame(
  RR = cp$allRRfit,
  RR_CIl = cp$allRRlow,
  RR_CIh = cp$allRRhigh,
  var = var_name[v],
  var_value = var_pred,
  tas_lag = 3,
  ssrd = "exc-ssrd",
  MMT = MMT,
  trust = "meta"
)
# write.csv(RR,row.names = F,paste0("output/",season[s]," months/sensitivity_analysis/meta_tas-lag3_ssrd-exc_tas-cen",cen,".csv"))
## ----meta: inc rh----
var_name <- c("tas", "rh")

s <- 1
v <- 1
for (s in 1:1) { # warm/cold season
  for (v in 1:2) {
    coef_vcov <- read.csv(paste0("./output/", season[s], " months/coef_vcov_tas-lag3_ssrd-lag3_rh-lag1_", var_name[v], ".csv"))

    coef <- as.matrix(coef_vcov[, grep("^b", names(coef_vcov))])
    vcov <- as.matrix(coef_vcov[, grep("^V", names(coef_vcov))])
    # random effects of trust
    metatmean <- mixmeta(coef, vcov, data = coef_vcov, random = ~ 1 | trust, bscov = "diag")
    # summary(metatmean) check Q and I-square
    # PREDICT COEF/VCOV FOR THOSE COMBINATIONS
    pred <- predict(metatmean, newdata = data.frame(trust = c("AvonWiltshire")), vcov = T)
    # PREDICT ASSOCIATIONS (IN A LIST)
    ik <- c(30, 70) # ik <-as.numeric(colMeans(ik_all[-1]))
    var_pred <- seq(0, 100, by = 0.1)
    bvar <- onebasis(var_pred, fun = "ns", knots = ik)
    cen <- 50
    cp <- crosspred(bvar,
      coef = pred$fit, vcov = pred$vcov,
      model.link = "log", by = 0.1, cen = cen
    )
    # # cen  the actual MMT
    RR <- cp$allRRfit
    RR_var <- data.frame(RR = RR, var = cp$predvar)
    RR_m <- min(RR_var$RR)

    MMT <- RR_var[RR_var$RR == RR_m, "var"]
    cen <- MMT
    # re-run pred using the new cen
    cp <- crosspred(bvar,
      coef = pred$fit, vcov = pred$vcov,
      model.link = "log", by = 0.1, cen = cen
    )

    RR <- data.frame(
      RR = cp$allRRfit,
      RR_CIl = cp$allRRlow,
      RR_CIh = cp$allRRhigh,
      var = var_name[v],
      var_value = var_pred,
      MMT = MMT,
      trust = "meta"
    )
    write.csv(RR, row.names = F, paste0("output/", season[s], " months/meta_tas-lag3_rh-lag1_", var_name[v], "-cen", cen, ".csv"))
  }
}
## ----meta: exc COVID19 period----
s <- 1
v <- 1
coef_vcov <- read.csv(paste0("./output/", season[s], " months/coef_vcov_tas-lag3_ssrd-lag3_", var_name[v], "_postCOVID.csv"))

coef <- as.matrix(coef_vcov[, grep("^b", names(coef_vcov))])
vcov <- as.matrix(coef_vcov[, grep("^V", names(coef_vcov))])
# random effects of trust
metatmean <- mixmeta(coef, vcov, data = coef_vcov, random = ~ 1 | trust, bscov = "diag")
# summary(metatmean) check Q and I-square
# PREDICT COEF/VCOV FOR THOSE COMBINATIONS
pred <- predict(metatmean, newdata = data.frame(trust = c("AvonWiltshire")), vcov = T)
# PREDICT ASSOCIATIONS (IN A LIST)
ik <- c(30, 70) # ik <-as.numeric(colMeans(ik_all[-1]))
var_pred <- seq(0, 100, by = 0.1)
bvar <- onebasis(var_pred, fun = "ns", knots = ik)
cen <- 16.4
cp <- crosspred(bvar,
  coef = pred$fit, vcov = pred$vcov,
  model.link = "log", by = 0.1, cen = cen
)

RR <- data.frame(
  RR = cp$allRRfit,
  RR_CIl = cp$allRRlow,
  RR_CIh = cp$allRRhigh,
  var = var_name[v],
  var_value = var_pred,
  cen = cen,
  trust = "meta"
)
write.csv(RR, row.names = F, paste0("output/", season[s], " months/meta_tas-lag3_ssrd-lag3_", var_name[v], "-cen", cen, "_postCOVID.csv"))