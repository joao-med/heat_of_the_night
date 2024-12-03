# Individual models and selection -----------------------------------------
### FIRST STAGE ###
# loading packages
library(tidyverse)
library(mgcv)
library(splines)
library(stats)
library(dlnm)
library(mvmeta)
options(scipen=999)
theme_set(theme_bw())

# FUNCTION FOR COMPUTING THE Q-AIC
QAIC <- function(model) {
  phi <- summary(model)$dispersion
  loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}
# Lag
lag <- 14
# loading data
data <- read_rds(paste0("00.data//data_m_w.rds"))
#QAIC
qaic_sum_01 <- 0
qaic_sum_02 <- 0
qaic_sum_03 <- 0
# getting mun names
city_names <- unique(data$mun_name)
# objects for saving
# Overall cumulative summaries
coef_all_n <- matrix(NA,length(data$mun_name %>% unique),
                   3,dimnames=list(data$mun_name %>% 
                                     unique,paste("b",seq(3),sep="")))
coef_all_CVD <- matrix(NA,length(data$mun_name %>% unique),
                   3,dimnames=list(data$mun_name %>% 
                                     unique,paste("b",seq(3),sep="")))
coef_all_RD <- matrix(NA,length(data$mun_name %>% unique),
                   3,dimnames=list(data$mun_name %>% 
                                     unique,paste("b",seq(3),sep="")))
# (co)variance matrices
vcov_all_n <- vector("list",length(data$mun_name %>% unique))
names(vcov_all_n) <- data$mun_name %>% unique
vcov_all_CVD <- vector("list",length(data$mun_name %>% unique))
names(vcov_all_CVD) <- data$mun_name %>% unique
vcov_all_RD <- vector("list",length(data$mun_name %>% unique))
names(vcov_all_RD) <- data$mun_name %>% unique
for (i in city_names){
  print(i)
  # selecting city ----------------------------------------------------------
  data_city <- data %>% subset(mun_name == i) %>% 
    subset(month %in% c(1:3,11:12)) %>% mutate(year = year %>% as.character())
  data_city$n %>% sum
  # span of time in days
  data_city$time <- 1:length(data_city$date)
  # -------------------------------------------------------------------------
  ## Creating cross-basis of variables
  # tmean
  cb_tmean <- crossbasis(data_city$mean, lag = lag,
                         arglag=list(fun="ns", knots = 3),
                         argvar=list(fun="ns", knots = 3),
                         group = data_city$year)
  # hot nights index
  # Points for knots in HNE crossbasis
  p60_HNE = quantile(data_city$HNE,0.6)
  p90_HNE = quantile(data_city$HNE,0.9)
  # HNE
  HN <- crossbasis(data_city$HNE, lag = lag, 
                   arglag = list(fun = "ns", knots = 3),
                   argvar = list(fun = "ns", knots = c(p60_HNE,p90_HNE)),
                   group = data_city$year) 
  
  # relative humidity
  RH <- ns(data_city$rh, df = 3)
  # Model -------------------------------------------------------------------
  mod_n <- glm(n ~ HN +
                 cb_tmean +
                 RH +
                 dow +
                 ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
               family = quasipoisson(), data = data_city)
  mod_CVD <- glm(n_CVD ~ HN +
                   cb_tmean +
                   RH +
                   dow +
                   ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
                 family = quasipoisson(), data = data_city)
  mod_RD <- glm(n_RD ~ HN +
                  cb_tmean +
                  RH +
                  dow +
                  ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
                family = quasipoisson(), data = data_city,maxit = 50)
  qaic_sum_01 <- qaic_sum_01 + QAIC(mod_n)
  qaic_sum_02 <- qaic_sum_02 + QAIC(mod_CVD)
  qaic_sum_03 <- qaic_sum_03 + QAIC(mod_RD)

  # # Extracting coef
  # n total
  crall_n <- crossreduce(HN,mod_n,cen = 0, type = "overall")
  coef_all_n[i,] <- coef(crall_n)
  vcov_all_n[[i]] <- vcov(crall_n)
  # n CVD
  crall_CVD <- crossreduce(HN,mod_CVD,cen = 0, type = "overall")
  coef_all_CVD[i,] <- coef(crall_CVD)
  vcov_all_CVD[[i]] <- vcov(crall_CVD)
  # n RD
  crall_RD <- crossreduce(HN,mod_RD,cen = 0, type = "overall")
  coef_all_RD[i,] <- coef(crall_RD)
  vcov_all_RD[[i]] <- vcov(crall_RD)
}
#Coefs & (Co)v
coef_all_n %>%  write_rds(paste0("03.output//coef_all.rds"))
vcov_all_n %>% write_rds(paste0("03.output//vcov_all.rds"))
coef_all_CVD %>%  write_rds(paste0("03.output//coef_CVD.rds"))
vcov_all_CVD %>% write_rds(paste0("03.output//vcov_CVD.rds"))
coef_all_RD %>%  write_rds(paste0("03.output//coef_RD.rds"))
vcov_all_RD %>% write_rds(paste0("03.output//vcov_RD.rds"))

# QAIC --------------------------------------------------------------------
qaic_sum_01
qaic_sum_02
qaic_sum_03


