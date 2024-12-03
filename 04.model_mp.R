# Meta-regression ---------------------------------------------------------
### THIRD STAGE ###
library(tidyverse)
library(mgcv)
library(dlnm)
library(patchwork)
library(gridExtra)
library(mixmeta)
library(xlsx)
options(scipen=999)
theme_set(theme_bw())

# Loading data for meta regression
data <- read_rds("00.data//data_m_w.rds")
cities <- data$mun_name %>% unique
data <- lapply(cities,function(x) data[data$mun_name==x,])
names(data) <- cities

# Wald test
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))}

# Univariate meta-regression -- latitude
mm_list <-list(meta_model_all,meta_model_CVD, meta_model_RD)
mm_name <- c("all","CVD","RD")
for (i in c(1:3)){
  print(i)
  mm <- mm_list[[i]] 
  coef_all <- read_rds(paste0("03.output//coef_",mm_name[i],".rds"))
  vcov_all <- read_rds(paste0("03.output//vcov_",mm_name[i],".rds"))
  
  # latitude
  lat <- sapply(data, function(x) mean(x$lat,na.rm=T))
  round(quantile(lat,na.rm=T,c(10,90)/100),2)
  metalat <- update(mm,.~lat)
  wald_lat <- round(fwald(metalat,"lat"),10)
  i2 <- summary(metalat)
  chisq <- drop1(metalat, test = "Chisq")
  tib_lat <- tibble (Wald_test = wald_lat,
                     I2 = i2$i2stat[1],
                     chisq = chisq$`Pr(>Chi)`[2])
  
  # Wald TEST
  variables <- c('Latitute')
  wald_values <- bind_rows(tib_lat)
  wald_tab_fixed <- bind_cols(Variables = variables, wald_values, model = mm_name[i])
  # saving in environment
  assign(paste0("tib_wald_",mm_name[i]),wald_tab_fixed)
}

# Multivariate meta-regressions -- Lat plus other variables
# variables
# population
pop <- sapply(data, function(x) mean(x$pesotot,na.rm=T))
# percentage of population 65+
p65 <- sapply(data, function(x) mean(x$p_65,na.rm=T))
# percentage of urban population
urb <- sapply(data, function(x) mean(x$urb_p,na.rm=T))
# Average per capita family income 
income <- sapply(data, function(x) mean(x$rdpc,na.rm=T))
# percentage of poor population
ppoor <- sapply(data, function(x) mean(x$pmpob,na.rm=T))
# GINI
GINI <- sapply(data, function(x) mean(x$gini,na.rm=T))
# longitude
lon <- sapply(data, function(x) mean(x$lon,na.rm=T))

# All Causes 
mm1 <- mm_list[[1]] 
coef_all <- read_rds(paste0("03.output//coef_",mm_name[1],".rds"))
vcov_all <- read_rds(paste0("03.output//vcov_",mm_name[1],".rds"))
# models
meta_all0 <- update(mm1,.~lat)
meta_all1 <- update(mm1,.~lat + pop)
meta_all2 <- update(mm1,.~lat + p65)
meta_all3 <- update(mm1,.~lat + urb)
meta_all4 <- update(mm1,.~lat + income)
meta_all5 <- update(mm1,.~lat + ppoor)
meta_all6 <- update(mm1,.~lat + lon)

# Test
drop1(meta_all1, test = "Chisq")
drop1(meta_all2, test = "Chisq")
drop1(meta_all3, test = "Chisq")
drop1(meta_all4, test = "Chisq")
drop1(meta_all5, test = "Chisq")
drop1(meta_all6, test = "Chisq")

# Information table
## models list
baseline <- summary(mm1)
list_all <- list(meta_all0,meta_all1,meta_all2,meta_all3,meta_all4,meta_all5, meta_all6)
table_all <- tibble()
for (i in list_all){
  
  tib <- i %>% summary
  names <-  unique(tib$coefficients %>% row.names() %>% str_remove("b[1-9]."))
  if (names %>% length() == 2 ){
    names <- paste(names[2]) %>% str_remove("NA")
    lrt_n = 2
  } else {
    names <- paste(names[2],"+",names[3]) %>% str_remove("NA")
    lrt_n = 3
  }
  LRT <- drop1(i,test = "Chisq")
  table_all <- bind_rows(table_all,
                         tibble(
                           AIC = round(tib$AIC,2),
                           I2 = round(tib$i2stat[1],2),
                           LRT_test = LRT$`Pr(>Chi)`[lrt_n],
                           Model= names)
                         
  )
}
table_all <- bind_rows(tibble(AIC = baseline$AIC,
                              I2 = baseline$i2stat[1],
                              LRT_test = NULL,
                              Model = "Baseline"),table_all)
# CVD 
mm2 <- mm_list[[2]] 
coef_all <- read_rds(paste0("03.output//coef_",mm_name[2],".rds"))
vcov_all <- read_rds(paste0("03.output//vcov_",mm_name[2],".rds"))
# models
meta_CVD0 <- update(mm2,.~lat)
meta_CVD1 <- update(mm2,.~lat + pop)
meta_CVD2 <- update(mm2,.~lat + p65)
meta_CVD3 <- update(mm2,.~lat + urb)
meta_CVD4 <- update(mm2,.~lat + income)
meta_CVD5 <- update(mm2,.~lat + ppoor)
meta_CVD6 <- update(mm2,.~lat + lon)
# tests
drop1(meta_CVD1, test = "Chisq")
drop1(meta_CVD2, test = "Chisq")
drop1(meta_CVD3, test = "Chisq")
drop1(meta_CVD4, test = "Chisq")
drop1(meta_CVD5, test = "Chisq")
drop1(meta_CVD6, test = "Chisq")
# Information table
## models list
baseline <- summary(mm2)
list_CVD <- list(meta_CVD0,meta_CVD1,meta_CVD2,meta_CVD3,meta_CVD4,meta_CVD5, meta_CVD6)
table_CVD <- tibble()
for (i in list_CVD){
  tib <- i %>% summary
  names <-  unique(tib$coefficients %>% row.names() %>% str_remove("b[1-9]."))
  if (names %>% length() == 2 ){
    names <- paste(names[2]) %>% str_remove("NA")
    lrt_n = 2
  } else {
    names <- paste(names[2],"+",names[3]) %>% str_remove("NA")
    lrt_n = 3
  }
  LRT <- drop1(i,test = "Chisq")
  table_CVD <- bind_rows(table_CVD,
                         tibble(
                           AIC = round(tib$AIC,2),
                           I2 = round(tib$i2stat[1],2),
                           LRT_test = LRT$`Pr(>Chi)`[lrt_n],
                           Model= names)
                         
  )
}
table_CVD <- bind_rows(tibble(AIC = baseline$AIC,
                              I2 = baseline$i2stat[1],
                              LRT_test = NULL,
                              Model = "Baseline"),table_CVD)
# RD 
mm3 <- mm_list[[3]] 
coef_all <- read_rds(paste0("03.output//coef_",mm_name[3],".rds"))
vcov_all <- read_rds(paste0("03.output//vcov_",mm_name[3],".rds"))
# Models
meta_RD0 <- update(mm3,.~lat)
meta_RD1 <- update(mm3,.~lat + pop)
meta_RD2 <- update(mm3,.~lat + p65)
meta_RD3 <- update(mm3,.~lat + urb)
meta_RD4 <- update(mm3,.~lat + income)
meta_RD5 <- update(mm3,.~lat + ppoor)
meta_RD6 <- update(mm3,.~lat + lon)
# tests
drop1(meta_RD1, test = "Chisq")
drop1(meta_RD2, test = "Chisq")
drop1(meta_RD3, test = "Chisq")
drop1(meta_RD4, test = "Chisq")
drop1(meta_RD5, test = "Chisq")
drop1(meta_RD6, test = "Chisq")
# Information table
## models list
baseline <- summary(mm3)
list_RD <- list(meta_RD0,meta_RD2,meta_RD3,meta_RD4,meta_RD5, meta_RD6)
table_RD <- tibble()
for (i in list_RD){
  tib <- i %>% summary
  names <-  unique(tib$coefficients %>% row.names() %>% str_remove("b[1-9]."))
  if (names %>% length() == 2 ){
  names <- paste(names[2]) %>% str_remove("NA")
    lrt_n = 2
  } else {
  names <- paste(names[2],"+",names[3]) %>% str_remove("NA")
    lrt_n = 3
  }
  LRT <- drop1(i,test = "Chisq")
  table_RD <- bind_rows(table_RD,
                        tibble(
                          AIC = round(tib$AIC,2),
                          I2 = tib$i2stat[1],
                          LRT_test = LRT$`Pr(>Chi)`[lrt_n],
                          Model= names)
                        
  )
}

table_RD <- bind_rows(tibble(AIC = baseline$AIC,
                             I2 = baseline$i2stat[1],
                             LRT_test = NULL,
                             Model = "Baseline"),table_RD)

# Saving complete table of all subgroups
table_all <- tibble(table_all, `Mortality Group` = "All Causes")
table_CVD <- tibble(table_CVD, `Mortality Group` = "CVD")
table_RD <- tibble(table_RD, `Mortality Group` = "RD")
# Uniting 
table_meta_complete <- bind_rows(table_all,table_CVD,table_RD)
xlsx::write.xlsx(table_meta_complete, "01.tables//supp//04.table_meta_01.xlsx")

# plots
## PREDICTION FROM META-REGRESSION ##
# Base for prediction
data_p <- read_rds("00.data//data_m_w.rds")  %>% 
  mutate(mun_name = mun_name %>% as.factor())
percentiles <- seq(0, 999, by = 1) / 1000
avg_HNE = quantile(data_p$HNE, probs = percentiles) 
avg_HNE = tibble(percentile = percentiles, value = avg_HNE)
# knots
knots = avg_HNE %>% filter(percentile %in% c(0.50,0.90))
knots = knots$value
# Base
x_argvar = avg_HNE$value 
b_argvar = onebasis(x_argvar, fun = "ns",knots = knots)

# function for table
plot.pred.var <- function(model, var, baseline_model, name1,name2, title){
  # base for prediction has to be loaded
  coef_all <- read_rds(paste0("03.output//coef_",name1,".rds"))
  vcov_all <- read_rds(paste0("03.output//vcov_",name1,".rds"))
  
  if(var %>% length() == 2)
  {
    print("TRUE")
    val1 <- tibble(var = round(quantile(var[[1]],na.rm=T,c(10,90)/100),3))
    val2 <- tibble(var = round(quantile(var[[2]],na.rm=T,c(10,90)/100),3))
    colnames(val1) <- name2[1]
    colnames(val2) <- name2[2]
    pred <- mixmeta::predict.mixmeta(model,data.frame(val1,val2),vcov=T)
  } else {
    print("FALSE")
    val <- tibble(var = round(quantile(var,na.rm=T,c(10,90)/100),3))
    colnames(val) <- name2
    pred <- mixmeta::predict.mixmeta(model,data.frame(val),vcov=T)
  }
  
  # Crosspreds
  baseline <- crosspred(b_argvar,coef = baseline_model$coefficients,
                        vcov = baseline_model$vcov,
                        model.link="log",by=0.1,cen=0)
  cp_10 <- crosspred(b_argvar,coef=pred[[1]]$fit,vcov=pred[[1]]$vcov,
                     model.link="log",by=0.1,cen=0)
  cp_90 <- crosspred(b_argvar,coef=pred[[2]]$fit,vcov=pred[[2]]$vcov,
                     model.link="log",by=0.1,cen=0)
  # creating tables 
  gg_tibble <- function(tib){
    tibble(x = tib$predvar,fit = tib$allRRfit ,
           low = tib$allRRlow, high = tib$allRRhigh)}
  #applying function
  tibble_base <- tibble(gg_tibble(baseline),p = "base")
  tibble_10 <- tibble(gg_tibble(cp_10),p = "p10")
  tibble_90 <- tibble(gg_tibble(cp_90),p = "p90")
  tibble_combined <- bind_rows(tibble_10, tibble_90)
  # Saving a complete table to environment
  complete <- bind_rows(tibble_base,tibble_combined)
  assign(paste0("complete_table",name1),complete)
  #plot
  tibble_base %>%
    ggplot(aes(x, fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
    geom_line(data = tibble_combined, aes(x, fit, color = p), linetype = 2) +
    geom_ribbon(data = tibble_combined, aes(x = x, ymin = low,
                                            ymax = high, fill = p), alpha = 0.1) +
    geom_hline(aes(yintercept = 1))+
    ylab("RR") + 
    xlab("HNE") + 
    scale_color_manual(values = c("p10" = "blue", "p90" = "red")) +
    scale_fill_manual(values = c("p10" = "blue", "p90" = "red")) +
    theme_minimal() +
    guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
    ggtitle(paste0(title))
  
}

# Plotting
p1 <- plot.pred.var(meta_all0, lat, meta_model_all, 
                    "all", "lat", "All Causes - Latitude")+
  theme(legend.position="none")
p2 <- plot.pred.var(meta_CVD0, lat, meta_model_CVD, 
                    "CVD", "lat",  "Cardiovascular Causes - Latitude")+
  theme(legend.position="none",
        axis.title.y=element_blank())
p3 <- plot.pred.var(meta_RD0, lat, meta_model_RD, 
                    "RD", "lat", "Respiratory Causes - Latitude")+
  theme(axis.title.y=element_blank())
p1+p2+p3
ggsave("02.plots//main//03.plot_meta.tiff", dpi = 600, width = 12, height = 4)

# tables
table.pred.var <- function(model, var, baseline_model, name1,name2){
  # base for prediction has to be loaded
  coef_all <- read_rds(paste0("03.output//coef_",name1,".rds"))
  vcov_all <- read_rds(paste0("03.output//vcov_",name1,".rds"))
  
  if(var %>% length() == 2)
  {
    print("TRUE")
    val1 <- tibble(var = round(quantile(var[[1]],na.rm=T,c(10,90)/100),3))
    val2 <- tibble(var = round(quantile(var[[2]],na.rm=T,c(10,90)/100),3))
    colnames(val1) <- name2[1]
    colnames(val2) <- name2[2]
    pred <- mixmeta::predict.mixmeta(model,data.frame(val1,val2),vcov=T)
  } else {
    print("FALSE")
    val <- tibble(var = round(quantile(var,na.rm=T,c(10,90)/100),3))
    colnames(val) <- name2
    pred <- mixmeta::predict.mixmeta(model,data.frame(val),vcov=T)
  }
  # Crosspreds
  baseline <- crosspred(b_argvar,coef = baseline_model$coefficients,
                        vcov = baseline_model$vcov,
                        model.link="log",by=0.1,cen=0)
  cp_10 <- crosspred(b_argvar,coef=pred[[1]]$fit,vcov=pred[[1]]$vcov,
                     model.link="log",by=0.1,cen=0)
  cp_90 <- crosspred(b_argvar,coef=pred[[2]]$fit,vcov=pred[[2]]$vcov,
                     model.link="log",by=0.1,cen=0)
  # creating tables 
  gg_tibble <- function(tib){
    tibble(x = tib$predvar,fit = tib$allRRfit ,
           low = tib$allRRlow, high = tib$allRRhigh)}
  #applying function
  tibble_base <- tibble(gg_tibble(baseline),p = "base")
  tibble_10 <- tibble(gg_tibble(cp_10),p = "p10")
  tibble_90 <- tibble(gg_tibble(cp_90),p = "p90")
  tibble_combined <- bind_rows(tibble_10, tibble_90)
  # Combining all tables
  complete <- bind_rows(tibble_base,tibble_combined)
  complete <- tibble(complete, `Mortality Group` = name1)
  return(complete)
}

# filter by percentile
percentis <- avg_HNE %>% filter (percentile %in% c(0.99))
t1 <- table.pred.var(meta_all0, lat, meta_model_all, 
                     "all", "lat") %>% filter (x %in% round(percentis$value))
t2 <- table.pred.var(meta_CVD0, lat, meta_model_CVD, 
                     "CVD","lat") %>% filter (x %in% round(percentis$value))
t3 <- table.pred.var(meta_RD0, lat, meta_model_RD, 
                     "RD", "lat") %>% filter (x %in% round(percentis$value))

write.xlsx(bind_rows(t1,t2,t3), "01.tables//supp//05.table_meta_02.xlsx")
