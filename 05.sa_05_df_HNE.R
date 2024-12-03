
# Sensitivity Analysis 05  ------ DF HNE ---------------------------------------
# Individual models and selection -----------------------------------------
### FIRST STAGE ###
# loading packages
library(tidyverse)
library(mgcv)
library(splines)
library(stats)
library(dlnm)
library(mixmeta)
library(xlsx)
library(patchwork)
options(scipen=999)
theme_set(theme_bw())

# Lag
df <- c(4,5)
lag <- 14
# SA version
SA <- c("06","07")

for (s in 1:2){
  # loading data
  data <- read_rds(paste0("00.data//data_m_w.rds"))
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
    # Relative humidity
    RH <- ns(data_city$rh, df = 3)
    # hot nights index
    # Points for knots in HNE crossbasis
    p60_HNE = quantile(data_city$HNE,0.6)
    p90_HNE = quantile(data_city$HNE,0.9)
    # HNE
    HN <- crossbasis(data_city$HNE, lag = lag, 
                     arglag = list(fun = "ns", knots = df),
                     argvar = list(fun = "ns", knots = c(p60_HNE,p90_HNE)),
                     group = data_city$year) 
    
    
    # Model -------------------------------------------------------------------
    mod_n <- glm(n ~ HN +
                   cb_tmean +
                   RH+
                   dow +
                   ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
                 family = quasipoisson(), data = data_city)
    mod_CVD <- glm(n_CVD ~ HN +
                     cb_tmean +
                     RH+
                     dow +
                     ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
                   family = quasipoisson(), data = data_city)
    mod_RD <- glm(n_RD ~ HN +
                    cb_tmean +
                    RH+
                    dow +
                    ns(yday(data_city$date), df=4)*factor(year(data_city$date)), # for seasonality
                  family = quasipoisson(), data = data_city, maxit = 50)
    # # Extracting coef
    # n tota
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
  coef_all_n %>%  write_rds(paste0("04.sensitivity//01.output//coef_all_",SA[s],".rds"))
  vcov_all_n %>% write_rds(paste0("04.sensitivity//01.output//vcov_all_",SA[s],".rds"))
  coef_all_CVD %>%  write_rds(paste0("04.sensitivity//01.output//coef_CVD_",SA[s],".rds"))
  vcov_all_CVD %>% write_rds(paste0("04.sensitivity//01.output//vcov_CVD_",SA[s],".rds"))
  coef_all_RD %>%  write_rds(paste0("04.sensitivity//01.output//coef_RD_",SA[s],".rds"))
  vcov_all_RD %>% write_rds(paste0("04.sensitivity//01.output//vcov_RD_",SA[s],".rds"))
  
  # Meta Analysis -------------------------------------------
  ### SECOND STAGE ###
  # loafing data
  data <- read_rds(paste0("00.data//data_m_w.rds")) %>% 
    mutate(mun_name = mun_name %>% as.factor())
  n_var <- list("all","CVD", "RD")
  for(o in n_var){
    print(o)
    # loading coefs ------------------------------------------------------------
    coef_all <- read_rds(paste0("04.sensitivity//01.output//coef_",o,"_",SA[s],".rds"))
    vcov_all <- read_rds(paste0("04.sensitivity//01.output//vcov_",o,"_",SA[s],".rds"))
    # City level meta-data
    city_info <- coef_all %>% rownames() %>% tibble(mun_name = .) %>% 
      left_join(data) %>% dplyr::select(c(1,15,16)) %>% unique
    # Meta analysis -----------------------------------------------------------
    # cities as random effects
    # one level random effects (cities within a region)
    meta_model <- mixmeta(coef_all,vcov_all, random = ~1|Region/mun_name, 
                          data = city_info, method="ml")
    # Saving model in environment
    assign(paste0("meta_model_",o),meta_model)
    
    # Predictions -------------------------------------------------------------
    # Base for predictions -- using same attributed of the cross-basis in estimation
    # Avg HNE distribution
    percentiles <- seq(0, 999, by = 1) / 1000
    avg_HNE = quantile(data$HNE, probs = percentiles)
    avg_HNE = tibble(percentile = percentiles, value = avg_HNE)
    # knots
    knots = avg_HNE %>%  filter(percentile %in% c(0.50,0.90))
    knots = knots$value
    # base
    x_argvar = avg_HNE$value 
    b_argvar = onebasis(x_argvar, fun = "ns",knots = knots)
    # Random-effects 
    # Overall meta-analysis
    pred_overall <- crosspred(b_argvar, 
                              coef = coef(meta_model), vcov = vcov(meta_model),cen =0,
                              model.link="log", by= 0.1)
    
    ## by regions
    region_name <- c("North","Northeast","Southeast","South","Midwest")
    # Predctions generated by blups 
    blup_region <- unique(blup(meta_model, pi=T, level= 1, vcov = T))
    for (i in c(1:5)){
      # BLUPs
      blup_temp <- blup_region[[i]]
      # SEPARATED PREDICTION FOR EACH REGION
      assign(x = paste0('pred_',region_name[i]),
             crosspred(b_argvar, 
                       coef = blup_temp$blup, vcov = blup_temp$vcov,cen = 0,
                       model.link="log", by = 0.1))
    }
    # PLOTS -------------------------------------------------------------------
    print("plotting")
    #Extract values
    tib_plot <- function(pred){
      
      return(tibble(x = pred$predvar,
                    fit = pred$matRRfit, 
                    low = pred$matRRlow, 
                    high = pred$matRRhigh))
    }
    # Overall Prediction
    P0 <-  ggplot(pred_overall %>% tib_plot(),aes(x=x,y = fit))+
      geom_line(color =  'steelblue')+
      geom_ribbon(aes(ymin = low, ymax = high),fill = "steelblue", alpha = 0.2) +
      geom_hline(aes(yintercept = 1))+
      geom_vline(aes(xintercept = 37), linetype = 2, color = "steelblue")+
      xlab("HNE")+
      ylab("RR")+
      ggtitle("Overall")
    
    # Regions
    regions_pred <- list(pred_North,pred_Northeast,pred_Midwest,
                         pred_Southeast,pred_South)
    regions_names <- c("North", "Northeast","Midwest","Southeast","South")
    for (i in 1:5){
      pred <- regions_pred[[i]]
      P <-  ggplot(pred %>% tib_plot(),aes(x=x,y = fit))+
        geom_line(color =  'steelblue')+
        geom_ribbon(aes(ymin = low, ymax = high),fill = "steelblue", alpha = 0.2) +
        xlab("HNE")+
        ylab("RR")+
        geom_hline(aes(yintercept = 1))+
        geom_vline(aes(xintercept = 37), linetype = 2, color = "steelblue")+
        ggtitle(paste0(regions_names[i]))
      
      
      assign(paste0("p_",i),P)
    }
    
    e <- ifelse(o == "all","All Cause Mortality",o)
    e <- ifelse(o == "CVD","Cardiovascular Mortality",e)
    e <- ifelse(o == "RD","Respiratory Disease Mortality",e)
    # patchwork
    layout <- "
AAAABB
AAAACC
EEFFDD
"
    plot_e <- P0+ p_1+p_2+p_3+p_4+p_5 +
      plot_layout(design = layout)+
      plot_annotation(title = paste0(e," HNE df =",df[s]))
    ggsave(paste0("04.sensitivity//03.plots//sa_",o,"_",SA[s],".png"),
           dpi = 300,height = 6, width = 10 )
    # Tables
    # RR values in different percentiles
    percentis <- avg_HNE %>% filter (percentile %in% c(0.99))
    regions_pred <- list(pred_North,pred_Northeast,pred_Midwest,
                         pred_Southeast,pred_South, pred_overall)
    regions_names <- c("North", "Northeast","Midwest","Southeast","South","Overall")
    pred_table <- tibble()
    for (a in 1:6){
      
      pred_table <- bind_rows(pred_table,regions_pred[[a]] %>% tib_plot() %>% 
                                filter (x %in% round(percentis$value)) %>% 
                                bind_cols(percentil  = percentis$percentile, 
                                          pred = regions_names[a]))  
      
    }
    colnames(pred_table) <- c("HNE", "fit", "low", "high", "percentil", "pred")
    # saving as object
    assign(paste0("table_",o),pred_table)
  }
  # uniting tables
  table_all <- tibble(table_all, `Mortality Group` = "All Causes" )
  table_CVD <- tibble(table_CVD, `Mortality Group` = "CVD" )
  table_RD <- tibble(table_RD, `Mortality Group` = "RD" )
  table_pred_complete <- bind_rows(table_all,table_CVD,table_RD)
  table_pred_complete <- bind_cols(table_pred_complete, 
                                   sensitivity_analysis = paste0("HNE df =",df[s]))
  # Filter only overall table
  table_pred_complete <- table_pred_complete %>% filter (pred == "Overall") 
  
  # Saving table 
  write.xlsx(table_pred_complete, 
             paste0("04.sensitivity//02.tables//table_sa_",SA[s],".xlsx"))
}



