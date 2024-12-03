# Data  -------------------------------------------------------
## Data for meta-predictions

# Processing census 2010 --------------------------------------------------
capitals <- c('1100205','1200401','1302603','1400100','1501402','1600303','1721000',
              '2111300','2211001','2304400','2408102','2507507','2611606','2704302',
              '2800308','2927408','3106200','3205309','3304557','3550308','4106902',
              '4205407','4314902','5002704','5103403','5208707','5300108') %>% 
  str_sub(end= 6)
# chosen variables
vars <- c("Codmun6", "Município","GINI","THEIL","RDPC","PMPOB","pesourb","pesotot",
          "PESO65","MULHERTOT","HOMEMTOT")
# loading
library(readxl)
census <- read_xlsx("00.data//censo_agregado.xlsx", sheet = 2) %>% 
  filter(ANO == 2010 & Codmun6 %in% capitals) %>% select(all_of(vars)) %>% 
  janitor::clean_names() %>% write_rds("00.data//census_processed.rds")
# processing
census <- census %>% mutate(
  urb_p = pesourb/pesotot,
  p_65 = peso65/pesotot,
  fem_p = mulhertot/pesotot
) %>% 
  select(!c(pesourb, peso65,mulhertot,homemtot))
# saving
census %>%  write_rds("00.data//census_processed.rds")

