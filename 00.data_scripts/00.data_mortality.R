# Data  -------------------------------------------------------

# Mortality Data # 
library(microdatasus)
library(brclimr)
library(tidyverse)

# Fetching Mortality Data  ------------------------------------------------
# list of cities
capitals <- c('1100205','1200401','1302603','1400100','1501402','1600303','1721000',
              '2111300','2211001','2304400','2408102','2507507','2611606','2704302',
              '2800308','2927408','3106200','3205309','3304557','3550308','4106902',
              '4205407','4314902','5002704','5103403','5208707','5300108') %>% 
  str_sub(end= 6) # removing last digit to be in accordance with DATASUS data
uf <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", 
        "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", 
        "SP", "SE", "TO")

# mortality
for (i in uf){
  print(i)
  dados_m <- fetch_datasus(year_start = 2000, year_end = 2019, 
                           information_system = "SIM-DO",uf = i,
                           vars = c("DTOBITO","DTNASC","IDADE","TIPOBITO",
                                    "SEXO","RACACOR","CODMUNRES","CAUSABAS"))
  assign(paste0('dados',i),dados_m)
}
# Binding capitals
dados_m <- bind_rows(dadosAC, dadosAL, dadosAP, dadosAM, dadosBA,
                     dadosCE, dadosDF, dadosES, dadosGO, dadosMA,
                     dadosMT, dadosMS, dadosMG, dadosPA, dadosPB,
                     dadosPR, dadosPE, dadosPI, dadosRJ, dadosRN,
                     dadosRS, dadosRO, dadosRR, dadosSC, dadosSP,
                     dadosSE, dadosTO) %>% mutate(CODMUNRES = CODMUNRES %>% 
                                                    str_sub(end=6)) #Removing last digit to be always (some have 7)
# filtering capitals
dados_m <- dados_m %>% filter(CODMUNRES %in% capitals)
# Processing 
dados_m <- process_sim(dados_m)
dados_m <- dados_m %>% mutate(
  DTNASC = DTNASC %>% as.Date(),
  DTOBITO = DTOBITO %>% as.Date(),
  SEXO = SEXO %>% as.factor(),
  RACACOR = RACACOR %>% as.factor,
  CAUSABAS = CAUSABAS %>% as.factor()%>% str_sub(end  = 3),
  CODMUNRES = CODMUNRES %>% as.factor(),
  dia = DTOBITO %>% day,
  semana = DTOBITO %>% week,
  ano = DTOBITO %>% year,
  mes = DTOBITO %>% month,
  dia_semana = DTOBITO %>% weekdays())
# Saving capitals data
write_rds(dados_m,'00.data/data_m_capitals.rds')



# -------------------------------------------------------------------------
# Organizing_data #
# Mortality data ----------------------------------------------------------
# loading data
dados_m <- read_rds('00.data/data_m_capitals.rds')[,c(1,2,4:12,21:25)]
cid <- readxl::read_excel("00.data//CID-10.xltx") %>% dplyr::select(CAT,DESCRICAO)
mun_names <- readxl::read_excel("00.data//codigos_municipios.xls")[,12:13] 
colnames(mun_names) <- c('CODMUNRES','mun_name')
mun_names <- mun_names %>% 
  mutate(CODMUNRES = CODMUNRES %>% str_sub(end = 6))
brpop <- brpop::mun_pop() 
brpop <- brpop %>% filter(age_group == "Total") %>% 
  select(!age_group) %>% 
  mutate(mun = mun %>% 
           as.factor())
# Filtering years 2000-2019
dados_m <- dados_m %>% filter(ano >= 2000 & ano <= 2019)
dados_m <- dados_m %>% mutate(
  IDADE = floor(time_length(difftime(DTOBITO,DTNASC), "year")))
# Due to impossible ages, lets filter it 
# (Ages above 130 and one which has -1, but can be ruled out as a death of a newborn)
dados_m <- dados_m %>% filter (!IDADE >= 130)
dados_m <- dados_m %>%  mutate(IDADE = ifelse(dados_m$IDADE < 0 , 0, IDADE))
# ICDs Categories
cat_cid <- function(cid) {
  if (substr(cid, 1, 1) %in% c("J")) {
    return("RD")
  } else if (substr(cid, 1, 1) %in% c("I")) {
    return("CVD")
  } else {
    return("other")
  }
}
# categorizing ICDs
dados_m$cat_cid <-  sapply(dados_m$CAUSABAS, cat_cid)
# calculating the daily frequency
data_CVD_RD <- dados_m %>% group_by(DTOBITO,CODMUNRES,
                             dia_semana,mes,ano,cat_cid) %>% 
  summarise(n = n())
data <- data_CVD_RD %>% 
  pivot_wider(names_from = "cat_cid", values_from = "n", values_fill = 0) %>% 
  mutate(n = CVD + RD + other) %>% 
  select(-other)

# Completing dates that has no deaths
data <- data %>% ungroup() %>% 
  complete(DTOBITO = seq.Date(as.Date('2000-01-01'), 
                              as.Date('2019-12-31'),by ="day"),CODMUNRES)
data <- data %>% mutate(dia_semana = DTOBITO %>% weekdays(),
                        mes = DTOBITO %>% month(),
                        ano = DTOBITO %>% year(),
                        n = ifelse(is.na(n),0,n),
                        CVD = ifelse(is.na(CVD),0,CVD),
                        RD = ifelse(is.na(RD),0,RD))
# cleaning data, removing non useful cols and adding names to the codes (city and ICD)
# And population
data <- data %>% drop_na(DTOBITO) %>% 
  left_join(mun_names) %>% 
  left_join(brpop,by =c('CODMUNRES'= 'mun', 'ano' = 'year'))

# Fixing names
colnames(data) <- c("date","mun_code","dow","month","year","n_CVD","n_RD",
                    "n","mun_name","pop")
# saving data 
data %>% write_rds("00.data//data_m.rds")
