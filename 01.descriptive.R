# Descriptive analysis ---------------------------------------------------------
# Loading packages
library(tidyverse)
library(geofacet)
library(xlsx)
library(ggspatial) 
options(scipen=999)
theme_set(theme_bw())
# Loading data
data <- read_rds("00.data//data_m_w.rds")
# grid data
code_mun <- readxl::read_excel("00.data//codigos_municipios.xls")[,c(2, 12:13)] %>% 
  janitor::clean_names() %>% 
  mutate(code_muni = codigo_municipio_completo %>%
           str_sub(end =6)) %>% 
  filter (code_muni %in% unique(data$mun_code))
grid <- read.csv("00.data//br_states_grid.csv") %>% 
  left_join(code_mun, by = c("name" = "nome_uf")) 
grid <- grid %>% select(!c(3,4,6,8))
colnames(grid) <- c("row","col","code","name")
# merging
data <- left_join(data,grid, by = c("mun_name"="name"))

# brazilian map -----------------------------------------------------------
brazil <- geobr::read_country()
regions_map <- geobr::read_region()
mun <- geobr::read_municipality() %>%
  mutate(code_muni= code_muni %>% str_sub(end = 6)) %>% filter(code_muni %in% unique(data$mun_code))
# plotting map
ggplot(mun)+
  geom_sf(data = brazil, fill = "white")+
  geom_sf(data = regions_map, aes(fill =name_region ))+
  geom_sf(aes(color = "steelblue"))+    
  geom_sf_text(aes(label = name_muni), nudge_x = -1.2, nudge_y = 1.0)+
  # scale_fill_manual(values = c("steelblue"))+
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "br") +
  labs( x = "Longitude", y = "Latitude", fill = "City Area",
        caption = "SIRGAS 2000 | Source: Instituto Brasileiro de Geografia e Estatística")+
  theme(legend.text = element_blank(),
        legend.position = "bottom")


ggplot() +
  geom_sf(data = brazil, fill = "white", color = "gray") +
  geom_sf(data = regions_map, aes(fill = name_region), color = "black") +
  scale_fill_manual(values = scales::brewer_pal(palette = "Blues")(length(unique(regions_map$name_region))),
                    name = "Regions") +
  geom_sf(data = mun, aes(color = "City Area"), size = 1) +
  scale_color_manual(values = c("steelblue"), name = "Legend") +
  geom_sf_text(data = mun, aes(label = name_muni), nudge_x = -1.2, nudge_y = 1.0) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "br") +
  labs(x = "Longitude", y = "Latitude",
       caption = "SIRGAS 2000 | Source: Instituto Brasileiro de Geografia e Estatística"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

ggsave("02.plots//main//01.plot.tiff", width = 10, height = 10, dpi = 600)
# Mortality ---------------------------------------------------------------
# Distribution of mortality

# all-year
mortality_Y <- data %>%  group_by (mun_name, Region) %>% 
  summarise(Population_mean = round(pop %>% mean),
            All_Cause_Y = sum(n),
            CVD_Y = sum(n_CVD),
            RD_Y = sum(n_RD))
# Study period
mortality_SP <- data %>%   subset(month %in% c(1:3,11:12)) %>%  
  group_by (mun_name,Region) %>% 
  summarise(    All_Cause_SP = sum(n),
                CVD_SP = sum(n_CVD),
                RD_SP = sum(n_RD))
# join tables
mortality <- left_join( mortality_Y,mortality_SP) %>% arrange(Region)
# reorder
mortality <- mortality[,c(1,2,3,4,7,5,8,6,9)]
# Sum of mortality
sum_mortality <- mortality %>% ungroup() %>% 
  summarise(mun_name = "total",
            Population_mean = sum(Population_mean),
            All_Cause_Y = sum(All_Cause_Y),All_Cause_SP = sum(All_Cause_SP),
            CVD_Y = sum(CVD_Y),CVD_SP = sum(CVD_SP),
            RD_Y = sum(RD_Y),RD_SP = sum(RD_SP))
mortality <- bind_rows(mortality,sum_mortality)
#saving
write.xlsx(mortality %>% ungroup(),'01.tables//supp//01.table_mortality.xlsx')

# Weather variables -------------------------------------------------------
# T mean
table_tmean <- data %>%  group_by (mun_name, Region) %>% 
  summarize(
    Mean=round(mean(mean),2),
    Min=round(min(mean),2), 
    Max=round(max(mean),2)) 
# RH
table_rh <- data %>% group_by (mun_name,Region) %>% 
  summarize(
    Mean_RH=round(mean(rh),2))
# HNE
table_HNE <- data %>%  group_by (mun_name,Region) %>% 
  summarize(
    Mean_HNE=round(mean(HNE),2),
    Min_HNE=round(min(HNE),2), 
    Max_HNE=round(max(HNE),2),
    p99_HNE = round(quantile(HNE,0.99),2)
  ) %>% arrange(desc(mun_name))
# Join
weather <-left_join(table_tmean,table_rh) %>% arrange(Region)
weather <-left_join(weather,table_HNE) %>% arrange(Region)
# saving
write.xlsx(weather %>% ungroup(),'01.tables//supp//02.table_weather.xlsx')
# Mean Temperature --------------------------------------------------------

table_tmean_month <- data %>% group_by(month) %>% 
  summarize (
    Mean = round(mean(mean),2)
  ) %>% arrange(desc(Mean))
# box plot by month
plot_tmean_month <- data %>% ggplot()+ 
  geom_boxplot(aes(month %>% as.factor(),mean), color = "steelblue")+
  facet_geo(~mun_name,grid = grid, scales = "free_y")+
  xlab('Month')+
  ylab('Mean Temperature (T°C)')
#Saving
table_tmean_month %>% write.xlsx('01.tables//supp//03.table_tmean.xlsx')
ggsave(plot = plot_tmean_month, "02.plots//supp//01.plot_tmean.tiff", 
       dpi = 600, height = 10, width = 10 )

# HNE ---------------------------------------------------------------------
# Bar plot sum
plot_bar_HNE <- data %>% group_by(mun_name, month) %>% 
  summarize(HNE= sum(HNE)) %>% 
  ggplot()+ 
  geom_bar(aes(x = month, y = HNE), stat = "identity", fill = "steelblue")+
  facet_geo(~mun_name,grid = grid, scales = "free_y")+
  xlab('Month')+
  ylab('Hot Night Excess Sum')
# saving
ggsave(plot = plot_bar_HNE, "02.plots//supp//02.plot_HNE.tiff", 
       dpi = 600, height = 10, width = 10 )

