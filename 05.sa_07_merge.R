# Sensitivity Analysis 05  ------ DF other -------------------------------------#
# Merging s.a tables

library(tidyverse)
library(xlsx)
# loop
tib <- tibble()
for (i in 1:9){
  liltib <- read.xlsx(paste0('04.sensitivity//02.tables//table_sa_0',i,".xlsx"), 
                      sheetIndex = 1)
  tib <- bind_rows(tib,liltib)
}
# filtering excess information
tib <- tib %>% select(-c(NA., HNE, percentil,pred)) %>%
  mutate(
    # fit = round(fit,2),
    CI = paste0("(", round(low, 2), " - ", round(high, 2), ")")) %>%
  select(fit, CI, `Mortality.Group`, sensitivity_analysis) %>% arrange(Mortality.Group)
# Saving 
write.xlsx(tib, "01.tables//supp//06.sensitivity_table.xlsx")
