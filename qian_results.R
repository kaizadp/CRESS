
library(tidyverse)
library(googlesheets4)
theme_set(theme_bw(base_size = 14))

qian_extractions = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1MXeEUzkBxeSSgKcK14SLRgcLzUC5_h7COZ-E-efX1bw/", 
                              sheet = "ICP_KFP_edits") %>% 
  janitor::clean_names() 
  

qian_extractions_long = 
  qian_extractions %>% 
  dplyr::select(-c(contains("_l")), rep, soil_weight_g, vol_ml, note) %>% 
  pivot_longer(cols = contains("g_g"))
  


qian_extractions_long %>% 
  ggplot(aes(x = extraction, y = value, color = time_hr))+
  geom_point()+
  facet_wrap(~name, scales = "free_y")
