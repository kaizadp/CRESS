library(tidyverse)
library(googlesheets4)
theme_set(theme_bw())
options(scipen = 9999)

# -------------------------------------------------------------------------

xrf_data = read.csv("1-data/data_raw/XRF/XRF.csv", skip = 1)

xrf_long = 
  xrf_data %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(CRESS_ID), names_to = "analyte", values_to = "XRF_ugg") %>% 
  mutate(XRF_ugg = as.numeric(XRF_ugg),
         XRF_ugg = replace_na(XRF_ugg, 0))

xrf_long %>% 
  write.csv("1-data/data_processed/XRF_processed.csv", row.names = F, na = "")
