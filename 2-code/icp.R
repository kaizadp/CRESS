library(tidyverse)
library(googlesheets4)
theme_set(theme_bw())
options(scipen = 9999)

# -------------------------------------------------------------------------

sample_key = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/")
analytes = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KVmtSHtLs9ljyzNVzbpEQ-GWavy0_Bpi-gCd8MOw6uc/")
sample_weights = read_sheet("https://docs.google.com/spreadsheets/d/1HadvtmmzzITDaXQXLhnM7m2HURLJEilQTGbAC5LkAgc")
ICP_FILEPATH = "1-data/data_raw/icpms"

#
# ICP - IMPORT ------------------------------------------------------------

icp_data <- 
  list.files(path=ICP_FILEPATH, pattern = ".csv", full.names = TRUE) %>% 
  lapply(read_csv, id = "source") %>% 
  bind_rows %>% 
  mutate(source = basename(source)) %>% 
  rename(X = `...2`)


#
# ICP - PROCESS -----------------------------------------------------------

icp_columns = 
  icp_data %>% 
  mutate_at(vars(-c(X, source)), as.numeric) %>% 
  pivot_longer(cols = -c(X, source), values_to = "ppb", names_to = "analyte") %>% 
  mutate(sample_ID = str_extract(X, "CRESS_[0-9]{3}"),
         extraction = str_extract(X, "CRESS_[0-9]{3}[A-Z]"),
         extraction = str_remove(extraction, "CRESS_[0-9]{3}")) %>% 
  mutate(extraction = case_match(extraction, 
                                 "A" ~ "A: DTPA",
                                 "B" ~ "B: Water",
                                 "C" ~ "C: HCl",
                                 "D" ~ "D: Dithionite",
                                 "E" ~ "E: Pyrophosphate")) %>% 
  mutate(ppb = if_else(ppb < 0, 0, ppb)) %>% 
  mutate(ppb = if_else(ppb < 0, 0, ppb)) 


icp_blanks = 
  icp_columns %>% 
  filter(grepl("blank", X, ignore.case = T)) %>% 
  separate(X, sep = " Blank ", into = c("a", "extraction")) %>% 
  dplyr::select(source, extraction, analyte, ppb) %>% 
  mutate(extraction = str_remove_all(extraction, " "),
         extraction = case_match(extraction, 
                                 "DTPA" ~ "A: DTPA",
                                 "Water" ~ "B: Water",
                                 "HCl" ~ "C: HCl",
                                 "Dithionite" ~ "D: Dithionite",
                                 "Pyrophosphate" ~ "E: Pyrophosphate")) %>% 
  mutate(ppb = if_else(ppb < 0, 0, ppb)) %>% 
  rename(blank_ppb = ppb) %>% 
  replace(is.na(.), 0)

icp_processed = 
  icp_columns %>% 
  filter(!grepl("blank", X, ignore.case = T)) %>% 
  dplyr::select(source, sample_ID, extraction, analyte, ppb) %>% 
  filter(!is.na(sample_ID)) %>% 
  replace(is.na(.), 0) %>% 
  left_join(icp_blanks) %>% 
  mutate(ppb_blank_corr = ppb - blank_ppb,
         ppb_blank_corr = if_else(ppb_blank_corr < 0, 0, ppb_blank_corr)) %>% 
  # standardize to soil weight
  mutate(extraction_type = case_when(grepl("A:", extraction) ~ "DTPA",
                                     grepl("B:|C:|D:|E:|F:", extraction) ~ "Soil sequence"),
         volume_mL = case_when(extraction_type == "DTPA" ~ 10,
                               extraction_type == "Soil sequence" ~ 40)) %>% 
  left_join(sample_weights %>% dplyr::select(sample_ID, extraction_type, wt_g)) %>% 
  mutate(ug_g = ppb * volume_mL / (wt_g * 1000),
         mg_g = ug_g/1000) %>% 
  left_join(sample_key) %>% 
  left_join(analytes) %>% 
  dplyr::select(sample_ID, soil_name, replicate, analyte, group, extraction, ppb_blank_corr, ug_g, mg_g) %>% 
  mutate(across(where(is.numeric), round, 2))


#
# ICP - EXPORT ------------------------------------------------------------

icp_processed %>% 
  write.csv("1-data/data_processed/ICP_processed.csv", row.names = F, na = "")



