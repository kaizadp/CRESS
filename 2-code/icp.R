library(tidyverse)
library(googlesheets4)
theme_set(theme_bw())
options(scipen = 9999)

# -------------------------------------------------------------------------

# sample_key = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/")
# sample_weights = read_sheet("https://docs.google.com/spreadsheets/d/1HadvtmmzzITDaXQXLhnM7m2HURLJEilQTGbAC5LkAgc")

extraction_key_soils = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/",
                                                 sheet = "EXTRACTION_KEY_SOILS")
extraction_key_extracts = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/",
                                                    sheet = "EXTRACTION_KEY_EXTRACTS")
soil_key_digestions = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/",
                                                sheet = "SOIL_KEY_DIGESTIONS")

analytes = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KVmtSHtLs9ljyzNVzbpEQ-GWavy0_Bpi-gCd8MOw6uc/")
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
  mutate(#X = str_replace(X, "CRESS_[0-9]{3}_", "CRESS_[0-9]{3}"),
         sample_ID = str_extract(X, "CRESS_[0-9]{3}"),
         extract_code = str_extract(X, "CRESS_[0-9]{3}[A-Z]"),
         extract_code = str_remove(extract_code, "CRESS_[0-9]{3}")) %>% 
 # mutate(extraction = case_match(extraction, 
 #                                "A" ~ "A: DTPA",
 #                                "B" ~ "B: Water",
 #                                "C" ~ "C: HCl",
 #                                "D" ~ "D: Dithionite",
 #                                "E" ~ "E: Pyrophosphate")) %>% 
  mutate(ppb = if_else(ppb < 0, 0, ppb)) %>% 
  mutate(ppb = if_else(ppb < 0, 0, ppb)) 


icp_blanks = 
  icp_columns %>% 
  filter(grepl("Extracts", source)) %>% 
  filter(grepl("blank", X, ignore.case = T)) %>% 
#  separate(X, sep = " Blank ", into = c("a", "extraction")) %>% 
  dplyr::select(source, X, analyte, ppb) %>%
  mutate(extract_code = str_extract(X, "Blank_[A-Z]"),
         extract_code = str_remove(extract_code, "Blank_")) %>% 
#  mutate(extraction = str_remove_all(extraction, " "),
#         extraction = case_match(extraction, 
#                                 "DTPA" ~ "A: DTPA",
#                                 "Water" ~ "B: Water",
#                                 "HCl" ~ "C: HCl",
#                                 "Dithionite" ~ "D: Dithionite",
#                                 "Pyrophosphate" ~ "E: Pyrophosphate")) %>% 
#  mutate(ppb = if_else(ppb < 0, 0, ppb)) %>% 
  rename(blank_ppb = ppb) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(source, analyte, extract_code, blank_ppb)

icp_samples = 
  icp_columns %>% 
  filter(grepl("Extracts", source)) %>% 
  filter(!grepl("blank", X, ignore.case = T)) %>% 
  filter(!is.na(sample_ID)) %>% 
  dplyr::select(source, analyte, sample_ID, extract_code, ppb)
  

icp_processed = 
  icp_samples %>% 
  replace(is.na(.), 0) %>% 
  left_join(icp_blanks) %>% 
  mutate(ppb_blank_corr = ppb - blank_ppb,
         ppb_blank_corr = if_else(ppb_blank_corr < 0, 0, ppb_blank_corr)) %>%
  left_join(extraction_key_extracts) %>% 
      #   
      #  # standardize to soil weight 
      #  mutate(extraction_type = case_when(grepl("A:", extraction) ~ "DTPA", 
      #                                     grepl("B:|C:|D:|E:|F:", extraction) ~ "Soil sequence"), 
      #         volume_mL = case_when(extraction_type == "DTPA" ~ 10, 
      #                               extraction_type == "Soil sequence" ~ 40)) %>%   
      #  left_join(sample_weights %>% dplyr::select(sample_ID, extraction_type, wt_g)) %>% 
  mutate(ug_g = ppb * volume_mL / (wt_g * 1000),
         mg_g = ug_g/1000) %>% 
  left_join(extraction_key_soils) %>% 
  left_join(analytes) %>% 
  dplyr::select(sample_ID, sample_name, analyte, group, extraction_sequence, extract_code,  extract, fraction, ppb_blank_corr, ug_g, mg_g) %>% 
  mutate(across(where(is.numeric), round, 2))


#
# ICP - EXPORT ------------------------------------------------------------

icp_processed %>% 
  write.csv("1-data/data_processed/ICP_processed.csv", row.names = F, na = "")



