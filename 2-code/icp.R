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
  pivot_longer(cols = -c(X, source), values_to = "ppb", names_to = "analyte") %>% 
  filter(grepl("CRESS", X, ignore.case = T)) %>% 
  drop_na() %>% 
  mutate(sample_ID = str_extract(X, "CRESS_[0-9]{3}"),
         extract_code = str_extract(X, "CRESS_[0-9]{3}[A-Z]"),
         extract_code = str_remove(extract_code, "CRESS_[0-9]{3}")) %>% 
  mutate(ppb = as.numeric(ppb),
         ppb = if_else(ppb < 0, 0, ppb),
         ppb = replace_na(ppb, 0))

## DIGESTS
icp_digests = 
  icp_columns %>% 
  filter(grepl("Digest", source)) %>% 
  filter(grepl("CRESS", X, ignore.case = T)) %>% 
  rename(CRESS_ID = X) %>% 
  dplyr::select(-c(sample_ID, extract_code, source)) %>% 
  group_by(CRESS_ID, analyte) %>% 
  dplyr::summarise(ppb = mean(ppb)) %>% 
  ungroup() %>% 
  mutate(ug_g = ppb * 25 / (0.1 * 1000), #25 mL, 0.1 g
         mg_g = ug_g/1000) %>% 
  mutate(across(where(is.numeric), round, 2))


## EXTRACTS
icp_blanks = 
  icp_columns %>% 
  filter(grepl("Extracts", source)) %>% 
  filter(grepl("blank", X, ignore.case = T)) %>% 
  dplyr::select(source, X, analyte, ppb) %>%
  mutate(extract_code = str_extract(X, "Blank_[A-Z]"),
         extract_code = str_remove(extract_code, "Blank_")) %>% 
  rename(blank_ppb = ppb) %>% 
  dplyr::select(source, analyte, extract_code, blank_ppb)

icp_samples = 
  icp_columns %>% 
  filter(grepl("Extracts", source)) %>% 
  filter(!grepl("blank", X, ignore.case = T)) %>% 
  filter(!is.na(sample_ID)) %>% 
  dplyr::select(source, analyte, sample_ID, extract_code, ppb) %>% 
  left_join(icp_blanks) %>% 
  mutate(ppb_blank_corr = ppb - blank_ppb,
         ppb_blank_corr = if_else(ppb_blank_corr < 0, 0, ppb_blank_corr)) %>% 
  left_join(extraction_key_extracts) %>% 
  mutate(ug_g = ppb * volume_mL / (wt_g * 1000)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  left_join(extraction_key_soils) %>% 
#  left_join(analytes) %>% 
#  dplyr::select(sample_ID, sample_name, analyte, group, extraction_sequence, extract_code,  extract, fraction, ppb_blank_corr, ug_g, mg_g) %>% 
#  mutate(across(where(is.numeric), round, 2)) %>% 
  left_join(soil_key_digestions) %>% 
  dplyr::select(sample_ID, sample_name, CRESS_ID, analyte, extraction_sequence, extract_code, extract, fraction , ug_g)

## calculate residual fraction by substracting the total of EXTRACTS from the DIGESTS
icp_residual = 
  icp_samples %>% 
#  filter(grepl("Sequence", extraction_sequence)) %>% 
  group_by(sample_ID, sample_name, CRESS_ID, analyte, extraction_sequence) %>% 
  dplyr::summarise(sum_ugg = sum(ug_g)) %>% 
  mutate(
    fraction = "R: residual",
         extract = "R: residual",
         extract_code = "R") %>% 
  left_join(icp_digests %>% dplyr::select(CRESS_ID, analyte, ug_g) %>% rename(digest_ugg = ug_g)) %>% 
  mutate(ug_g = digest_ugg - sum_ugg,
         ug_g = case_when(ug_g < 0 ~ 0, .default = ug_g)) %>%  
  dplyr::select(-c(sum_ugg, digest_ugg))


icp_processed = 
  icp_samples %>% 
  bind_rows(icp_residual) %>% 
  mutate(mg_g = ug_g/1000) %>% 
  group_by(sample_ID, extraction_sequence, analyte) %>% 
  dplyr::mutate(total_ugg = sum(ug_g),
                percent = 100 * ug_g/total_ugg) %>% 
  left_join(analytes)


#
# ICP - EXPORT ------------------------------------------------------------

icp_processed %>% 
  write.csv("1-data/data_processed/ICP_processed.csv", row.names = F, na = "")

icp_digests %>% 
  dplyr::select(-ppb) %>% 
  write.csv("1-data/data_processed/ICP_processed_digests.csv", row.names = F, na = "")
