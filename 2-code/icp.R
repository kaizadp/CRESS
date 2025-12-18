library(tidyverse)
theme_set(theme_bw())
options(scipen = 9999)

# -------------------------------------------------------------------------

sample_key = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/")
analytes = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KVmtSHtLs9ljyzNVzbpEQ-GWavy0_Bpi-gCd8MOw6uc/")
#data = read.csv("1-data/62037_Patel_ICPMS_2025-12-17.csv")

ICP_FILEPATH = "1-data/data_raw/icpms"

# -------------------------------------------------------------------------


icp_data <- 
  list.files(path=ICP_FILEPATH, pattern = ".csv", full.names = TRUE) %>% 
  lapply(read_csv, id = "source") %>% 
  bind_rows %>% 
  mutate(source = basename(source)) %>% 
  rename(X = `...2`)




columns = 
  data %>% 
  mutate_at(vars(-X), as.numeric) %>% 
  pivot_longer(cols = -X, values_to = "ppb", names_to = "analyte") %>% 
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
  #%>% 
  #filter(!is.na(sample_ID)) %>% 
  replace(is.na(.), 0)

blanks = 
  columns %>% 
  filter(grepl("blank", X, ignore.case = T)) %>% 
  separate(X, sep = " Blank ", into = c("a", "extraction")) %>% 
  dplyr::select(extraction, analyte, ppb) %>% 
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

all_reps = 
  columns %>% 
  filter(!grepl("blank", X, ignore.case = T)) %>% 
  dplyr::select(sample_ID, extraction, analyte, ppb) %>% 
  filter(!is.na(sample_ID)) %>% 
  replace(is.na(.), 0) %>% 
  left_join(blanks) %>% 
  mutate(ppb_blank_corr = ppb - blank_ppb,
         ppb_blank_corr = if_else(ppb_blank_corr < 0, 0, ppb_blank_corr)) %>% 
  left_join(sample_key) %>% 
  left_join(analytes)

processed_summary = 
  all_reps %>% 
  group_by(soil_name, extraction, group, analyte) %>% 
  dplyr::summarise(ppb = mean(ppb_blank_corr),
                   ppb_se = sd(ppb_blank_corr)/sqrt(n()),
                   ppm = ppb/1000) %>% 
  mutate(across(where(is.numeric), round, 2))

#
## by core, all reps ----
processed %>% 
  filter(extraction == "A: DTPA") %>% 
  ggplot(aes(x = sample_ID, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
processed %>% 
  filter(extraction != "A: DTPA") %>% 
  ggplot(aes(x = sample_ID, y = ppb, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
## summary ----
processed_summary %>% 
  filter(extraction == "A: DTPA") %>% 
  ggplot(aes(x = soil_name, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

processed_summary %>% 
  filter(extraction != "A: DTPA" & 
           group == "REEs") %>% 
  ggplot(aes(x = soil_name, y = ppb, fill = extraction))+
  geom_bar(stat = "identity")+
  labs(title = "REEs")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

processed_summary %>% 
  filter(extraction != "A: DTPA" & 
           group == "CMMs and metals") %>% 
  ggplot(aes(x = soil_name, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  labs(title = "CMMs and metals")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

processed_summary %>% 
  filter(extraction != "A: DTPA" & 
           group == "cations") %>% 
  ggplot(aes(x = soil_name, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  labs(title = "cations")+
  facet_wrap(~analyte, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
