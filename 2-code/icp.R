library(tidyverse)

sample_key = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18yWE-YkqX01J-qg6sd-M40_6dwYPyZVb-1iFwEEy2cM/")
data = read.csv("1-data/62037_Patel_ICPMS_2025-12-17.csv")

processed = 
  data %>% 
  mutate_at(vars(-X), as.numeric) %>% 
  pivot_longer(cols = -X, values_to = "ppb") %>% 
  drop_na() %>% 
  mutate(sample_ID = str_extract(X, "CRESS_[0-9]{3}"),
         extraction = str_extract(X, "CRESS_[0-9]{3}[A-Z]"),
         extraction = str_remove(extraction, "CRESS_[0-9]{3}")) %>% 
  mutate(extraction = case_match(extraction, "A" ~ "A: DTPA",
                                 "B" ~ "B: Water",
                                 "C" ~ "C: HCl",
                                 "D" ~ "D: Dithionite",
                                 "E" ~ "E: Pyrophosphate")) %>% 
  filter(!grepl("blank", X, ignore.case = T)) %>% 
  left_join(sample_key)

processed_summary = 
  processed %>% 
  group_by(soil_name, extraction, name) %>% 
  dplyr::summarise(ppb = mean(ppb))

## 
processed %>% 
  filter(extraction == "A: DTPA") %>% 
  ggplot(aes(x = sample_ID, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
processed %>% 
  filter(extraction != "A: DTPA") %>% 
  ggplot(aes(x = sample_ID, y = ppb, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##
processed_summary %>% 
  filter(extraction == "A: DTPA") %>% 
  ggplot(aes(x = soil_name, y = ppb/1000, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

processed_summary %>% 
  filter(extraction != "A: DTPA") %>% 
  ggplot(aes(x = soil_name, y = ppb, fill = extraction))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
