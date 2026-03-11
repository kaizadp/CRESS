
xrf_data = read.csv("1-data/data_raw/XRF/XRF.csv", skip = 1)

xrf_long = 
  xrf_data %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(CRESS_ID), names_to = "analyte", values_to = "XRF_ugg") %>% 
  mutate(XRF_ugg = as.numeric(XRF_ugg),
         XRF_ugg = replace_na(XRF_ugg, 0))


xrf_digest_compar = 
  xrf_long %>% 
  left_join(icp_digests %>% dplyr::select(CRESS_ID, analyte, ug_g)) %>% 
  rename(digest_ugg = ug_g) %>% 
#  pivot_longer(cols = -c(CRESS_ID, analyte)) %>% 
  force()

xrf_digest_compar %>% 
  ggplot(aes(x = XRF_ugg, y = digest_ugg, color = CRESS_ID))+
  geom_point()+
  facet_wrap(~analyte, scales = "free")
