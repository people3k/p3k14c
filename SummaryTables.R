library(ggpubr)
library(tidyverse)


RC <- read_csv(
  here::here("data/raw_data/radiocarbon_dates_scrubbedv5_1.csv")
)

RCraw <- read_csv(
  here::here("data/raw_data/radiocarbon_dates_MUSHv5_0_datasetReview1.csv")
)
  
  

# NewColumns <- read_csv(
#   here::here("data/raw_data/NewColumns.csv")
# ) %>% 
#   unique() %>% 
#   dplyr::filter(!is.na(Source),
#                 !is.na(LabID))
# 
# RC2 <- left_join(RC, NewColumns, by = c("LabID", "Source")) 
# 
# write_csv(RC2, 
#           here::here("data/derived_data/RC3.csv"),
#           na= "")
# 



RC_sum_continent <- RCraw %>% 
  group_by(Continent) %>% 
  summarize(
    N = n(),
    meanError = mean(Error),
    medianError = median(Error)
  )

RC_sum_country <- RC %>% 
  group_by(Country) %>% 
  summarize(
    N = n(),
    meanSD = mean(Sd),
    medianSD = median(Sd)
  )

RC_sum_region2 <- RC %>% 
  mutate(`Loc?` = ifelse(LocAccuracy != "0", TRUE, FALSE)) %>% 
  group_by(Continent,Region, `Loc?`) %>% 
  summarize(
    N = n(),
    meanSD = mean(Sd),
    medianSD = median(Sd)
  ) %>% 
  pivot_wider(id_cols = c(Continent, Region), 
              names_from = c(`Loc?`),
              values_from = c(N, meanSD, medianSD))

RC_sum_region <- RC %>% 
  mutate(`Loc?` = ifelse(LocAccuracy != "0", TRUE, FALSE)) %>% 
  group_by(Region, `Loc?`) %>% 
  summarize(
    N = n(),
    meanSD = mean(Sd),
    medianSD = median(Sd)
  ) %>% 
  pivot_wider(id_cols = c(Region), 
              names_from = c(`Loc?`),
              values_from = c(N, meanSD, medianSD))


RC_sum_continent %>% write_csv(
  here::here("data/derived_data/RC_sum_continent.csv")
)

RC_sum_country %>% write_csv(
  here::here("data/derived_data/RC_sum_country.csv")
)

RC_sum_region2 %>% write_csv(
  here::here("data/derived_data/RC_sum_region2.csv")
)

RC_sum_region %>% write_csv(
  here::here("data/derived_data/RC_sum_region.csv")
)


