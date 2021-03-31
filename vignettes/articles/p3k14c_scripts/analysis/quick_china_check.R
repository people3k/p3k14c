library(tidyverse)

preScrub <- read_csv("4china-postDup.csv") 
postScrub <- read_csv("5china-postscrub.csv") %>%
  dplyr::mutate(Country = as.factor(Country),
                survived = "TRUE") %>% 
  dplyr::filter(
    Continent == "Asia"
  ) 



combo <- preScrub %>% 
  left_join(postScrub, by = c("LabID", "Age", "Sd", "Material", 
            "Taxa", "Long", "Lat", "LocAccuracy", "Period", 
            "SiteID", "SiteName", "Country", "Province", "Region",
            "Continent")) %>% 
  dplyr::filter(is.na(survived))






