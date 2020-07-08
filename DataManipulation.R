library(tidyverse)

RC <- read_csv(
  here::here("data/raw_data/radiocarbon_scrubbed_v3_3_KDE.csv")
)

bounds <- read.csv(
  here::here("data/raw_data/Qual_BoundingBoxes.csv")
)


RCcount_sites <- RC %>% 
  group_by(Long, Lat, SiteID, SiteName, Country, Province, Region) %>%
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nDates = n) %>% 
  select(-n)

RCcount_province <- RC %>% 
  group_by(Country, Province,Region) %>%
  dplyr::count() %>% 
  ungroup() %>% 
  filter(!is.na(Province))%>% 
  mutate(nDates = n) %>% 
  select(-n)

RCcount_region <- RC %>% 
  group_by(Country, Region) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nDates = n) %>% 
  select(-n)

RCcount_Country <- RC %>% 
  group_by(Country, Region) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nDates = n) %>% 
  select(-n)


#### USA check ----

USA_states <- read.csv(
  here::here("data/raw_data/Qual_SiteCounts/USASiteCounts.csv")
) %>% 
  mutate(Province = State) %>% 
  select(
    - ï..ST
  )

USA_dates <- RCcount_province %>% 
  filter(Country == "USA") %>% 
  mutate(
    Country = as.factor(Country),
    Province = as.factor(Province)
    , Region = as.factor(Region)
  ) %>% 
  left_join(
    USA_states
  ) %>% 
  mutate(
    
  )


ggplot(data=USA_dates, 
       aes(
         x= nSites,
         y=nDates
       ))+
  geom_point()

ggplot(data=USA_dates, 
       aes(
         x= percFed,
         y= nDates
       ))+
  geom_point()

ggplot(data=USA_dates, 
       aes(
         x= Area_km2,
         y= nDates
       ))+
  geom_point()

ggplot(data=USA_dates, 
       aes(
         x= nSites / Area_km2,
         y= nDates/ Area_km2
       ))+
  geom_point()



m <- lm((nDates/ Area_km2)~(nSites / Area_km2), USA_dates)
summary(m)

ggplot(data=USA_dates,
       aes(
         x=Region,
         y=nDates
       )
       )+
  geom_boxplot()+
  theme(
    axis.text.x = element_text(angle = 90)
  )


m <- lm(nDates~nSites, USA_dates)
summary(m)
