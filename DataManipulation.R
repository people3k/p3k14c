# install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
library(ggpubr)
# library(readr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)

RC <- read_csv(
  here::here("data/raw_data/radiocarbon_scrubbed_v4_1_1.csv")
)

bounds <- read.csv(
  here::here("data/raw_data/Qual_BoundingBoxes.csv")
) %>% 
  mutate(Regional = as.factor(ï..Region)) %>%
  select(-`ï..Region`) %>%
  select(Regional, Country, Province) %>% 
  separate_rows(Province, sep = ", ") %>%
  filter(Regional %in% c(
    "Whole Western US",
    "Plains US" ,
    "Desert West US",
    "West Coast US")) %>% 
  mutate(Country = replace (Country, Country ==  "United States", "USA"))
  # mutate(Region = str_replace(Region, " ", ""),
  #        Province = str_replace(Province, " ", ""))
  # pivot_longer(cols = c(Region), names_from = Province)


RCcount_sites <- RC %>% 
  group_by(Long, Lat, SiteID, SiteName, Country, Province, Region) %>%
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nDates = n) %>% 
  select(-n)

RCcount_province <- RC %>% 
  group_by(Country, Province) %>%
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
# select(- ST)

USA_dates <- RCcount_province %>% 
  filter(Country == "USA") %>% 
  mutate(
    Country = as.factor(Country),
    Province = as.factor(Province)
    # , Region = as.factor(Region)
  ) %>% 
  left_join(
    USA_states
  ) %>% 
  mutate(
    nSitesPerKM2 = nSites / Area_km2,
    nDatesPerKM2 = nDates / Area_km2
  )



GGpercFed_ndates <- ggplot(data=USA_dates, 
       aes(
         x= percFed,
         y= nDatesPerKM2
       ))+
  geom_point()+
  labs(x= c("% Federal Land"), y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("U.S.A. States")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))


GGnSites_nDates <- ggplot(data=USA_dates, 
       aes(
         x= nSitesPerKM2,
         y= nDatesPerKM2
       ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("U.S.A. States")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))

m <- lm((nDatesPerKM2)~(nSitesPerKM2), USA_dates)
summary(m)


ggplot(data=USA_dates,
       aes(
         x=Region,
         y=nDates
       ))+
  geom_boxplot()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90)
  )


##### Western US -----
Bounds2 <- bounds %>% 
  left_join(USA_dates)

GGWest <- ggplot(data=Bounds2 %>% 
         filter(Regional == "Whole Western US"), 
       aes(
         x= nSitesPerKM2,
         y= nDatesPerKM2
       ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("Western U.S.A. States")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))


m <- lm(nDatesPerKM2~nSitesPerKM2, Bounds2 %>% 
          filter(Regional == "Whole Western US"))
summary(m)

#### US with guessed counties
RCcount_county <- RC %>% 
  filter(Country == "USA") %>%
  group_by(Country, Province,Region, Guessed_subprovince) %>% 
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nDates = n) %>% 
  select(-n)

RC_county2 <- RC %>% 
  filter(Country == "USA") %>%
  select(SiteName, SiteID, Country, Province, Guessed_subprovince) %>% 
  distinct(SiteName, SiteID, Country, Province, Guessed_subprovince) %>% 
  group_by(Country, Province, Guessed_subprovince) %>% 
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nSites = n) %>% 
  select(-n) %>% 
  left_join(RCcount_county) %>% 
  left_join(bounds)

GGWestbyCounty <- ggplot(data=RC_county2 %>% 
         filter(Regional       
                 == 
                   "Whole Western US"
                  # "Desert West US"
                  # "Plains US"
                  # "West Coast US"
                ), 
       aes(
         x= nSites,
         y= nDates
       ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("W. U.S.A. States by county")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))

m <- lm(nDates~nSites,RC_county2 %>% 
          filter(Regional == "Whole Western US"))
summary(m)






RC_county4 <- RC %>% 
  filter(Country == "USA") %>%
  select(SiteName, SiteID, Country, Province, Guessed_subprovince) %>% 
  distinct(SiteName, SiteID, Country, Province, Guessed_subprovince) %>% 
  group_by(Country, Province, Guessed_subprovince) %>% 
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nSites = n) %>% 
  select(-n) %>% 
  left_join(RCcount_county) 

GGUSbyCounty <- ggplot(data=RC_county4 , 
                         aes(
                           x= nSites,
                           y= nDates
                         ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("U.S.A. States by county")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))


GGEastbyCounty <- ggplot(data=RC_county4 %>% 
                           filter(! Province       
                                  %in% c(
                                    "California",
                                    "Oregon",
                                    "Washington",
                                    "Arizona",
                                    "New Mexico",
                                    "Utah",
                                    "Colorado",
                                    "Idaho",
                                    "Montana",
                                    "Wyoming",
                                    "Nevada",
                                    "North Dakota",
                                    "South Dakota",
                                    "Kansas",
                                    "Nebraska",
                                    "Oklahoma",
                                    "Texas"
                                  )), 
                         aes(
                           x= nSites,
                           y= nDates
                         ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("E. U.S.A. States by county")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))

### patchwork


ppi <-  180

png("linearQualAnalysis.png", width=6.5*ppi, height=8*ppi, res=ppi)
ggarrange(GGpercFed_ndates , 
          GGnSites_nDates,
          GGWestbyCounty, 
          GGEastbyCounty,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
dev.off()



#### Global by data -----

RC_county3 <- RC_county2 %>% 
  filter(is.na(Regional),
         !is.na(Guessed_subprovince),
         is.na(as.numeric(Guessed_subprovince)))

ggplot(data=RC_county3, 
       aes(
         x= nSites,
         y= nDates
       ))+
  geom_point()+  
  labs(x= expression("Arch. Sites Count"), 
       y= expression("14C Dates Count"))+
  ggtitle("Global by county")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 18))
