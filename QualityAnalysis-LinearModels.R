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

#### US with guessed counties ----
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

### patchwork ----


ppi <-  180

png("linearQualAnalysis.png", width=6.5*ppi, height=8*ppi, res=ppi)
ggarrange(GGpercFed_ndates , 
          GGnSites_nDates,
          GGWestbyCounty, 
          GGEastbyCounty,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
dev.off()



#### Global -----

bounds <- read.csv(
  here::here("data/raw_data/Qual_BoundingBoxes.csv")
) %>% 
  # mutate(Regional = as.factor(ï..Region)) %>%
  # select(-`ï..Region`) %>%
  # select(Regional, Country,  Provi nce) %>% 
  separate_rows(Country, sep = ", ") %>%
  filter(Country != "United States") %>% 
  select(-Province) %>%
  mutate(Regional = as.factor(Region))

#### China -----
ChinaSites <- read_csv(
  here::here("data/raw_data/Qual_SiteCounts/china.csv")
) %>% 
  mutate(Province = as.factor(Volume),
         Country = country) %>% 
  group_by(Province, Country) %>% 
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nSites = n) %>% 
  select(-n)
  
China <- RCcount_province %>% 
  filter(Country == "China") %>% 
  left_join(ChinaSites)

GGChina <- ggplot(data=China, 
                         aes(
                           x= nSites,
                           y= nDates
                         ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("China Provinces")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))
GGChina

#### West Europe ----
WEurSites <- read_csv(
  here::here("data/raw_data/Qual_SiteCounts/nw_europe.csv")
) %>% 
  mutate(Country = country) %>% 
  group_by(Country) %>% 
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nSites = n) %>% 
  select(-n)

WEur <- RCcount_Country %>% 
  left_join(WEurSites) %>% 
  filter(!is.na(nSites))

GGWEur <- ggplot(data=WEur, 
                  aes(
                    x= nSites,
                    y= nDates
                  ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("Western Europe Countries")+
  geom_text(aes(label = Country),  hjust=-0.1, vjust=0.4)+
  scale_x_continuous(limits = c(0, 5000))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))
GGWEur

m <- lm(nDates~nSites, data= WEur)
summary(m)

#### West Africa ----

WAfrBounds <- as.list(bounds[2,])

WAfrSites <- read_csv(
  here::here("data/raw_data/Qual_SiteCounts/Kay_WestAfrica_Sites.csv")
) %>% 
  filter(
    X >= WAfrBounds$Lat_min & X <= WAfrBounds$Lat_max,
    Y >= WAfrBounds$Long_min & Y <= WAfrBounds$Long_max
  ) %>% 
  group_by(Country) %>% 
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nSites = n) %>% 
  select(-n)

WAfr <- RC %>% 
  filter(
    Lat >= WAfrBounds$Lat_min & Lat <= WAfrBounds$Lat_max,
    Long >= WAfrBounds$Long_min & Long <= WAfrBounds$Long_max
  )  %>% 
  group_by(Country, Region) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nDates = n) %>% 
  select(-n) %>% 
  left_join(WAfrSites)

GGWAfr <- ggplot(data=WAfr, 
                 aes(
                   x= nSites,
                   y= nDates
                 ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("Western Africa Countries (Lat -10 to 15; Long -17 to 20)")+
  # geom_text(aes(label = Country),  hjust=-0.1, vjust=0.4)+
  scale_x_continuous(limits = c(0, 370))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))
GGWAfr

m <- lm(nDates~nSites, data= China)
summary(m)

#### Western SA ----
WSABounds <- as.list(bounds[1,])

WSASites <- RC %>% 
  filter(
    Lat >= WSABounds$Lat_min & Lat <= WSABounds$Lat_max,
    Long >= WSABounds$Long_min & Long <= WSABounds$Long_max
  )  %>% 
  select(SiteName, SiteID, Country, Guessed_province) %>% 
  distinct(SiteName, SiteID, Country)  %>% 
  group_by(Country) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nSites = n) %>% 
  select(-n)

WSA <- RC %>% 
  filter(
    Lat >= WSABounds$Lat_min & Lat <= WSABounds$Lat_max,
    Long >= WSABounds$Long_min & Long <= WSABounds$Long_max
  )  %>% 
  group_by(Country) %>%
  dplyr::count() %>% 
  ungroup() %>% 
  mutate(nDates = n) %>% 
  select(-n) %>% 
  left_join(WSASites)


GGWSA <- ggplot(data=WSA, 
                 aes(
                   x= nSites,
                   y= nDates
                 ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("Western S. American Countries (Lat -30 to -23; Long -70 to -57)")+
  geom_text(aes(label = Country),  hjust=-0.1, vjust=0.4)+
  scale_x_continuous(limits = c(0, 30))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))
GGWSA 

m <- lm(nDates~nSites, data= WAfr)
summary(m)

##### Global patchwork ----
ppi <-  180

png("globallinearQualAnalysis.png", width=6.5*ppi, height=8*ppi, res=ppi)
ggarrange(GGChina , 
          GGWAfr,
          GGWEur, 
          GGWSA,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
dev.off()




#### BAD GLOBAL COUNTIES ----
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
