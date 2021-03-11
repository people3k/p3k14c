# install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
library(ggpubr)
library(smatr)
library(sf)

options(scipen=999)

RC <- read_csv(
  here::here("data/raw_data/scrubbed_5_1_joined.csv"),
  col_types = "cddccdfddfcccfffcccc"
)

#### USA check ------
bounds <- read.csv(
  here::here("data/raw_data/Qual_BoundingBoxes.csv")
) %>% 
  mutate(Region = as.factor(`ï..Region`)) %>%
  # select(-`ï..Region`) %>%
  select(Region, Country, Province) %>% 
  separate_rows(Province, sep = ", ") %>%
  filter(Region %in% c(
    "Whole Western US",
    "Plains US" ,
    "Desert West US",
    "West Coast US")) %>% 
  mutate(Country = replace (Country, Country ==  "United States", "USA"))


USA_states <- read.csv(
  here::here("data/raw_data/Qual_SiteCounts/USASiteCounts.csv")
) %>% 
  mutate(Province = State) 

USA_dates <- RC %>% 
  mutate(Province = as.factor(Province)) %>% 
  group_by(Province, Country) %>%
  dplyr::count() %>% 
  ungroup() %>% 
  filter(!is.na(Province))%>%
  mutate(nDates = n) %>% 
  select(-n) %>% 
  dplyr::filter(Country == "USA") %>% 
  dplyr::mutate(
    # Country = as.factor(Country),
    Province = as.factor(Province)
    # , Region = as.factor(Region)
  ) %>% 
  dplyr::left_join(
    USA_states
  ) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / Area_km2,
    nDatesPerKM2 = nDates / Area_km2
  ) %>% 
  dplyr::filter(!is.na(nSites))

GGpercFed_ndates <- ggplot(data=USA_dates, 
                           aes(
                             x= percFed,
                             y= nDatesPerKM2
                           ))+
  geom_point()+
  labs(x= c("% Federal Land"), y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("U.S.A. States")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))

SMAResults <- sma((nDatesPerKM2)~(nSitesPerKM2), USA_dates)
summary(SMAResults)
smaSummary <- coef(SMAResults) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResults$pval))



GGnSites_nDates_km <-
  ggplot()+
  geom_point(data=USA_dates, 
             aes(
               x= nSitesPerKM2,
               y= nDatesPerKM2
             )) +  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("U.S.A. States")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummary , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummary,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red") +
  geom_text(data = smaSummary,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")

# geom_text(data=Periods, # Put Period labels
#           aes(x=((EndDate+StartDate)/2), #center their X value between start and end date
#               label = Name, 
#               y = Inf) , # Puts them at top of the graph
#           vjust = -0.4)+  # Puts them just above the graph instead of inside.


SMAResults2 <- sma((nDates)~(nSites), USA_dates)
summary(SMAResults2)
smaSummary2 <- coef(SMAResults2) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResults2$pval))



GGnSites_nDates_noKM <- ggplot(data=USA_dates, 
                               aes(
                                 x= nSites,
                                 y= nDates
                               ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("U.S.A. States")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummary2 , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummary2,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red")+
  geom_text(data = smaSummary2,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")

#### US with guessed counties ========
RCcount_county <- RC %>% 
  filter(Country == "USA") %>%
  select(SiteName, SiteID, Country, guessed_province, guessed_subprovince) %>% 
  # distinct(SiteName, SiteID, Country, Province, guessed_subprovince) %>% 
  group_by(Country, guessed_province, guessed_subprovince) %>% 
  dplyr::count() %>% 
  ungroup()%>% 
  rename(nDates = n) 

Counties <- read_csv(
  here::here("data/raw_data/USCounties.csv")
) %>% 
  dplyr::rename(Area_km2 = LandKM2 ,
                guessed_subprovince = NAME,
                guessed_province = STATE)


RC_county <- RC %>% 
  filter(Country == "USA") %>%
  select(SiteName, SiteID, Country, guessed_province, guessed_subprovince) %>% 
  distinct(SiteName, SiteID, Country, guessed_province, guessed_subprovince) %>% 
  group_by(Country, guessed_province, guessed_subprovince) %>% 
  dplyr::count() %>% 
  ungroup() %>% 
  rename(nSites = n) %>% 
  left_join(RCcount_county) %>% 
  left_join(bounds) %>% 
  left_join(Counties, by = ) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / Area_km2,
    nDatesPerKM2 = nDates / Area_km2
  )

SMAResults3 <- sma((nDates)~(nSites), RC_county)
summary(SMAResults3)
smaSummary3 <- coef(SMAResults3) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResults3$pval))


GGUSAbyCounty <- ggplot(data=RC_county, 
                        aes( x= nSites,
                             y= nDates))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites"), 
       y= expression("Count 14C Dates"))+
  ggtitle("U.S.A. States by county")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummary3 , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummary3,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red")+
  geom_text(data = smaSummary3,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")




SMAResults4 <- sma((nDatesPerKM2)~(nSitesPerKM2), RC_county)
summary(SMAResults4)
smaSummary4 <- coef(SMAResults4) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResults4$pval))


GGUSAbyCountyPerKM2 <- 
  ggplot(data=RC_county, 
         aes( x= nSitesPerKM2,
              y= nDatesPerKM2))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggrepel::geom_text_repel(aes(label = ifelse(nSitesPerKM2 > 0.1,
                                              paste(guessed_subprovince,", ", AbbrevState,
                                                    sep = ""),
                                              NA ))) +
  scale_y_continuous(limits = c(0, 1.26)) +
  ggtitle("U.S.A. States by county")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14)) +
  geom_abline(data = smaSummary4 , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummary4,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red")+
  geom_text(data = smaSummary4,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")



SMAResults5 <- sma((nDatesPerKM2)~(nSitesPerKM2), RC_county%>%
                     dplyr::filter(nSitesPerKM2 < 0.1))
summary(SMAResults5)
smaSummary5 <- coef(SMAResults5) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResults5$pval))

GGUSAbyCountyPerKM2_smol <- 
  ggplot(data=RC_county %>%
           dplyr::filter(nSitesPerKM2 < 0.1), 
         aes( x= nSitesPerKM2,
              y= nDatesPerKM2))+
  geom_point() +  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  # scale_y_continuous(limits = c(0, 1.26)) +
  ggtitle("U.S.A. States by county")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14)) +
  geom_abline(data = smaSummary5 , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummary5,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red")+
  geom_text(data = smaSummary5,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")



### patchwork =======


ppi <-  180

png("linearQualAnalysis2.png", width=6.5*ppi, height=11*ppi, res=ppi)
ggarrange(GGnSites_nDates_noKM,
          GGnSites_nDates_km,
          GGUSAbyCounty,
          GGUSAbyCountyPerKM2,
          GGpercFed_ndates ,
          GGUSAbyCountyPerKM2_smol, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)
dev.off()



#### Global -----

bounds <- read.csv(
  here::here("data/raw_data/Qual_BoundingBoxes.csv")
) %>% 
  filter(Country != "United States") %>% 
  mutate(Regional = as.factor(ï..Region)) %>%
  select(-`ï..Region`) %>%
  # select(Regional, Country,  Province) %>% 
  select(-Province) %>%
  mutate(Regional = as.factor(Regional))

countries <- sf::read_sf(
  here::here("data/raw_data/Qual_SiteCounts/ne_10m_admin_0_countries.shp")
) %>% 
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0") %>% 
  dplyr::mutate(area_m = (geometry %>% st_area())) %>% 
  dplyr::arrange(- area_m) %>% 
  as.data.frame() %>% 
  dplyr::group_by(ADMIN) %>% 
  dplyr::summarize(area_sqm = sum(area_m)) %>% 
  dplyr::mutate(area_sqkm = as.numeric(area_sqm * 1e-6 )) %>% 
  dplyr::rename(
    Country = ADMIN
  ) %>% 
  dplyr::select(Country, area_sqkm)

provinces <- sf::read_sf(
  here::here("data/raw_data/Qual_SiteCounts/ne_10m_admin_1_states_provinces_scale_rank.shp")
) %>% 
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=0") %>% 
  dplyr::mutate(area_m = (geometry %>% st_area())) %>% 
  dplyr::arrange(- area_m) %>% 
  as.data.frame() %>% 
  dplyr::group_by(admin, name) %>% 
  dplyr::summarize(area_sqm = sum(area_m)) %>% 
  dplyr::mutate(area_sqkm = as.numeric(area_sqm * 1e-6 )) %>% 
  dplyr::rename(
    Country = admin,
    Province = name
  ) %>% 
  dplyr::select(Country, Province, area_sqkm)

#### China ========
ChinaSites <- read_csv(
  here::here("data/raw_data/Qual_SiteCounts/china.csv")
) %>% 
  dplyr::mutate(Province = as.factor(Volume),
                Country = country) %>% 
  dplyr::group_by(Province, Country) %>% 
  dplyr::count() %>% 
  dplyr::ungroup()%>% 
  dplyr::mutate(nSites = n) %>% 
  dplyr::select(-n) %>% 
  dplyr::mutate(Province = Province %>% 
                  stringr::str_replace_all(" Autonomous Region", "") %>% 
                  stringr::str_replace_all(" Hui", ""  ) %>% 
                  stringr::str_replace_all(" Uyghur", ""))



China <- RC %>% 
  dplyr::filter(Country == "China") %>% 
  dplyr::mutate(Province = guessed_province) %>% 
  dplyr::group_by(Province, Country) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(Province))%>%
  dplyr::rename(nDates = n) %>% 
  dplyr::mutate(Province = Province %>% 
                  stringr::str_replace_all("Xizang", "Tibet")) %>% 
  dplyr::inner_join(ChinaSites) %>% 
  left_join(
    provinces %>% 
      dplyr::filter(Country == "China") %>% 
      dplyr::mutate(Province = Province %>% 
                      stringr::str_replace("Nei Mongol", "Inner Mongolia") %>% 
                      stringr::str_replace("Ningxia Hui", "Ningxia") %>% 
                      stringr::str_replace("Xinjiang Uygur", "Xinjiang") %>% 
                      stringr::str_replace("Xizang", "Tibet"))
  ) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / area_sqkm,
    nDatesPerKM2 = nDates / area_sqkm
  )

SMAResultsChina <- sma((nDatesPerKM2)~(nSitesPerKM2), China)
summary(SMAResultsChina)
smaSummaryChina <- coef(SMAResultsChina) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResultsChina$pval))

GGChina <- ggplot(data=China, 
                  aes(
                    x= nSitesPerKM2,
                    y= nDatesPerKM2
                  )) +
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("China Provinces")+
  ggrepel::geom_text_repel(aes(label = ifelse(nDatesPerKM2 > 0.003  , Province, NA )),  hjust=-0.1, vjust=0.4)+
  # scale_x_continuous(limits = c(0,8000))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummaryChina , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummaryChina,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 4,
            color = "red")+
  geom_text(data = smaSummaryChina,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 6,
            color = "red")
GGChina

#### West Europe ========
WEurSites <- RC %>% 
  dplyr::filter(Country %in% c(
    "France",
    "Netherlands", 
    "Germany",
    "Switzerland",
    "Belgium",
    "Spain",
    "Portugal",
    "United Kingdom",
    "Ireland"
  )) %>% 
  dplyr::select(SiteName, SiteID, Country, guessed_province) %>% 
  dplyr::distinct(SiteName, SiteID, Country, guessed_province)  %>% 
  dplyr::group_by(Country) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(nSites = n) %>% 
  dplyr::select(-n)

WEur <-  RC %>% 
  group_by(Country) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nDates = n) %>% 
  select(-n) %>% 
  right_join(WEurSites) %>% 
  filter(!is.na(nSites)) %>% 
  left_join(
    countries
  ) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / area_sqkm,
    nDatesPerKM2 = nDates / area_sqkm
  )


SMAResultsWEur <- sma((nDatesPerKM2)~(nSitesPerKM2), WEur)
summary(SMAResultsWEur)
smaSummaryWEur <- coef(SMAResultsWEur) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResultsWEur$pval))

GGWEur <- ggplot(data=WEur, 
                 aes(
                   x= nSitesPerKM2,
                   y= nDatesPerKM2
                 ))+
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("Western Europe Countries")+
  ggrepel::geom_text_repel(aes(label = ifelse(nSitesPerKM2 > 0.02,Country, NA)),  hjust=-0.1, vjust=0.4)+
  # scale_x_continuous(limits = c(0, 5500))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummaryWEur , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummaryWEur,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red")+
  geom_text(data = smaSummaryWEur,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 5)))),
            hjust = 0,
            vjust = 4,
            color = "red")
GGWEur


#### West Africa ========

WAfrSites <- read_csv(
  here::here("data/raw_data/Qual_SiteCounts/Kay_WestAfrica_Sites.csv")
) %>% 
  filter(
    Country %in% c(
      "Benin",
      "BurkinaFaso",
      "Cameroon",
      "Chad",
      "Congo",
      "DRC",
      "Equat.Guinea",
      "Gabon",
      "Ghana",
      "Guinea",
      "IvoryCoast",
      "Liberia",
      "Mali",
      "Mauritania",
      "Niger",
      "Nigeria",
      "Togo"
    )
  ) %>% 
  group_by(Country) %>% 
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nSites = n) %>% 
  select(-n)

WAfr <- RC %>% 
  group_by(Country) %>%
  dplyr::count() %>% 
  ungroup()%>% 
  mutate(nDates = n) %>% 
  select(-n) %>% 
  right_join(WAfrSites)%>% 
  left_join(
    countries %>% 
      dplyr::mutate(
        Country = Country %>% 
          stringr::str_replace("Congo" ,"Republic of the Congo") %>% 
          stringr::str_replace("IvoryCoast", "Ivory Coast") %>% 
          stringr::str_replace("BurkinaFaso", "Burkina Faso") %>% 
          stringr::str_replace("DRC" , "Democratic Republic of the Congo") %>% 
          stringr::str_replace("Equat.Guinea" , "Equatorial Guinea"))) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / area_sqkm,
    nDatesPerKM2 = nDates / area_sqkm
  )



SMAResultsWAfr <- sma((nDatesPerKM2)~(nSitesPerKM2), WAfr)
summary(SMAResultsWAfr)
smaSummaryWAfr <- coef(SMAResultsWAfr) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResultsWAfr$pval))

GGWAfr <- ggplot(data=WAfr, 
                 aes(
                   x= nSitesPerKM2,
                   y= nDatesPerKM2
                 )) +
  geom_point()+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2),
       subtitle = " (10S-15N; 17W-20E)")+
  ggtitle("Western Africa Countries")+
  geom_text(aes(label = ifelse(nDatesPerKM2 > 0.00075,Country, NA)),  hjust=1.1, vjust=0.4)+
  # scale_x_continuous(limits = c(0, 370))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14))+
  geom_abline(data = smaSummaryWAfr , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummaryWAfr,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red") +
  geom_text(data = smaSummaryWAfr,
            aes(x = 0, y = Inf,
                label = paste("p =", 
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 6)))),
            hjust = 0,
            vjust = 4,
            color = "red")
GGWAfr

#### Western SA =========
WSABounds <- as.list(bounds[1,])

WSASites <- RC %>% 
  dplyr::filter(Country %in% c("Chile", "Peru", "Bolivia", "Argentina")) %>% 
  # dplyr::filter(
  #   Lat >= WSABounds$Lat_min & Lat <= WSABounds$Lat_max,
  #   Long >= WSABounds$Long_min & Long <= WSABounds$Long_max
  # )  %>% 
  dplyr::select(SiteName, SiteID, Country, guessed_province) %>% 
  dplyr::distinct(SiteName, SiteID, Country, guessed_province)  %>% 
  dplyr::group_by(Country, guessed_province) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(nSites = n) %>% 
  dplyr::select(-n)

WSA <- RC %>% 
  dplyr::filter(Country %in% c("Chile", "Peru", "Bolivia", "Argentina")) %>% 
  # dplyr::filter(
  #   Lat >= WSABounds$Lat_min & Lat <= WSABounds$Lat_max,
  #   Long >= WSABounds$Long_min & Long <= WSABounds$Long_max
  # )  %>% 
  dplyr::group_by(Country, guessed_province) %>%
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(nDates = n) %>% 
  dplyr::select(-n) %>% 
  dplyr::left_join(WSASites) %>% 
  dplyr::mutate(Country = as.factor(Country)) %>% 
  left_join(
    provinces %>% dplyr::rename("guessed_province" = "Province")
  ) %>% 
  dplyr::mutate(
    nSitesPerKM2 = nSites / area_sqkm,
    nDatesPerKM2 = nDates / area_sqkm
  )


SMAResultsWSA <- sma((nDatesPerKM2)~(nSitesPerKM2), WSA)
summary(SMAResultsWSA)
smaSummaryWSA <- coef(SMAResultsWSA) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(pval = as.numeric(SMAResultsWSA$pval))


GGWSA <- ggplot(data=WSA, 
                aes(
                  x= nSitesPerKM2,
                  y= nDatesPerKM2
                ))+
  geom_point(aes(shape = Country))+  
  labs(x= expression("Count Arch. Sites per"~KM^2), 
       y= expression("Count 14C Dates per"~KM^2))+
  ggtitle("SW South American Provinces")+
  # geom_text(aes(label = Country),  hjust=-0.1, vjust=0.4)+
  # scale_x_continuous(limits = c(0, 30))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.position = c(0.75, 0.2),
    legend.title = element_blank())+
  geom_abline(data = smaSummaryWSA , 
              aes(intercept = elevation, 
                  slope = slope),
              color = "red") +
  geom_text(data = smaSummaryWSA,
            aes(x = 0, y = Inf,
                label = paste("Y = ", round(slope,2), "x +", round(elevation,2))),
            hjust = 0,
            vjust = 2,
            color = "red") +
  geom_text(data = smaSummaryWSA,
            aes(x = 0, y = Inf,
                label = paste("p =",
                              ifelse(pval <0.00001, "< 0.00001",
                                     round(pval, 6)))),
            hjust = 0,
            vjust = 4,
            color = "red")
GGWSA 


##### Global patchwork =======
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
