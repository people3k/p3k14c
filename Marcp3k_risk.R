####sensitivity (including kde) analysis fo SciPaper####
# install.packages("devtools")
# install.packages("purrr")
c("readr",
  "tibble",
  "tidyr",
  "dplyr",
  "sf",
  "sparr",
  "spatstat") %T>%
  purrr::walk(devtools::install_cran,
              quiet = TRUE) %>%
  purrr::walk(library, 
              character.only = TRUE)


#data
scipap <-
  "radiocarbon_dates_scrubbedv5_1.csv" %>%
  readr::read_csv() %>%
  tidyr::drop_na(Long, Lat) %>%
  sf::st_as_sf(coords = c("Long","Lat"),
               crs = 4326) %>%
  dplyr::mutate(Region = NA,
                Region = ifelse(Continent == "Europe",
                                "Europe",
                                Region),
                Region = ifelse(Country == "USA",
                                "United States", 
                                Region),
                Region = ifelse(Country == "China",
                                "China", 
                                Region),
                Region = ifelse(Long >= -17 & 
                                  Long <= 20 & 
                                  Lat >= -10 & 
                                  Lat <= 15,
                                "Western Africa",
                                Region),
                Region = ifelse(Long >= -70 & 
                                  Long <= -57 & 
                                  Lat >= -30 & 
                                  Lat <= -23,
                                "Southwestern USA",
                                Region)
  )

swus <- 
  c(xmin = -70,
    xmax = -57,
    ymin = -30,
    ymax = -23) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc(crs = 4326)

wa <- 
  c(xmin = -17,
    xmax = 20,
    ymin = -10,
    ymax = 15) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc(crs = 4326)

#count sites
scipap_counts <-
  scipap %>%
  dplyr::group_by(Region) %>%
  dplyr::count()


#data
scipap<-read.csv("/home/marc/Dropbox/m_pers/people3k/20210224/radiocarbon_dates_scrubbedv5_1.csv",header=TRUE)
scipap<-drop_na(scipap,Long,Lat)
#subset for various windows
#scipap.china<-filter(scipap,Country=="China")
#scipap.wa<-filter(scipap,Long>=-17&Long<=20&Lat>=-10&Lat<=15)
scipap.euro<-filter(scipap,Continent=="Europe")
#scipap.swa<-filter(scipap,Long>=-70&Long<=-57&Lat>=-30&Lat<=-23)
scipap.usa<-filter(scipap,Country=="USA")
#count sites
#scipap.china.c<-count(scipap.china,SiteName)
#scipap.china.c<-left_join(scipap.china.c,scipap.china,by="SiteName")
#scipap.china.c<-distinct(scipap.china.c,SiteName,.keep_all=TRUE)
#scipap.wa.c<-count(scipap.wa,SiteName)
#scipap.wa.c<-left_join(scipap.wa.c,scipap.wa,by="SiteName")
#scipap.wa.c<-distinct(scipap.wa.c,SiteName,.keep_all=TRUE)
scipap.euro.c<-count(scipap.euro,SiteName)
scipap.euro.c<-left_join(scipap.euro.c,scipap.euro,by="SiteName")
scipap.euro.c<-distinct(scipap.euro.c,SiteName,.keep_all=TRUE)
#scipap.swa.c<-count(scipap.swa,SiteName)
#scipap.swa.c<-left_join(scipap.swa.c,scipap.swa,by="SiteName")
#scipap.swa.c<-distinct(scipap.swa.c,SiteName,.keep_all=TRUE)
scipap.usa.c<-count(scipap.usa,SiteID)
scipap.usa.c<-left_join(scipap.usa.c,scipap.usa,by="SiteID")
scipap.usa.c<-distinct(scipap.usa.c,SiteID,.keep_all=TRUE)
#spatial data
wgs84<-"+proj=longlat +datum=WGS84 +no_defs"
equal_earth<-"+proj=eqearth +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
world<-ne_download(scale=110,type="land",category="physical",returnclass = "sf")
world.equal<-st_transform(world,crs=equal_earth)
countries<-ne_download(scale=50,type="countries",category="cultural", returnclass = "sf")

europe<-("/home/marc/Dropbox/m_pers/people3k/quality/europe.shp") #load data
europe<-shapefile(europe)
europe<-st_as_sf(europe)
europe<-st_transform(europe,equal_earth)

nw.europe<-countries[countries$ADMIN=="Netherlands"|countries$ADMIN=="Belgium"|countries$ADMIN=="Denmark"|countries$ADMIN=="France"|
                       countries$ADMIN=="Germany"|countries$ADMIN=="Luxembourg",]
nw.europe<-st_transform(nw.europe,equal_earth)
nw.europe<-st_intersection(europe,nw.europe)
mask.nw.europe<-as.owin(nw.europe)
# mask usa
usa<-ne_states(country="United States of America",returnclass = "sf")
#usa<-usa[usa$name!="Alaska"&usa$name!="Hawaii",]
#usa<-st_transform(usa,equal_earth)
#mask.usa<-as.owin(usa)
#mask desert west US
dw.usa<-usa[usa$name=="Idaho"|usa$name=="Colorado"|usa$name=="Utah"|usa$name=="Arizona"|usa$name=="New Mexico"|usa$name=="Wyoming"|
              usa$name=="Nevada",]
dw.usa<-st_transform(dw.usa,equal_earth)
mask.dw.usa<-as.owin(dw.usa)
#reproject point data and prepare individual ppp for KDE analyses 
#china
#p.scipap.china<-st_as_sf(scipap.china.c,coords=c("Long","Lat"),crs=wgs84)
#china.eqe<-st_transform(p.scipap.china,equal_earth)
#china.eqe<-as(china.eqe,"Spatial")
#china.eqe<-as(china.eqe,"ppp")
#china.ppp<-ppp(china.eqe$x,china.eqe$y,window=mask.china,marks=china.eqe$marks$n)
# europe
p.scipap.euro<-st_as_sf(scipap.euro.c,coords=c("Long","Lat"),crs=wgs84)
euro.eqe<-st_transform(p.scipap.euro,equal_earth)
euro.eqe<-as(euro.eqe,"Spatial")
euro.eqe<-as(euro.eqe,"ppp")
#north-west europe
nw.euro.ppp<-ppp(euro.eqe$x,euro.eqe$y,window=mask.nw.europe,marks=euro.eqe$marks$n)
#euro.ppp<-ppp(euro.eqe$x,euro.eqe$y,window=mask.europe,marks=euro.eqe$marks$n)
# western africa
#p.scipap.wa<-st_as_sf(scipap.wa.c,coords=c("Long","Lat"),crs=wgs84)
#wa.eqe<-st_transform(p.scipap.wa,equal_earth)
#wa.eqe<-as(wa.eqe,"Spatial")
#wa.eqe<-as(wa.eqe,"ppp")
#wa.ppp<-ppp(wa.eqe$x,wa.eqe$y,window=mask.wa,marks=wa.eqe$marks$n)
#south-western america
#p.scipap.swa<-st_as_sf(scipap.swa.c,coords=c("Long","Lat"),crs=wgs84)
#swa.eqe<-st_transform(p.scipap.swa,equal_earth)
#swa.eqe<-as(swa.eqe,"Spatial")
#swa.eqe<-as(swa.eqe,"ppp")
#swa.ppp<-ppp(swa.eqe$x,swa.eqe$y,window=mask.swa,marks=swa.eqe$marks$n)
#usa
p.scipap.usa<-st_as_sf(scipap.usa.c,coords=c("Long","Lat"),crs=wgs84)
usa.eqe<-st_transform(p.scipap.usa,equal_earth)
usa.eqe<-as(usa.eqe,"Spatial")
usa.eqe<-as(usa.eqe,"ppp")
#usa.ppp<-ppp(usa.eqe$x,usa.eqe$y,window=mask.usa,marks=usa.eqe$marks$n)
#dw.usa
dw.usa.ppp<-ppp(usa.eqe$x,usa.eqe$y,window=mask.dw.usa,marks=usa.eqe$marks$n)
#risk surface analysis
#china
#h0.china<-OS(china.ppp,nstar="npoints")
#kde.dates.china<-bivariate.density(china.ppp,h0=h0.china,adapt=FALSE,edge="diggle")
#plot(kde.dates.china)
#kde.sites.china<-bivariate.density(china.ppp,h0=h0.china,adapt=FALSE,edge="diggle",weights=china.ppp$marks)
#plot(kde.sites.china)
#dates.sites.china<-risk(kde.dates.china,kde.sites.china,tolerate=TRUE)
#plot(dates.sites.china)
#western africa
#h0.wa<-OS(wa.ppp,nstar="npoints")
#kde.dates.wa<-bivariate.density(wa.ppp,h0=h0.wa,adapt=FALSE,edge="diggle")
#plot(kde.dates.wa)
#kde.sites.wa<-bivariate.density(wa.ppp,h0=h0.wa,adapt=FALSE,edge="diggle",weights=wa.ppp$marks)
#plot(kde.sites.wa)
#dates.sites.wa<-risk(kde.dates.wa,kde.sites.wa,tolerate=TRUE)
#plot(dates.sites.wa)
# south-western america
#h0.swa<-OS(swa.ppp,nstar="npoints")
#kde.dates.swa<-bivariate.density(swa.ppp,h0=h0.swa,adapt=FALSE,edge="diggle")
#plot(kde.dates.swa)
#kde.sites.swa<-bivariate.density(swa.ppp,h0=h0.swa,adapt=FALSE,edge="diggle",weights=swa.ppp$marks)
#plot(kde.sites.swa)
#dates.sites.swa<-risk(kde.dates.swa,kde.sites.swa,tolerate=TRUE)
#plot(dates.sites.swa)
# europe
#h0.euro<-OS(euro.ppp,nstar="npoints")
#kde.dates.euro<-bivariate.density(euro.ppp,h0=h0.euro,adapt=FALSE,edge="diggle")
#plot(kde.dates.euro)
#kde.sites.euro<-bivariate.density(euro.ppp,h0=h0.euro,adapt=FALSE,edge="diggle",weights=euro.ppp$marks)
#plot(kde.sites.euro)
#dates.sites.euro<-risk(kde.dates.euro,kde.sites.euro,tolerate=TRUE)
#plot(dates.sites.euro)
#north-western europe
h0.nw.euro<-OS(nw.euro.ppp,nstar="npoints")
kde.dates.nw.euro<-bivariate.density(nw.euro.ppp,h0=h0.nw.euro,adapt=FALSE,edge="diggle")
plot(kde.dates.nw.euro, main="Kernel Density Estimate",xlab="Easting",ylab="Northing")
plot(nw.europe,col=sf.colors(n=1,alpha=0),add=TRUE)
kde.sites.nw.euro<-bivariate.density(nw.euro.ppp,h0=h0.nw.euro,adapt=FALSE,edge="diggle",weights=nw.euro.ppp$marks)
plot(kde.sites.nw.euro,
     main=expression("Kernel Density estimate (weighed by number of " ^14* "C dates per sites)"),xlab="Easting",ylab="Northing")
plot(nw.europe,col=sf.colors(n=1,alpha=0),add=TRUE)
dates.sites.nw.euro<-risk(kde.dates.nw.euro,kde.sites.nw.euro,tolerate=TRUE)
plot(dates.sites.nw.euro,main="Risk surface analysis",xlab="Easting",ylab="Northing")
plot(nw.europe,col=sf.colors(n=1,alpha=0),add=TRUE)
#usa
#h0.usa<-OS(usa.ppp,nstar="npoints")
#usa.dates<-bivariate.density(usa.ppp,h0=h0.usa,adapt=FALSE,edge="diggle")
#plot(usa.dates)
#usa.sites<-bivariate.density(usa.ppp,h0=h0.usa,adapt=FALSE,edge="diggle",weights=usa.ppp$marks)
#plot(usa.sites)
#dates.sites.usa<-risk(usa.sites,usa.dates,tolerate=TRUE)
#plot(dates.sites.usa)
#desert west usa
h0.dw.usa<-OS(dw.usa.ppp,nstar="npoints")
dw.usa.dates<-bivariate.density(dw.usa.ppp,h0=h0.dw.usa,adapt=FALSE,edge="diggle")
plot(dw.usa.dates, main="Kernel Density Estimate",xlab="Easting",ylab="Northing")
plot(dw.usa,col=sf.colors(n=1,alpha=0),add=TRUE)
dw.usa.sites<-bivariate.density(dw.usa.ppp,h0=h0.dw.usa,adapt=FALSE,edge="diggle",weights=dw.usa.ppp$marks)
plot(dw.usa.sites,
     main=expression("Kernel Density Estimate (weighed by number of " ^14* "C dates per sites)"),xlab="Easting",ylab="Northing")
plot(dw.usa,col=sf.colors(n=1,alpha=0),add=TRUE)
dates.sites.dw.usa<-risk(dw.usa.sites,dw.usa.dates,tolerate=TRUE)
plot(dates.sites.dw.usa,main="Risk surface analysis",xlab="Easting",ylab="Northing")
plot(dw.usa,col=sf.colors(n=1,alpha=0),add=TRUE)
