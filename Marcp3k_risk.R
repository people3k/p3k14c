#### sensitivity (including kde) analysis fo SciPaper####
# install.packages("devtools")
# install.packages("purrr")
# install.packages("magrittr")
c(
  "readr",
  "tibble",
  "tidyr",
  "dplyr",
  "sf",
  "sparr",
  "spatstat",
  "magrittr"
) %T>%
  purrr::walk(devtools::install_cran,
    quiet = TRUE
  ) %>%
  purrr::walk(library,
    character.only = TRUE
  )


# data
scipap <-
  "data/raw_data/radiocarbon_dates_scrubbedv5_1.csv" %>%
  here::here() %>%
  readr::read_csv() %>%
  tidyr::drop_na(Long, Lat) %>%
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    crs = 4326
  ) %>%
  dplyr::mutate(
    Region = NA,
    Region = ifelse(Continent == "Europe",
      "Europe",
      Region
    ),
    Region = ifelse(Country == "USA",
      "United States",
      Region
    )
  )


# count sites
scipap_counts <-
  scipap %>%
  dplyr::group_by(Region) %>%
  dplyr::count()


# data
# scipap <- read.csv("/home/marc/Dropbox/m_pers/people3k/20210224/radiocarbon_dates_scrubbedv5_1.csv", header = TRUE)
# scipap <- drop_na(scipap, Long, Lat)
# subset for various windows
scipap.euro <- filter(scipap, Continent == "Europe")
scipap.usa <- filter(scipap, Country == "USA")

# count sites
scipap.euro.c <- count(scipap.euro, SiteName)
scipap.euro.c <- left_join(scipap.euro.c, 
                           scipap.euro, 
                           by = "SiteName")
scipap.euro.c <- distinct(scipap.euro.c, 
                          SiteName, 
                          .keep_all = TRUE)

scipap.usa.c <- count(scipap.usa, SiteID)
scipap.usa.c <- left_join(scipap.usa.c, scipap.usa, by = "SiteID")
scipap.usa.c <- distinct(scipap.usa.c, SiteID, .keep_all = TRUE)
# spatial data
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
equal_earth <- "+proj=eqearth +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
world <- ne_download(scale = 110, type = "land", category = "physical", returnclass = "sf")
world.equal <- st_transform(world, crs = equal_earth)
countries <- ne_download(scale = 50, type = "countries", category = "cultural", returnclass = "sf")

europe <- ("/home/marc/Dropbox/m_pers/people3k/quality/europe.shp") # load data
europe <- shapefile(europe)
europe <- st_as_sf(europe)
europe <- st_transform(europe, equal_earth)

nw.europe <- countries[countries$ADMIN == "Netherlands" | countries$ADMIN == "Belgium" | countries$ADMIN == "Denmark" | countries$ADMIN == "France" |
  countries$ADMIN == "Germany" | countries$ADMIN == "Luxembourg", ]
nw.europe <- st_transform(nw.europe, equal_earth)
nw.europe <- st_intersection(europe, nw.europe)
mask.nw.europe <- as.owin(nw.europe)
# mask usa
usa <- ne_states(country = "United States of America", returnclass = "sf")
# mask desert west US
dw.usa <- usa[usa$name == "Idaho" | usa$name == "Colorado" | usa$name == "Utah" | usa$name == "Arizona" | usa$name == "New Mexico" | usa$name == "Wyoming" |
  usa$name == "Nevada", ]
dw.usa <- st_transform(dw.usa, equal_earth)
mask.dw.usa <- as.owin(dw.usa)
# reproject point data and prepare individual ppp for KDE analyses
# europe
p.scipap.euro <- st_as_sf(scipap.euro.c, coords = c("Long", "Lat"), crs = wgs84)
euro.eqe <- st_transform(p.scipap.euro, equal_earth)
euro.eqe <- as(euro.eqe, "Spatial")
euro.eqe <- as(euro.eqe, "ppp")
# north-west europe
nw.euro.ppp <- ppp(euro.eqe$x, euro.eqe$y, window = mask.nw.europe, marks = euro.eqe$marks$n)
# usa
p.scipap.usa <- st_as_sf(scipap.usa.c, coords = c("Long", "Lat"), crs = wgs84)
usa.eqe <- st_transform(p.scipap.usa, equal_earth)
usa.eqe <- as(usa.eqe, "Spatial")
usa.eqe <- as(usa.eqe, "ppp")
# dw.usa
dw.usa.ppp <- ppp(usa.eqe$x, usa.eqe$y, window = mask.dw.usa, marks = usa.eqe$marks$n)
# risk surface analysis
# north-western europe
h0.nw.euro <- OS(nw.euro.ppp, nstar = "npoints")
kde.dates.nw.euro <- bivariate.density(nw.euro.ppp, h0 = h0.nw.euro, adapt = FALSE, edge = "diggle")
plot(kde.dates.nw.euro, main = "Kernel Density Estimate", xlab = "Easting", ylab = "Northing")
plot(nw.europe, col = sf.colors(n = 1, alpha = 0), add = TRUE)
kde.sites.nw.euro <- bivariate.density(nw.euro.ppp, h0 = h0.nw.euro, adapt = FALSE, edge = "diggle", weights = nw.euro.ppp$marks)
plot(kde.sites.nw.euro,
  main = expression("Kernel Density estimate (weighed by number of "^14 * "C dates per sites)"), xlab = "Easting", ylab = "Northing"
)
plot(nw.europe, col = sf.colors(n = 1, alpha = 0), add = TRUE)
dates.sites.nw.euro <- risk(kde.dates.nw.euro, kde.sites.nw.euro, tolerate = TRUE)
plot(dates.sites.nw.euro, main = "Risk surface analysis", xlab = "Easting", ylab = "Northing")
plot(nw.europe, col = sf.colors(n = 1, alpha = 0), add = TRUE)
# desert west usa
h0.dw.usa <- OS(dw.usa.ppp, nstar = "npoints")
dw.usa.dates <- bivariate.density(dw.usa.ppp, h0 = h0.dw.usa, adapt = FALSE, edge = "diggle")
plot(dw.usa.dates, main = "Kernel Density Estimate", xlab = "Easting", ylab = "Northing")
plot(dw.usa, col = sf.colors(n = 1, alpha = 0), add = TRUE)
dw.usa.sites <- bivariate.density(dw.usa.ppp, h0 = h0.dw.usa, adapt = FALSE, edge = "diggle", weights = dw.usa.ppp$marks)
plot(dw.usa.sites,
  main = expression("Kernel Density Estimate (weighed by number of "^14 * "C dates per sites)"), xlab = "Easting", ylab = "Northing"
)
plot(dw.usa, col = sf.colors(n = 1, alpha = 0), add = TRUE)
dates.sites.dw.usa <- risk(dw.usa.sites, dw.usa.dates, tolerate = TRUE)
plot(dates.sites.dw.usa, main = "Risk surface analysis", xlab = "Easting", ylab = "Northing")
plot(dw.usa, col = sf.colors(n = 1, alpha = 0), add = TRUE)
