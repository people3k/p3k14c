#### sensitivity (including kde) analysis fo SciPaper####
# install.packages("devtools")
# install.packages("purrr")
# install.packages("magrittr")
library(magrittr)
c(
  "readr",
  "tibble",
  "tidyr",
  "dplyr",
  "sf",
  "sparr",
  "spatstat",
  "magrittr",
  "ggplot2",
  "maptools"
) %T>%
  purrr::walk(devtools::install_cran,
              quiet = TRUE
  ) %>%
  purrr::walk(library,
              character.only = TRUE
  )

# Functions
window.ppp <- function(x, w){
  spatstat.geom::Window(x) <- w
  x
}


# data
scipap <-
  "data/raw_data/radiocarbon_dates_scrubbedv5_1.csv" %>%
  here::here() %>%
  readr::read_csv(col_types = 
                    readr::cols(
                      Age = readr::col_double(),
                      Error = readr::col_double(),
                      Long = readr::col_double(),
                      Lat = readr::col_double(),
                      .default = readr::col_character()
                    )) %>%
  tidyr::drop_na(Long, Lat) %>%
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
  ) %>%
  dplyr::count(Region, 
               SiteName, 
               Long, 
               Lat) %>%
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    crs = 4326
  )

# Count dates/sites
scipap %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(Dates = sum(n, na.rm = TRUE),
                   Sites = dplyr::n())


# windows
windows <-
  list(`Northwestern Europe` = 
         rnaturalearth::ne_states(country = c("Netherlands",
                                              "Belgium",
                                              "Denmark",
                                              "France",
                                              "Germany",
                                              "Luxembourg"),
                                  returnclass = "sf") %>%
         dplyr::filter(geonunit %in% c("Netherlands",
                                       "Belgium",
                                       "Denmark",
                                       "France",
                                       "Germany",
                                       "Luxembourg")),
       `Desert US` = 
         rnaturalearth::ne_states(country = "United States of America", 
                                  returnclass = "sf") %>%
         dplyr::filter(name %in% c("Idaho",
                                   "Colorado",
                                   "Utah",
                                   "Arizona",
                                   "New Mexico",
                                   "Wyoming",
                                   "Nevada"))) %>%
  purrr::map(~(
    .x %>%
      sf::st_transform(8857) %>%
      as("Spatial") %>%
      as("owin")
  ))




# reproject point data and prepare individual ppp for KDE analyses
# europe
kde_dates <-
  windows %>%
  purrr::map(~window(scipap %>%
                       sf::st_transform(8857) %>%
                       as("Spatial") %>%
                       as("ppp"), 
                     .x)) %>%
  purrr::map(~(
    .x %>%
      sparr::OS() %>%
      bivariate.density(pp = .x, 
                        h0 = ., 
                        adapt = FALSE, 
                        edge = "diggle")
  ))

kde_sites <-
  windows %>%
  purrr::map(~window(scipap %>%
                       sf::st_transform(8857) %>%
                       as("Spatial") %>%
                       as("ppp"), 
                     .x)) %>%
  purrr::map(~(
    .x %>%
      sparr::OS() %>%
      bivariate.density(pp = .x, 
                        h0 = ., 
                        adapt = FALSE, 
                        edge = "diggle",
                        weights = .x$marks$n)
  ))

kde_risk <-
  purrr::map2(.x = kde_dates,
            .y = kde_sites,
            .f = ~risk(.x, .y, tolerate = TRUE))


plot(kde_dates$`Northwestern Europe`)
plot(kde_dates$`Desert US`)

us_ppp <- 
  scipap %>%
  dplyr::filter(Region == "United States") %>%
  sf::st_transform(8857) %>%
  as("Spatial") %>%
  as("ppp") %$%
  ppp(x, 
      y, 
      window = dw.usa,
      marks = marks$n)


us_ppp %>%
  sparr::OS(nstar = "npoints") %>%
  bivariate.density(pp = us_ppp, 
                    h0 = ., 
                    adapt = FALSE, 
                    edge = "diggle",
                    weights = us_ppp$marks) %>%
  plot()


kde.dates.euro <-
  world.ppp %>%
  
  sparr::OS(nstar = "npoints") %>%
  bivariate.density(pp = world.ppp, 
                    h0 = ., 
                    adapt = FALSE, 
                    edge = "diggle")

dw.usa.ppp <-
  scipap %>%
  dplyr::filter(Region == "United States") %>%
  sf::st_transform(8857) %>%
  as("Spatial") %>%
  as("ppp") %$%
  ppp(x, 
      y, 
      window = dw.usa,
      marks = marks$n)

world.ppp <-
  scipap %>%
  sf::st_transform(8857) %>%
  as("Spatial") %>%
  as("ppp") %$%
  ppp(x, 
      y, 
      window = world.equal,
      marks = marks$n)

kde.dates.world <-
  world.ppp %>%
  sparr::OS(nstar = "npoints") %>%
  bivariate.density(pp = world.ppp, 
                    h0 = ., 
                    adapt = FALSE, 
                    edge = "diggle")


# risk surface analysis
# north-western europe
h0.nw.euro <- sparr::OS(nw.euro.ppp, nstar = "npoints")
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
