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


plot(kde_dates$`Desert US`,
     main = "Kernel Density Estimate", 
     xlab = "Easting", 
     ylab = "Northing")
plot(kde_sites$`Desert US`,
     main = expression("Kernel Density estimate (weighed by number of "^14 * "C dates per sites)"), 
     xlab = "Easting",
     ylab = "Northing")
plot(kde_risk$`Desert US`,
     main = "Risk surface analysis", 
     xlab = "Easting", 
     ylab = "Northing")

plot(kde_dates$`Northwestern Europe`,
     main = "Kernel Density Estimate", 
     xlab = "Easting", 
     ylab = "Northing")
plot(kde_sites$`Northwestern Europe`,
     main = expression("Kernel Density estimate (weighed by number of "^14 * "C dates per sites)"), 
     xlab = "Easting",
     ylab = "Northing")
plot(kde_risk$`Northwestern Europe`,
     main = "Risk surface analysis", 
     xlab = "Easting", 
     ylab = "Northing")
