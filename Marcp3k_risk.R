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
              type = "binary",
              quiet = TRUE
  ) %>%
  purrr::walk(library,
              character.only = TRUE
  )

# Spatial windows for risk analysis
windows <-
  list(
    
    # Global land area (EPSG 8857)
    `World` = 
      sf::st_bbox(c(xmin = -180,
                    xmax = 180,
                    ymin = -90,
                    ymax = 90), 
                  crs = "EPSG:4326") %>%
      sf::st_as_sfc() %>%
      sf::st_transform("EPSG:8857"),
    
    # Northwestern Europe in Albers Equal Area Conic (ESRI 102013)
    `Northwestern Europe` = 
      rnaturalearth::ne_states(country = c("Netherlands",
                                           "Belgium",
                                           "Denmark",
                                           "France",
                                           "Germany",
                                           "Luxembourg"),
                               returnclass = "sf") %>%
      dplyr::filter(!(name %in% c("Guyane française",
                                  "Martinique",
                                  "Guadeloupe",
                                  "St. Eustatius",
                                  "Saba",
                                  "Haute-Corse",
                                  "Corse-du-Sud",
                                  "Mayotte",
                                  "La Réunion"))) %>%
      dplyr::select(geometry) %>%
      sf::st_transform("ESRI:102013") %>%
      sf::st_union(),
    
    # Contiguous USA in Albers Equal Area Conic (ESRI 102003)
    `United States` = 
      rnaturalearth::ne_states(country = "United States of America", 
                               returnclass = "sf") %>%
      dplyr::filter(!(name %in% c("Hawaii",
                                  "Alaska")))%>%
      sf::st_transform("ESRI:102003") %>%
      sf::st_union()) %>%
  purrr::map(sf::st_cast,
             "MULTIPOLYGON")

# Radiocarbon date data
radiocarbon_dates <-
  "data/raw_data/P3k14C_scrubbed_fuzzed.csv" %>%
  here::here() %>%
  readr::read_csv(col_types = 
                    readr::cols(
                      Age = readr::col_double(),
                      Error = readr::col_double(),
                      Long = readr::col_double(),
                      Lat = readr::col_double(),
                      .default = readr::col_character()
                    )) %>%
  dplyr::mutate(dplyr::across(SiteID:SiteName, stringr::str_trim)) %>%
  tidyr::drop_na(Long, Lat) %>%
  dplyr::count(SiteID,
               SiteName,
               Long,
               Lat,
               name = "Dates") %>%
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    crs = "EPSG:4326",
    remove = FALSE
  ) %>%
  # Transform to equal earth projection (EPSG 8857)
  sf::st_transform("EPSG:8857") %>%
  # Assign windows using spatial intersection 
  # with a 100km buffer around the windows
  sf::st_join(
    windows %>%
      purrr::map(
        ~(
          .x %>%
            sf::st_transform("EPSG:8857") %>%
            sf::st_buffer(100000)
        )) %>%
      {
        tibble::tibble(Window = names(.), 
                       geom = do.call(c, .))
      } %>%
      sf::st_as_sf()
  )

# Count dates/sites
radiocarbon_dates %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Window) %>%
  dplyr::summarise(Dates = sum(Dates, na.rm = TRUE),
                   Sites = dplyr::n()) %>%
  na.omit()

radiocarbon_dates %>%
  dplyr::filter(is.na(Window)) %>%
  mapview::mapview()


# Create a Point Pattern ("ppp") object
radiocarbon_dates_ppp <-
  radiocarbon_dates %>%
  # Cast to a Spatial Points object
  as("Spatial") %>%
  # Cast to a Point Pattern object
  as("ppp") %>%
  # Window to the World in order to extend KDE around globe
  window(
    windows$World %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      as("Spatial") %>%
      as("owin")
  )

# Create a KDE of the sites
world_kde_sites <-
  bivariate.density(pp = radiocarbon_dates_ppp, 
                    h0 = 200000, 
                    adapt = FALSE, 
                    edge = "diggle",
                    resolution = 2000)

# Create a KDE of the sites, 
# weighted by the number of dates at each site
world_kde_sites_dates <-
  bivariate.density(pp = radiocarbon_dates_ppp, 
                    h0 = 200000, 
                    adapt = FALSE, 
                    edge = "diggle",
                    weights = radiocarbon_dates_ppp$marks$Dates,
                    resolution = 2000)

# Calculate the spatial relative risk/density ratio
world_risk <-
  risk(world_kde_sites,
       world_kde_sites_dates, 
       tolerate = TRUE)

world_kde_sites <- 
  world_kde_sites$z %>%
  raster::raster()
raster::crs(world_kde_sites) <- as(sf::st_crs("EPSG:8857"), "CRS")

plot(world_kde_sites[world_kde_sites > 1e-36])
world_kde_sites[world_kde_sites < 1e-30] <- NA

raster::plot(raster::raster(world_kde_sites$z))

plot(world_kde_dates)
plot(world_kde_sites)
plot(world_risk)
plot(windows$World, add = TRUE)

windows$World %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  as("Spatial") %>%
  as("owin")


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


plot(kde_dates$`United States`,
     main = "Kernel Density Estimate\nUnited States", 
     xlab = "Easting", 
     ylab = "Northing")
plot(kde_sites$`United States`,
     main = expression("Kernel Density estimate (weighed by number of "^14 * "C dates per sites)\nUnited States"), 
     xlab = "Easting",
     ylab = "Northing")
plot(kde_risk$`United States`,
     main = "Risk surface analysis\nUnited States", 
     xlab = "Easting", 
     ylab = "Northing")

plot(kde_dates$`Northwestern Europe`,
     main = "Kernel Density Estimate\nNorthwestern Europe", 
     xlab = "Easting", 
     ylab = "Northing")
plot(kde_sites$`Northwestern Europe`,
     main = expression("Kernel Density estimate (weighed by number of "^14 * "C dates per sites)\nNorthwestern Europe"), 
     xlab = "Easting",
     ylab = "Northing")
plot(kde_risk$`Northwestern Europe`,
     main = "Risk surface analysis\nNorthwestern Europe", 
     xlab = "Easting", 
     ylab = "Northing")
