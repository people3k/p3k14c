#### sensitivity (including kde) analysis for SciPaper####
library(p3k14c)

# Spatial windows for risk analysis
windows <-
  list(
    
    # Global land area (EPSG 8857)
    `World` = 
      rnaturalearth::ne_countries(scale = 10,
                                  returnclass = "sf")  %>%
      dplyr::select(geometry) %>%
      sf::st_transform("EPSG:8857") %>%
      sf::st_union(),
    
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
               Continent,
               Long,
               Lat,
               name = "Dates") %>%
  sf::st_as_sf(
    coords = c("Long", "Lat"),
    crs = "EPSG:4326",
    remove = FALSE
  ) %>%
  # Transform to equal earth projection (EPSG 8857)
  sf::st_transform("EPSG:8857")

# Count dates/sites
radiocarbon_dates %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Continent) %>%
  dplyr::summarise(Dates = sum(Dates, na.rm = TRUE),
                   Sites = dplyr::n()) %>%
  na.omit() %>%
  dplyr::arrange(dplyr::desc(Dates))

# Create a Point Pattern ("ppp") object
radiocarbon_dates_ppp <-
  radiocarbon_dates %>%
  # Cast to a Spatial Points object
  sf::as_Spatial() %>%
  # Cast to a Point Pattern object
  maptools::as.ppp.SpatialPointsDataFrame() %>%
  # Window to the World in order to extend KDE around globe
  window(
    windows$World %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_buffer(1000000) %>%
      sf::as_Spatial() %>%
      maptools::as.owin.SpatialPolygons()
  )

# Create a KDE of the sites
world_kde_sites <-
  sparr::bivariate.density(pp = radiocarbon_dates_ppp, 
                           h0 = 200000, 
                           adapt = FALSE, 
                           edge = "diggle",
                           resolution = 2000)

# Create a KDE of the sites, 
# weighted by the number of dates at each site
world_kde_sites_dates <-
  sparr::bivariate.density(pp = radiocarbon_dates_ppp, 
                           h0 = 200000, 
                           adapt = FALSE, 
                           
                           edge = "diggle",
                           weights = radiocarbon_dates_ppp$marks$Dates,
                           resolution = 2000)

# Calculate the spatial relative risk/density ratio
world_risk <-
  sparr::risk(world_kde_sites,
              world_kde_sites_dates, 
              tolerate = TRUE)







world_kde_sites <- 
  world_kde_sites$z %>%
  raster::raster()
raster::crs(world_kde_sites) <- as(sf::st_crs("EPSG:8857"), "CRS")


world_kde_sites_dates <- 
  world_kde_sites_dates$z %>%
  raster::raster()
raster::crs(world_kde_sites_dates) <- as(sf::st_crs("EPSG:8857"), "CRS")

world_risk <- 
  world_risk$rr %>%
  raster::raster()
raster::crs(world_risk) <- as(sf::st_crs("EPSG:8857"), "CRS")
plot(world_risk > -5)

raster::plot(raster::raster(world_risk$rr))

plot(world_kde_sites)
plot(world_kde_sites_dates)
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
