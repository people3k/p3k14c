#### sensitivity (including kde) analysis for SciPaper####
library(p3k14c)
library(ggplot2)

# Spatial windows for risk analysis
windows <-
  list(
    
    # Global land area (EPSG 8857)
    `World` = p3k14c::world,
    
    # Northwestern Europe in Albers Equal Area Conic (ESRI 102013)
    `Northwestern Europe` = p3k14c::nw_europe,
    
    # Contiguous USA in Albers Equal Area Conic (ESRI 102003)
    `United States` = p3k14c::conus
    
  )

# Radiocarbon date data
p3k14c_data <-
  "vignettes/articles/raw_data/P3k14C_scrubbed_fuzzed.csv" %>%
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

# World map with radiocarbon dates
ggplot() +
  geom_sf(data = p3k14c::world) +
  geom_sf(data = p3k14c_data,
          mapping = aes(color = Continent,
                        fill = Continent),
          alpha = 0.2,
          size = 0.75,
          shape = 19,
          stroke = 0) +
  geom_sf_text(data = 
                 tibble::tibble(
                   x = c(-180,-180,-180),
                   y = c(-50,0,50),
                   label = c("50ºS", "0º","50ºN")
                 ) %>%
                 sf::st_as_sf(coords = c("x","y"),
                              crs = 4326),
               mapping = aes(label = label),
               size = 3,
               colour = "grey30",
               hjust = 1.5) +
  scale_x_continuous(expand = ggplot2::expansion(0.05,0)) +
  scale_y_continuous(expand = ggplot2::expansion(0,0)) +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_map(legend.position = c(0.1,0.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   alpha = 1)))

world_plot_ratio <- 
  p3k14c::world %>%
  sf::st_bbox()  %>%
  as.list() %$% 
  {(ymax - ymin) / (xmax - xmin)}

fig_width <- 178
fig_height <- fig_width * world_plot_ratio

ggplot2::ggsave("Figure3_global_map.pdf",
                width = fig_width,
                height = fig_height,
                units = "mm"
)


# Count dates/sites
p3k14c_data %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Continent) %>%
  dplyr::summarise(Dates = sum(Dates, na.rm = TRUE),
                   Sites = dplyr::n()) %>%
  na.omit() %>%
  dplyr::arrange(dplyr::desc(Dates))

# Create a Point Pattern ("ppp") object
p3k14c_data_ppp <-
  p3k14c_data %>%
  # Cast to a Spatial Points object
  sf::as_Spatial() %>%
  # Cast to a Point Pattern object
  maptools::as.ppp.SpatialPointsDataFrame() %>%
  # Window to extend KDE around globe
  window(
    windows$World %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::as_Spatial() %>%
      maptools::as.owin.SpatialPolygons()
  )


# Create a KDE of the sites
p3k14c_kde_sites <-
  sparr::bivariate.density(pp = p3k14c_data_ppp, 
                           h0 = 200000, 
                           adapt = FALSE, 
                           edge = "none",
                           resolution = 4000)

# Create a KDE of the sites, 
# weighted by the number of dates at each site
p3k14c_kde_dates <-
  sparr::bivariate.density(pp = p3k14c_data_ppp, 
                           h0 = 200000, 
                           adapt = FALSE, 
                           edge = "none",
                           resolution = 4000,
                           weights = p3k14c_data_ppp$marks$Dates)

# Calculate the spatial relative risk/density ratio
p3k14c_risk <-
  sparr::risk(p3k14c_kde_sites,
              p3k14c_kde_dates,
              tolerate = TRUE)

# Get two-sided tolerance values
p3k14c_risk$P$v <-
  p3k14c_risk$P %>%
  as.matrix() %>%
  { 2 * pmin(., 1 - .) }

normalize_raster <- 
  function(x){
    # This simply re-normalizes so the values in the area of interest equal 1
    raster::values(x) <- 
      raster::values(x) %>% 
      { . / sum(., na.rm = TRUE) }
    x
  }

plot_kde <- 
  function(global_kde, windows){
    windows %>%
      purrr::map(
        ~(
          global_kde %>%
            as_raster.bivden(crs = "EPSG:8857",
                             crop = .x) %>%
            raster::projectRaster(crs = 
                                    .x %>%
                                    sf::st_crs() %>%
                                    as("CRS")) %>%
            normalize_raster() %>%
            raster::trim() %>%
            raster::as.data.frame(xy = TRUE) %>%
            na.omit() %>%
            {
              ggplot() +
                geom_tile(data = .,
                          mapping = aes(x = x, 
                                        y = y, 
                                        fill = layer)) +
                geom_sf(data = .x,
                        fill = "transparent",
                        color = "black") +
                scale_fill_viridis_c(option = "C") +
                theme_map()
            }
          
        )
      )
  }

plot_risk <- 
  function(global_risk, windows){
    windows %>%
      purrr::map(
        ~(
          global_risk %>%
            as_raster.rrs(crs = "EPSG:8857",
                          crop = .x) %>%
            raster::projectRaster(crs = 
                                    .x %>%
                                    sf::st_crs() %>%
                                    as("CRS")) %>%
            raster::trim() %>%
            raster::as.data.frame(xy = TRUE) %>%
            na.omit() %>%
            {
              ggplot() +
                geom_tile(data = .,
                          mapping = aes(x = x, 
                                        y = y, 
                                        fill = rr)) +
                geom_sf(data = .x,
                        fill = "transparent",
                        color = "black") +
                geom_contour(data = .,
                             mapping = aes(x = x, 
                                           y = y,
                                           z = p),
                             breaks = 0.05,
                             colour = "white") +
                scale_fill_viridis_c(option = "C",
                                     limits = c(-0.5, 0.5)) +
                theme_map()
            }
          
        )
      )
  }

# Generate the plots
p3k14c_plots <- 
  list(
    sites = 
      plot_kde(global_kde = p3k14c_kde_sites,
               windows = windows),
    dates = 
      plot_kde(global_kde = p3k14c_kde_dates,
               windows = windows),
    risk = 
      plot_risk(global_risk = p3k14c_risk,
                windows = windows)
  ) %>%
  purrr::transpose()
