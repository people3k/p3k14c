library(magrittr)

# The cleaned and fuzzed p3k14c dataset
# NOTE: DOWNLOAD FROM DOI HERE

# Download low resolution land area of Earth
download.file(
  url = "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip",
  destfile = here::here("data-raw/ne_110m_land.zip"))

unzip(zipfile = here::here("data-raw/ne_110m_land.zip"),
      exdir = here::here("data-raw/ne_110m_land"))

world <- 
  here::here("data-raw/ne_110m_land") %>%
  sf::read_sf() %>%
  sf::st_transform("EPSG:8857") %>%
  sf::st_union()

usethis::use_data(world)

# Northwestern Europe
nw_europe <-
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
  sf::st_transform("ESRI:102013") %>%
  dplyr::group_by(admin) %>%
  dplyr::summarise() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::rename(Name = admin)

usethis::use_data(nw_europe,
                  overwrite = TRUE)

conus <-
  rnaturalearth::ne_states(country = "United States of America", 
                         returnclass = "sf") %>%
  dplyr::filter(!(name %in% c("Hawaii",
                              "Alaska")))%>%
  sf::st_transform("ESRI:102003") %>%
  dplyr::select(Name = name)

usethis::use_data(conus,
                  overwrite = TRUE)
