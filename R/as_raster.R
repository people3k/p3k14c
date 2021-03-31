#' Converters for sparr objects to rasters
#'
#' @param x An object of class `bivden`
#'
#' @return A raster
#' @export
as_raster.bivden <-
  function(x, crs, crop = NULL){
    out <- raster::raster(x$z)
    raster::crs(out) <- sp::CRS(crs)
    if(!is.null(crop)){
      out %<>%
        raster::mask(
          crop %>%
            sf::st_transform("EPSG:8857") %>%
            as("Spatial")) %>%
        raster::crop(
          crop %>%
            sf::st_transform("EPSG:8857") %>%
            as("Spatial"), snap = "out")
    }
    out
  }

#' Converters for sparr objects to rasters
#'
#' @param x An object of class `rrs`
#'
#' @return A raster
#' @export
as_raster.rrs <-
  function(x, crs, crop = NULL){
    out <-
      list(
      rr = raster::raster(x$rr),
      p = raster::raster(x$P)
    ) %>%
      raster::brick()
    
    raster::crs(out) <- sp::CRS(crs)
    
    if(!is.null(crop)){
      out %<>%
        raster::mask(
          crop %>%
            sf::st_transform("EPSG:8857") %>%
            as("Spatial")) %>%
        raster::crop(
          crop %>%
            sf::st_transform("EPSG:8857") %>%
            as("Spatial"), snap = "out")
    }
    
    out
    
  }