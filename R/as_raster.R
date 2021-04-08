#' Converters for sparr objects to rasters
#'
#' @param x An object of class `bivden`
#' @param crs Coordinate reference system, something suitable 
#' as input to `st_crs`
#' @param crop `sf` object to which to crop the raster
#' 
#' @return A raster
#' @importFrom magrittr `%<>%` `%>%`
#' @importFrom stats na.omit
#' @export
as_raster.bivden <-
  function(x, crs, crop = NULL){
    out <- raster::raster(x$z)
    raster::crs(out) <- crs
    if(!is.null(crop)){
      out %<>%
        raster::mask(
          crop %>%
            sf::st_transform(crs) %>%
            as("Spatial")) %>%
        raster::crop(
          crop %>%
            sf::st_transform(crs) %>%
            as("Spatial"), snap = "out")
    }
    out %>%
      raster::trim() %>%
      raster::as.data.frame(xy = TRUE) %>%
      na.omit() %>%
      tibble::as_tibble()
  }

#' Converters for sparr objects to rasters
#'
#' @param x An object of class `rrs`
#' @param crs Coordinate reference system, something suitable 
#' as input to `st_crs`
#' @param crop `sf` object to which to crop the raster
#'
#' @return A raster
#' @importFrom methods as
#' @importFrom magrittr `%>%`
#' @importFrom stats na.omit
#' @export
as_raster.rrs <-
  function(x, crs, crop = NULL){
    out <-
      list(
      rr = raster::raster(x$rr),
      p = raster::raster(x$P)
    ) %>%
      raster::brick()
    
    raster::crs(out) <- crs
    
    if(!is.null(crop)){
      out %<>%
        raster::mask(
          crop %>%
            sf::st_transform(crs) %>%
            as("Spatial")) %>%
        raster::crop(
          crop %>%
            sf::st_transform(crs) %>%
            as("Spatial"), snap = "out")
    }
    
    out %>%
      raster::trim() %>%
      raster::as.data.frame(xy = TRUE) %>%
      na.omit() %>%
      tibble::as_tibble()
  }