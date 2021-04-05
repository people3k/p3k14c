#' The scrubbed and fuzzed p3k14c dataset
#'
#' The dates in this dataset were gathered from 40 pre-existing datasets, 
#' verified, and cleaned according to a standardized sample selection criteria. 
#' Dates for the United States and Canada are aggregated to county level, 
#' while locational information for dates from one dataset 
#' (Guedes and Bocinsky 2018) have been truncated to 2 decimal points. 
#' All dates are uncalibrated.
#'
#' @format An object of class `tibble`.
#' @source \url{https://core.tdar.org/dataset/459163/p3k14c}
"p3k14c_data"

#' World land area.
#'
#' A `MULTIPOLYGON` of global land area.
#'
#' @format An object of class `sfc_MULTIPOLYGON`.
#' @source \url{https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip}
"world"

#' Northwestern Europe country borders.
#'
#' A `MULTIPOLYGON` of Northwestern Europe.
#'
#' @format An object of class `sfc_MULTIPOLYGON`.
#' @source \url{https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states.zip}
"nw_europe"

#' Contiguous United States borders.
#'
#' A `MULTIPOLYGON` of the contiguous United States.
#'
#' @format An object of class `sfc_MULTIPOLYGON`.
#' @source \url{https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states.zip}
"conus"