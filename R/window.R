#' Tidy function to window a Point Pattern 
#'
#' @param x An object of class `ppp`
#' @param w An object of class `owin`
#'
#' @return An object of class `ppp` windowed to `w`
#' @export
window.ppp <- 
  function(x, w){
    spatstat.geom::Window(x) <- w
    x
  }