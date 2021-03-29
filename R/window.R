window.ppp <- 
  function(x, w){
    spatstat.geom::Window(x) <- w
    x
  }