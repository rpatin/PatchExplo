
#' Analyze a concentric ring of a \code{circlepatch} object
#'
#' Used to build its summary
#'
#' @param zone one element of a \code{circlepatch} object

zone2data <- function(zone){
  if(class(zone$UnExplored) == "logical"){
    return(data.frame("ExploredPercent"=100,"Explored"=rgeos::gArea(zone$Explored)))
  }else{
    if(class(zone$Explored) == "logical"){
      return(data.frame("ExploredPercent"=0,"Explored"=0))
    }else{
      explored <- rgeos::gArea(zone$Explored)
      unexplored <- rgeos::gArea(zone$UnExplored)
      return(data.frame("ExploredPercent"=100*explored/(explored+unexplored),"Explored"=explored))
    }
  }
}

#' Prevent geometry from being invalid
#'
#' @param x SpatialPolygons or SpatialPolygonsDataFrame

prevent_invalid_geometry <- function(x){
  return(rgeos::gBuffer(x, byid=TRUE, width=0))
}

#' Pause between graphs
#'
#'
wait_next_graph <- function(wait){
  if (wait) invisible(readline(prompt="Press [enter] to continue"))
}
