
#' Analyze a concentric ring of a \code{circlepatch} object
#'
#' Used to build its summary
#'
#' @param zone one element of a \code{circlepatch} object

# for(i in 1:length(x)){
#   zone2data(x[[i]])
# }
# zone <- x[[20]]

zone2data <- function(zone){
  if(class(zone$UnExplored) == "logical"){
    if(class(zone$Explored) == "logical"){
      return(data.frame("ExploredPercent"=0,"Explored"=0))
    } else {
      return(data.frame("ExploredPercent"=rgeos::gArea(zone$Explored)/rgeos::gArea(zone$Total),"Explored"=rgeos::gArea(zone$Explored)))
    }
  }else{
    if(class(zone$Explored) == "logical"){
      return(data.frame("ExploredPercent"=0,"Explored"=0))
    }else{
      explored <- rgeos::gArea(zone$Explored)
      unexplored <- rgeos::gArea(zone$UnExplored)
      total <- rgeos::gArea(zone$Total)
      return(data.frame("ExploredPercent"=100*explored/(total),"Explored"=explored))
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

#' Plot waterholes with names labeled
#'
#' @param x SpatialPointsDataFrame
#' @param buffer Size of buffer to plot around waterholes

plot_waterholes <- function(x,buffer=NULL){
  if(!is.null(x)){
    if(!is.null(buffer)){
      plot(rgeos::gBuffer(x,width=buffer),add=T,lty=2)
    }
    points(x,col="blue",pch=19,cex=1.5,label)
    text(x,label=x@data$name,cex=0.5,pos=4)
  }
  return(NULL)
}
