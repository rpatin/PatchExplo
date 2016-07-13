#' Divide a circular disk around a central point into concentric rings.
#'
#' @param central_point a SpatialPoints or SpatialPointsDataFrame in the center of the patch.
#' @inheritParams explore
#' @param nptsPerimeter number of points in perimeter

# source("../../UTM35S.R")
# lions <- readRDS("../../../data/Hwange/Formatted/NightLionsTest.Rds")
# pump=readOGR(dsn="../../../data/Hwange/Raw/Waterholes/waterhole_park_update2_Hugo_2014.shp",layer="waterhole_park_update2_Hugo_2014")
# pump@data$pump[229]="pan"
# pump=pump[pump@data$pump == "pump",]
# proj4string(pump)=UTMstring
# waterholes <- pump

# nptsPerimeter = 100
generate_circle_patch <- function(central_point,maxdist=10000,binsize=1000,nptsPerimeter=100,projstring)
{
  centerPoints=c("x"=central_point@coords[1,1],"y"=central_point@coords[1,2])
  names(centerPoints) <- c("x","y")
  PatchCircles <- list()

  StartCircle <- sampSurf::spCircle(centerPoint=centerPoints,radius=binsize,nptsPerimeter=nptsPerimeter,spUnits=CRS(projstring))$spCircle
  sp::proj4string(StartCircle)<- projstring

#   PatchCircles[[paste(binsize)]] <- list("UnExplored"=StartCircle,"Explored"=NA)
  PatchCircles[[paste(binsize)]] <- list("Total"=StartCircle,"UnExplored"=StartCircle,"Explored"=NA)

  for(dist in seq(2*binsize,maxdist,by=binsize))
  {
    large <- sampSurf::spCircle(centerPoint=centerPoints,radius=dist,nptsPerimeter=nptsPerimeter,spUnits=CRS(projstring))$spCircle
    small <- sampSurf::spCircle(centerPoint=centerPoints,radius=dist-binsize,nptsPerimeter=nptsPerimeter,spUnits=CRS(projstring))$spCircle
    Circle <- rgeos::gDifference(large,small)
    sp::proj4string(Circle) <- projstring
    PatchCircles[[paste(dist)]] <- list("Total"=Circle,"UnExplored"=Circle,"Explored"=NA)
  }
  class(PatchCircles)<- "circlepatch"
  return(PatchCircles)
}

