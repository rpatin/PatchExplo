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


generate_circle_patch <- function(central_point,maxdist=10000,binsize=1000,nptsPerimeter=100,projstring)
{
  centerPoints=c("x"=WH@coords[1,1],"y"=WH@coords[1,2])
  names(centerPoints) <- c("x","y")
  PatchCircles <- list()

  StartCircle <- sampSurf::spCircle(centerPoint=centerPoints,radius=BinSize,nptsPerimeter=nptsPerimeter,spUnits=projstring)$spCircle
  proj4string(StartCircle)<- projstring

  PatchCircles[[paste(BinSize)]] <- list("UnExplored"=StartCircle,"Explored"=NA)

  for(dist in seq(2*BinSize,maxDist,by=BinSize))
  {
    large <- sampSurf::spCircle(centerPoint=centerPoints,radius=dist,nptsPerimeter=nptsPerimeter,spUnits=projstring)$spCircle
    small <- sampSurf::spCircle(centerPoint=centerPoints,radius=dist-BinSize,nptsPerimeter=nptsPerimeter,spUnits=projstring)$spCircle
    Circle <- rgeos::gDifference(large,small)
    sp::proj4string(Circle) <- projstring
    PatchCircles[[paste(dist)]] <- list("UnExplored"=Circle,"Explored"=NA)
  }
  class(PatchCircles)<- "circlepatch"
  return(PatchCircles)
}
