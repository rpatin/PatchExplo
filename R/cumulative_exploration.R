#' Analyse exploration around a central place like a waterhole
#'
#' \code{explore} compute percentage and total area explored in each distance
#' class around the central place, for each day. It also compute the new area
#' explored at the beginning of the first day. It can work with any sequence of
#' movement, given each sequence as a unique \code{index} value
#'
#' @param data a data frame with coordinates  \code{data$x} and \code{data$y},
#'   and nearest waterhole in column \code{data$name}. It should be a movement
#'   sequence for a single animal and a single exploration event.
#' @param buffer width of exploration around the trajectory.
#' @param maxdist maximum distance to the central place above which we ignore
#'   exploration
#' @param binsize width of distance rings
#' @param projstring CRS projection of data
#' @param waterholes SpatialPointsDataFrame with waterholes location and column
#'   \code{$name} in data slot.
#' @examples
#' explore(data,buffer=100,maxdist=10000,binsize=1000,projstring,waterholes)

# library(rgdal)
# library(sp)
#
# source("../../UTM35S.R")
# lions <- readRDS("../../../data/Hwange/Formatted/NightLionsTest.Rds")
# data <- lions[[1]]
# pump=readOGR(dsn="../../../data/Hwange/Raw/Waterholes/waterhole_park_update2_Hugo_2014.shp",layer="waterhole_park_update2_Hugo_2014")
# pump@data$pump[229]="pan"
# pump=pump[pump@data$pump == "pump",]
# proj4string(pump)=UTMstring
# waterholes <- pump
# data <- lions[[1]]
#
# buffer=200
# maxdist=10000
# binsize=1000
# projstring = UTMstring
#
# test.explo <- explore(data,buffer=200,binsize=5000,projstring = UTMstring,waterholes = waterholes)

explore <- function(data,buffer=100,maxdist=10000,binsize=1000,projstring,waterholes)
{
  listindex <- unique(data$index)
  if (any(diff(sort(listindex)) != 1)) stop("There is a gap in index")
  central_point <- waterholes[which(waterholes@data$name==as.character(dplyr::first(data$name))),]

  BeginPatch <- generate_circle_patch(central_point,projstring=projstring,maxdist = maxdist,binsize = binsize)

  x <- list("circlepatches"=list(),
                      "nday"=length(listindex),
                      "buffer"=buffer,
                      "binsize"=binsize,
                      "maxdist"=maxdist,
                      "waterhole"=central_point,
                      "data"=data)
  class(x) <- 'exploration'

  night <- dplyr::filter(data,night=="night",index==listindex[1])
  ExploredDay <- explore_one_night(night=night,
                                   central_point=central_point,
                                   already_explored=BeginPatch,
                                   buffer=buffer,
                                   projstring=projstring,
                                   marginal=F)

  x$circlepatches[[1]] <- list("total"=ExploredDay,
                                         "current"=ExploredDay,
                                         "marginal"=ExploredDay)

  CumulativeExploredDay <- ExploredDay

  for(i in 2:length(listindex)){
    # print(i)
    night <- dplyr::filter(data,night=="night",index==listindex[i])
    MarginalExploredDay <- explore_one_night(night=night,
                                               central_point=central_point,
                                               already_explored=CumulativeExploredDay,
                                               buffer=buffer,
                                               projstring=projstring,
                                               marginal=T)

    CumulativeExploredDay <- explore_one_night(night=night,
                                               central_point=central_point,
                                               already_explored=CumulativeExploredDay,
                                               buffer=buffer,
                                               projstring=projstring,
                                               marginal=F)

    ExploredDay <- explore_one_night(night=night,
                                               central_point=central_point,
                                               already_explored=BeginPatch,
                                               buffer=buffer,
                                               projstring=projstring,
                                               marginal=F)

    x$circlepatches[[i]] <- list("total"=CumulativeExploredDay,
                                           "current"=ExploredDay,
                                           "marginal"=MarginalExploredDay)
  }
  names(x$circlepatches) <- seq(1,x$nday)
  return(x)
}
#' Analyse exploration around a central place for a single night
#'
#' \code{explore_one_night} compute percentage and total area explored in each
#' distance class around the central place, for each day. It also compute the
#' new area explored at the beginning of the first day. It can work with any
#' sequence of movement, given each sequence as a unique \code{index} value
#'
#' @param night a data frame with coordinates  \code{data$x} and \code{data$y}
#' @param buffer width of exploration around the trajectory.
#' @param central_point single feature SpatialPointsDataFrame with column
#'   \code{$name} in data slot.
#' @param already_explored \code{\link{circlepatch}} object containing areas
#'   unexplored and already explored
#' @examples
#' explore_one_night(night,central_point,already_explored,buffer=100,projstring,marginal=F)

# already_explored<-x$circlepatches[[10]]$total
# marginal=T
# zone= already_explored[[5]]

explore_one_night <- function(night,central_point,already_explored,buffer=100,projstring,marginal=F){
  night$idline <- 1:nrow(night)
  LinesUTM <- lapply(1:(nrow(night)-1),function(j){
    z<-night[c(j,j+1),]
    return(sp::Lines(list(sp::Line(cbind(z$x,z$y))),ID=dplyr::first(z$idline)))
  })
  rownames(night)<- night$idline
  SplineUTM<-sp::SpatialLines(LinesUTM,proj4string = sp::CRS(projstring))
  # spdf <- SpyUTM@data
  SplineBuffer <- rgeos::gBuffer(SplineUTM,width=buffer)

  #
  #   plot(zone$UnExplored,col='grey')
  #   plot(newExplored,col='orange',add=T)
  #   plot(zone$Explored,col='yellow')
  # already_explored <- ExploredDay

  Explored.list <- lapply(already_explored,function(zone){
    # plot(zone$UnExplored,add=T)
    if (class(zone$UnExplored) != "logical")
    {
      # plot(zone$UnExplored,add=T,col='red')
      newUnexplored = rgeos::gDifference(zone$UnExplored,SplineBuffer)
      if(class(newUnexplored)=="SpatialCollections"){
        newUnexplored <- newUnexplored@polyobj
      }
      newUnexplored <- prevent_invalid_geometry(newUnexplored)
      # plot(newUnexplored,add=T,col='grey')
      newExplored = rgeos::gIntersection(zone$UnExplored,SplineBuffer)
      # plot(newExplored,add=T,col='red')
      if(is.null(newUnexplored)) newUnexplored <- NA
      if(is.null(newExplored)) newExplored <- NA

      if (class(zone$Explored) != "logical"){
        if (class(newExplored) == "logical"){
          if (marginal == T){
            newExplored <- NA
          } else {
            newExplored= zone$Explored
          }
        } else {
          if (class(newExplored) == "SpatialCollections"){
            newExplored <- newExplored@polyobj
          }
          if (marginal == T){
            newExplored <- rgeos::gDifference(newExplored,zone$Explored)
            newExplored <- prevent_invalid_geometry(newExplored)
          } else {
            newExplored <-  rgeos::gUnion(newExplored,zone$Explored)
            newExplored <- prevent_invalid_geometry(newExplored)
          }
        }
      }
      return(list("UnExplored"=newUnexplored,"Explored"=newExplored))
    } else {
      if (marginal == T){
        zone$Explored <- NA
      }
      return(zone)
    }
  })
  class(Explored.list) <- "circlepatch"
  return(Explored.list)
}
