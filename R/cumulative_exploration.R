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

# source("../../UTM35S.R")
# lions <- readRDS("../../../data/Hwange/Formatted/NightLionsTest.Rds")
# pump=readOGR(dsn="../../../data/Hwange/Raw/Waterholes/waterhole_park_update2_Hugo_2014.shp",layer="waterhole_park_update2_Hugo_2014")
# pump@data$pump[229]="pan"
# pump=pump[pump@data$pump == "pump",]
# proj4string(pump)=UTMstring
# waterholes <- pump

explore <- function(data,buffer=100,maxdist=10000,binsize=1000,projstring,waterholes)
{
  listindex <- unique(data$index)
  if (any(diff(sort(listindex)) != 1)) stop("There is a gap in index")
  central_point <- waterholes[which(waterholes@data$name==as.character(dplyr::first(data$name))),]

  BeginPatch <- generate_circle_patch(central_point,projString=projString,maxDist = maxDist,BinSize = BinSize)

  night1 <- filter(totDF,night=="night",index==indexNight,indiv_code==ID)
  OtherWH <- F
  ListIndex <- seq(indexNight,indexNight+maxNight)
  i<- 1
  while(!OtherWH)
  {
    subIndex <- ListIndex[i]
    night <- filter(totDF,night=="night",index==subIndex,indiv_code==ID)
    if(nrow(night)>1)
    {
      if(subIndex==indexNight){
        ExploredDay <- Exploration(night = night,WH =WH,AlreadyExplored = BeginPatch,Buffer = Buffer)
        CumulativeExploredDay <- ExploredDay
        data <- Explored2dataframe(ExploredDay,WH,night,Buffer=Buffer,ReturnNumber=0)
        Cumdata <- data
        data <- mutate(data,CumulativeExploredPercent=ExploredPercent,CumulativeExplored=Explored)
        data$OtherWH <- F
      }else{
        CumulativeExploredDay <- Exploration(night = night,WH =WH,AlreadyExplored = CumulativeExploredDay,Buffer = Buffer)
        newCumdata <- Explored2dataframe(CumulativeExploredDay,WH,night,Buffer=Buffer,ReturnNumber=subIndex-indexNight)
        newCumdata$ExploredPercent<- newCumdata$ExploredPercent - Cumdata$ExploredPercent
        newCumdata$Explored<- newCumdata$Explored - Cumdata$Explored
        Cumdata <- newCumdata

        newCumdata <- rename(newCumdata,CumulativeExploredPercent=ExploredPercent,CumulativeExplored=Explored)
        ExploredDay <- Exploration(night = night,WH =WH,AlreadyExplored = BeginPatch,Buffer = Buffer)
        newdata <- Explored2dataframe(ExploredDay,WH,night,Buffer=Buffer,ReturnNumber=subIndex-indexNight)
        newdata <- left_join(newdata,newCumdata,by=c("distance", "ID", "waterhole", "BufferSize", "index", "ReturnNumber"))
        OtherWH <-  any(night$dist2pump < distOtherWH & as.character(night$name) != WH@data$name)
        newdata$OtherWH <- OtherWH
        data <- rbind(data,newdata)
      }
    }else{
      break()
    }
    i<- i+1
  }

  if(!is.null(data))
  {
    data$IndexFirstNight <- indexNight
    data <- mutate(data,
                   CumulativeExplored=ifelse(CumulativeExplored<0,0,CumulativeExplored),
                   CumulativeExploredPercent=ifelse(CumulativeExploredPercent<0,0,CumulativeExploredPercent)) %>% rename(indexCurrent=index)
  }
  return(data)

}
