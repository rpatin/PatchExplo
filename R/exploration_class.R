#' exploration class description
#'
#' @param x a \code{exploration} object generated for instance by
#'   \code{\link{explore}}
#' @name exploration
NULL

#' Print object of \code{exploration} class
#'
#' @rdname exploration

# x <- test.explo
#

print.exploration <- function(x){
  cat(paste("exploration object",
            "\n Bin size : ", x$binsize,"m",
            "\n Maximum distance : ", x$maxdist,"m",
            "\n Number of bins : ",x$maxdist/x$binsize,sep=""),
            "\n Number of exploration days : ", x$nday,
            "\n Buffer Size : ", x$buffer,"m",
            "\n Waterhole : ", as.character(x$waterhole@data$name),
            "\n Individual ID : ", as.character(dplyr::first(x$data$id)))
}


#' Plot object of \code{circlepatch} class
#' @rdname exploration

plot.exploration <- function(x, type = c("marginal", "daily"),wait=T){
  if (type == "marginal"){
    plot(x$circlepatches[[1]]$total,main='Day 1',col='red')
    wait_next_graph(wait)
    if (x$nday >1){
      for(i in 2:x$nday){
        plot(x$circlepatches[[i-1]]$total,main=paste("Day ",i,sep=""),col='grey90')
        plot(x$circlepatches[[i]]$marginal,main=paste("Day ",i,sep=""),col='red',add=T)
        wait_next_graph(wait)
      }
    }
  }
  if (type == "daily"){
    for(i in 1:x$nday){
      plot(x$circlepatches[[i]]$current,main=paste("Day ",i,sep=""),col='red')
      wait_next_graph(wait)
    }
  }
}

#' Summary of \code{circlepatch} class
#' @rdname exploration
#'
#'
summary.exploration <- function(x){
  df <- plyr::ldply(x$circlepatches,function(y){
    return(plyr::ldply(y,function(z){
      summary(z)$areas
      }) %>% dplyr::rename(type=.id) )
  }) %>% dplyr::rename(day=.id)
  df.melt <- reshape2::melt(df,measure.vars=c("ExploredPercent","Explored"))
  df <- reshape2::dcast(df.melt,day+distance~type+variable)
  df <- dplyr::mutate(df,day=as.numeric(day))
  df.explo <- list("nday"=x$nday,
                   "buffer"=x$buffer,
                   "binsize"=x$binsize,
                   "maxdist"=x$maxdist,
                   "areas"=df)
  class(df.explo) <- "summary.exploration"
  return(df.explo)
}

#' Plot method for \code{summary.exploration} object
#'
#' Used to build its summary
#' @import ggplot2
#' @param x a \code{summary.exploration} object

# x <- summary(test.explo)

plot.summary.circlepatch <- function(x,xlab="Distance to waterhole",ylab="Explored Area",ncol=2,current=T,marginal=T,total=T,wait=T){
  if(current){
    ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=current_Explored),stat='identity') + xlab(xlab)+ylab(ylab)+facet_wrap(~day)
    wait_next_graph(wait)
    ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=current_ExploredPercent),stat='identity')+ xlab(xlab)+ylab(paste(ylab," (Percent)"))+facet_wrap(~day)
    wait_next_graph(wait)
  }

}
