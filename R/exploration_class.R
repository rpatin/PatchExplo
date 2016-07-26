#' exploration class description
#'
#' @param x a \code{exploration} object generated for instance by
#'   \code{\link{explore}}
#' @name exploration
NULL

#' Print object of \code{exploration} class
#'
#' @rdname exploration
#' @export

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


#' Plot object of \code{exploration} class
#' @rdname exploration
#' @export

#  x <- test.explo
plot.exploration <- function(x, type = "marginal",wait=T,title=NULL,waterholes=NULL,buffer=NULL){
  if (type == "marginal"){
    plot(x$circlepatches[[1]]$total,main=paste(title," Night 1",sep=""),col='red',border=rgb(1,1,1,alpha=1))
    plot_waterholes(x=waterholes,buffer=buffer)
    wait_next_graph(wait)
    if (x$nday >1){
      for(i in 2:x$nday){
        plot(x$circlepatches[[i-1]]$total,main=paste(title," Night ",i,sep=""),col='grey50',border=rgb(1,1,1,alpha=1))
        plot(x$circlepatches[[i]]$marginal,main=paste(title," Night ",i,sep=""),col='red',border=rgb(1,1,1,alpha=1),add=T)
        plot_waterholes(x=waterholes,buffer=buffer)
        wait_next_graph(wait)
      }
    }
  }
  if (type == "daily"){
    for(i in 1:x$nday){
      plot(x$circlepatches[[i]]$current,main=paste(title," Night ",i,sep=""),col='red',border='white')
      plot_waterholes(x=waterholes,buffer=buffer)
      wait_next_graph(wait)
    }
  }
}

#' Summary of \code{exploration} class
#' @rdname exploration
#'
#'@importFrom magrittr %>%
#' @export
# x <- test.explo

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

#' Print method for \code{summary.exploration} object
#'
#' Print output
#' @param x a \code{summary.exploration} object
#' @export

# x <- summary(test.explo)
print.summary.exploration <- function(x){
  str(x,max.level = 1)
}

#' Plot method for \code{summary.exploration} object
#'
#' Used to build its summary
#' @import ggplot2
#' @param x a \code{summary.exploration} object
#'@importFrom magrittr %>%
#' @export
plot.summary.exploration <- function(x,xlab="Distance to waterhole",ylab="Explored Area",ncol=2,current=T,marginal=T,wait=T,title=NULL){
  if(current){
    g <- ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=current_Explored),stat='identity',width = x$binsize) + xlab(xlab)+ylab(ylab)+facet_wrap(~day)+ggtitle(title)
    gridExtra::grid.arrange(g)
    wait_next_graph(wait)
    g <-ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=current_ExploredPercent),stat='identity',width = x$binsize)+ xlab(xlab)+ylab(paste(ylab," (Percent)"))+facet_wrap(~day)+ggtitle(title)
    gridExtra::grid.arrange(g)
    wait_next_graph(wait)
  }
  if(marginal){
    g <-ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=marginal_Explored,fill=factor(day)),stat='identity',width = x$binsize) + xlab(xlab)+ylab(ylab)+ggtitle(title)
    gridExtra::grid.arrange(g)
    wait_next_graph(wait)
    g <-ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=marginal_ExploredPercent,fill=factor(day)),stat='identity',width = x$binsize)+ xlab(xlab)+ylab(paste(ylab," (Percent)"))+ggtitle(title)
    gridExtra::grid.arrange(g)
  }
}
