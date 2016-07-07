#' circlepatch class description
#'
#' @param x a \code{circlepatch} object generated for instance by
#'   \code{\link{generate_circle_patch}}
#' @name circlepatch
NULL

#' Print object of \code{circlepatch} class
#'
#' @rdname circlepatch

# x <- BeginPatch

print.circlepatch <- function(x){
  listdist <- as.numeric(names(x))
  maxdist <- max(listdist)
  binsize <- unique(diff(listdist))
  if(length(binsize)>1) stop("Object corrupted, binsize not constant.")
  nbins <- length(listdist)
  cat(paste("circlepatch object",
            "\n Bin size : ", binsize,
            "\n Maximum distance : ", maxdist,
            "\n Number of bins : ",nbins,sep=""))
}


#' Plot object of \code{circlepatch} class
#' @rdname circlepatch

plot.circlepatch <- function(x,border='black',col='red',background=rgb(1,1,1,alpha=1) ,add=F,main=NULL){
  listdist <- as.numeric(names(x))
  maxdist <- max(listdist)
  binsize <- unique(diff(listdist))
  if(length(binsize)>1) stop("Object corrupted, binsize not constant.")
  nbins <- length(listdist)

  plot(x[[nbins]]$UnExplored,col=background,border=border,add=add,main=main)

  if (class(x[[nbins]]$Explored) != 'logical'){
    plot(x[[nbins]]$Explored,add=T,col=col,border=border)
    }
  for (i in 1:(nbins-1)){
    plot(x[[i]]$UnExplored,add=T,col=background,border=border)
    if (class(x[[i]]$Explored) != 'logical') {
      plot(x[[i]]$Explored,add=T,col=col,border=border)
    }
  }
}

#' Summary method for \code{circlepatch} object
#'@rdname circlepatch
#'@import magrittr `%>%`

summary.circlepatch <- function(x){
  listdist <- as.numeric(names(x))
  maxdist <- max(listdist)
  binsize <- unique(diff(listdist))
  if(length(binsize)>1) stop("Object corrupted, binsize not constant.")
  nbins <- length(listdist)
  tmp <- plyr::ldply(x,zone2data) %>% dplyr::rename(distance=.id)
  tmp.summary <- list("maxdist"=maxdist,
                      "binsize"=binsize,
                      "nbins"=nbins,
                      "areas"=tmp)
  class(tmp.summary) <- "summary.circlepatch"
  return(tmp.summary)
}

# Beg.summary <- summary.circlepatch(BeginPatch)
# plot(Beg.summary)

#' Plot method for \code{summary.circlepatch} object
#'
#' Used to build its summary
#' @import ggplot2
#' @param x a \code{summary.circlepatch} object


plot.summary.circlepatch <- function(x,xlab="Distance to waterhole",ylab="Explored Area",ncol=2){
  g1 <- ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=Explored),stat='identity') + xlab(xlab)+ylab(ylab)
  g2 <- ggplot(x[["areas"]]) + geom_bar(aes(x=as.numeric(distance),y=ExploredPercent),stat='identity')+ xlab(xlab)+ylab(paste(ylab," (Percent)"))
  gridExtra::grid.arrange(g1,g2,ncol=ncol)
}
