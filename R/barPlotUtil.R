require(ggplot2)
require(ggthemes)

##' Compute labels for percentages of a vector
##'
##' Generate labels for percentages that are greater than the minimal threshold.
##' @title 
##' @param x the percentage vector
##' @param threshold the minimal threshold to label a percentage
##' @param digits the number of digits of the percentage labels 
##' @return a vector of labels (character)
##' @author Arnaud Malapert
GetPercentLabels <- function(x, threshold = 1, digits = 1) {
  ind <- x >= threshold
  r <- rep("", length(x))
  r[ind] <- sprintf(paste0("%.", digits, "f%%"), x[ind])
  return(r)
}



##' Generate a simple bar chart computing the percentages of all values in the vector `x`.
##'
##' The bar chart has x-axis labels if if their percentages are greater than the minimal threshold.
##' @title 
##' @param x the vector
##' @param threshold the minimal threshold to label a bar 
##' @param digits the number of digits of the percentage labels
##' @return a ggplot object
##' @references \url{http://rstudio-pubs-static.s3.amazonaws.com/4305_8df3611f69fa48c2ba6bbca9a8367895.html}, \url{http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees}
##' @author Arnaud Malapert
BarPlotRaw <- function(x, threshold = 1, digits = 0) {
  x <- as.data.frame(table(x[drop=TRUE], useNA = "ifany"))
  label <- GetPercentLabels(100*x$Freq/sum(x$Freq), threshold = threshold, digits = digits)
  pos <- x$Freq / 2
  ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", position = "dodge", fill = ptol_pal()(1)) +
    geom_text(aes(y = pos, label=label), color = "grey80", size=7, fontface = 2) +
    coord_flip() + theme_gdocs() 
}

##' Generate a stacked bar plot computing the percentages of all pairs in the columns `aesX` and `aesY` of the data `df`.
##'
##' The stacked bar chart has optional labels for the x-axis and fill categories.
##' @title 
##' @param df the data with columns aesX and aesF
##' @param aesX x-axis of the plot 
##' @param aesF categories of the stacked bars
##' @param legend.title title of the legend
##' @param labelX `TRUE` indicates that each stacked bar is labeled 
##' @param labelF `TRUE` indicates that each bar is labeled 
##' @return a ggplot object
##' @references \url{http://stackoverflow.com/questions/21236229/stacked-bar-chart}
##' @author Arnaud Malapert
BarStackedPlotRaw <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  x <- as.data.frame(ftable(droplevels(df[ , c(aesX, aesF)]), exclude= NaN))
  totFreq <- sum(x$Freq)
  ## Compute the percentage labels
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, threshold = 3, digits = 0)
  x <- ddply(x, aesX, transform, pos = sum(Freq)-cumsum(Freq) + (0.5 * Freq), top = cumsum(Freq))
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq, threshold = 1, digits = 0)
  m <- length(unique(x[,aesF]))
  ## exploit recycling
  x$toplab[ append(rep(TRUE,m-1), FALSE) ] <- ""
  ## browser()
  p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") + coord_flip() + theme_gdocs()
  ## Labels in the bar cells
  if(labelF) {
    p <- p + geom_text(aes(y = x$pos, label = x$percentage), size = 5, show.legend = FALSE)
  }
  ## Labels of the bar
  if(labelX) {
    vjust <- ifelse( length(levels(x[,aesX])) < 7, -0.5, 0.5)
    p <- p + geom_text(aes(y = x$top, label = x$toplab), size = 7, hjust = -0.25, vjust = vjust, fontface = 2, show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.1)))
  }
  ## Set title and legend
  if(is.null(legend.title)) {
    p <- p + scale_fill_ptol( na.value = "grey80") 
  } else {
    p <- p + scale_fill_ptol(name=legend.title, na.value = "grey80") 
  }
  p <- p + theme(legend.position="bottom", legend.direction="horizontal")
  return(p)
}
