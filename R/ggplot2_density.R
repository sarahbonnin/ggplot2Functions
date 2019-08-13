#### Density plots ####

#' Density plot from a data frame using ggplot2
#'
#' @param data (compulsory) a data frame in which each column is a sample.
#' @param title (optional) main title of the plot and the name of the file. Default: time stamp.
#'
#' @return a PDF plot that contains one page per column in data.
#' @export
#' @import ggplot2
#'
#' @examples
#' dat <- data.frame(A=rnorm(200), B=rnorm(200))
#' ggplot2_density(dat, title="test")

ggplot2_density <- function(data, title){

  # check if ggplot2 is installed; if not, install it
  #list.of.packages <- c("ggplot2", "cowplots")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages)

  # load ggplot2 and cowplots
  #require(ggplot2); require(cowplots)

  # check if the data argument was passed: no input!
  if (missing(data))
    stop("Need to input a data argument (matrix or data frame)")

  # title defaults to time stamp
  if(missing(title)) title <- Sys.Date()

  # function to produce density plot
  dens_func <- function(data2, sample){
    # select sample to be plotted
    mysample <- data2[,sample]
    # get maximum x coordinate at peak of density plot (we will add a vertical line to the plot):
    maxdens <- which.max(density(mysample)$y)
    xmaxdens <- density(mysample)$x[maxdens]

    ggplot(data2, aes_string(sample)) +
      geom_density() +
      geom_vline(xintercept=xmaxdens, col="red") +
      annotate(geom="text", x=xmaxdens+0.1, y=median(density(mysample)$y), label=round(xmaxdens, 2), col="red") + ggtitle(paste("density plot for", sample,  title))
  }

  # save plot in pdf format
  pdf(paste0("Density_", gsub(" ", "_", title), ".pdf"), height=7, width=9)
  for(i in 1:ncol(data)){
    print(dens_func(data, colnames(data)[i]))
  }
  dev.off()


}
