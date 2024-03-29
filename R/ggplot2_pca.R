#' Principal Component Analysis.
#'
#' @param data (compulsory) either a matrix, a DESeqTransform or a DESeqDataSet object.
#' @param title (optional) main title of the plot and the name of the file. Default: time stamp.
#' @param first_pc (optional) component plotted on the x-axis. Default: 1.
#' @param second_pc (optional) component plotted on the y-axis. Default: 2.
#' @param samples (optional) vector containing the sample names. Default: column names of data.
#' @param expGroups (optional) vector containing the name of the experimental groups (same order as in the columns of data). no default
#'
#' @return a PDF file
#' @export
#' @import DESeq2 ggplot2
#'
#' @examples
#' dat <- matrix(rnorm(1200), ncol=6)
#' ggplot2_pca(dat, first_pc=1, second_pc=3, samples=1:6, expGroups=rep(c("A", "B"), 3), title="test")

ggplot2_pca <- function(data, title, first_pc, second_pc, samples, expGroups){

  # check if ggplot2 is installed; if not, install it
  #list.of.packages <- c("ggplot2", "DESeq2")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages)

  # load ggplot2 and cowplots
  #library(ggplot2); require("DESeq2")

  # check if the data argument was passed: no input!
  if (missing(data))
    stop("Need to input a data argument (matrix)")

  # test if data is a matrix or a DESeq2 object; adjust accordingly
  if(methods::is(data, "DESeqTransform")) data <- SummarizedExperiment::assay(data)
  if(methods::is(data, "DESeqDataSet")) data <- SummarizedExperiment::assay(rlog(data))

  # Performs principal component analysis on data
  pca <- stats::prcomp(t(data))
  # Retrieve the percentages of variation for each component
  percentVar <- pca$sdev^2 / sum( pca$sdev^2 )

  # check if optional arguments are missing: set defaults if they are

  # title defaults to time stamp
  if(missing(title)) title <- Sys.Date()

  # principal components plotted default to 1 and 2
  if(missing(first_pc) | missing(second_pc)){
    first_pc <- 1
    second_pc <- 2
  }
  # defaults will be the column names of the matrix
  if(missing(samples)) samples <- colnames(data)

  # if groups is not passed, set to NA: the plot will be modified accordingly.
  if(missing(expGroups)) expGroups <- NA

  # Create data frame with principal components (elected or default), sample and group information; Create fist plot layer with or without "groups"

  if(any(is.na(expGroups))){
    d <- data.frame(PC1=pca$x[,first_pc], PC2=pca$x[,second_pc], sample=samples)
    p <- ggplot2::ggplot(data=d, aes(x=PC1, y=PC2, label=sample))
  }else{
    d <- data.frame(PC1=pca$x[,first_pc], PC2=pca$x[,second_pc], group=expGroups, sample=samples)
    p <- ggplot2::ggplot(data=d, aes(x=PC1, y=PC2, color=group, label=sample))
  }

  # set x limits to allow space for the labels
  xlimits <- c(min(d$PC1)+0.2*min(d$PC1), max(d$PC1)+0.2*max(d$PC1))

  # plot
  p2 <- p + ggplot2::geom_point(size=3) +
    ggplot2::theme_bw() +
    ggplot2::xlab(paste0("PC:",first_pc," ", round(percentVar[first_pc] * 100),"% variance")) +
    ggplot2::ylab(paste0("PC:",second_pc," ", round(percentVar[second_pc] * 100),"% variance")) +
    ggplot2::ggtitle(paste("PCA", title)) +
    ggplot2::geom_text(size=5, hjust=-0.1) +
    ggplot2::xlim(xlimits)

  # save plot in pdf format
  grDevices::pdf(paste0("PCA_", gsub(" ", "_", title), "_PC",first_pc, "_PC", second_pc, ".pdf"), height=7, width=9)
  print(p2)
  grDevices::dev.off()
}
