#### Principal component analysis ####

## PCA function with ggplot2, from any matrix ##
# data: compulsory; a matrix
# title: optional; will be used to construct the main title of the plot and the name of the file; defaults to time stamp
# first_pc, second_pc; optional: which components do you want to plot. first will go to the x axis, second will go to the y axis; defaults to 1 and 2, respectively.
# groups: optional; vector that contains the name of the experimental groups (same order as in the columns of data).
# samples: optional; vector that contains the name of the samples for labeling the points; defaults to the column names of data.
# run as:
# pca_ggplot(dat, first_pc=1, second_pc=3, samples=1:6, groups=c(rep("one", 3), rep("two", 3)), title="my first PCA")

pca_ggplot <- function(data, title, first_pc, second_pc, groups, samples){

  # check if ggplot2 is installed; if not, install it
  list.of.packages <- c("ggplot2", "cowplots", "DESeq2")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  # load ggplot2 and cowplots
  require(ggplot2); require(cowplots); require("DESeq2")

  # check if the data argument was passed: no input!
  if (missing(data))
    stop("Need to input a data argument (matrix)")

  # test if data is a matrix or a DESeq2 object; adjust accordingly
  if(is(data, "DESeqTransform")) data <- assay(data)
  if(is(data, "DESeqDataSet")) data <- assay(rlog(data))

  # Performs principal component analysis on data
  pca <- prcomp(t(data))
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
  if(missing(groups)) groups <- NA

  # Create data frame with principal components (elected or default), sample and group information; Create fist plot layer with or without "groups"

  if(any(is.na(groups))){
    d <- data.frame(PC1=pca$x[,first_pc], PC2=pca$x[,second_pc], sample=samples)
    p <- ggplot(data=d, aes(x=PC1, y=PC2, label=sample))
  }else{
    d <- data.frame(PC1=pca$x[,first_pc], PC2=pca$x[,second_pc], group=groups, sample=samples)
    p <- ggplot(data=d, aes(x=PC1, y=PC2, color=group, label=sample))
  }

  # set x limits to allow space for the labels
  xlimits <- c(min(d$PC1)+0.2*min(d$PC1), max(d$PC1)+0.2*max(d$PC1))

  # plot
  p2 <- p + geom_point(size=3) +
    theme_bw() +
    xlab(paste0("PC:",first_pc," ", round(percentVar[first_pc] * 100),"% variance")) +
    ylab(paste0("PC:",second_pc," ", round(percentVar[second_pc] * 100),"% variance")) +
    ggtitle(paste("PCA", title)) +
    geom_text(size=5, hjust=-0.1) +
    xlim(xlimits)

  # save plot in pdf format
  pdf(paste0("PCA_", gsub(" ", "_", title), "_PC",first_pc, "_PC", second_pc, ".pdf"), height=7, width=9)
  print(p2)
  dev.off()
}
