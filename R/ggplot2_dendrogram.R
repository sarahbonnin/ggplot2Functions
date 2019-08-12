#### Dendrogram / hierarchical clustering ####

#' Hierarchical clustering from a matrix using ggplot2 and ggdendro
#'
#' @param data (compulsory) either a matrix, a DESeqTransform or a DESeqDataSet object.
#' @param title (optional) main title of the plot and the name of the file. Default: time stamp.
#' @param samples (optional) vector containing the sample names. Default: column names of data.
#' @param expGroups (optional) vector containing the name of the experimental expGroups (same order as in the columns of data). no default
#'
#' @return a PDF file.
#' @export
#' @import DESeq2 ggplot2 ggdendro
#'
#' @examples
#' dat <- matrix(rnorm(1200), ncol=6)
#' ggplot2_dendrogram(dat, samples=1:6, expGroups=rep(c("A", "B"), 3), title="test")

ggplot2_dendrogram <- function(data, title, samples, expGroups){

  # check if ggplot2 is installed; if not, install it
  # list.of.packages <- c("ggplot2", "cowplots", "ggdendro", "DESeq2")
  # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # if(length(new.packages)) install.packages(new.packages)

  # load ggplot2 and cowplots
  # require(ggplot2); require(cowplots); require(ggdendro); require("DESeq2")

  # check if the data argument was passed: no input!
  if (missing(data))
    stop("Need to input a data argument (matrix or data frame)")

  # title defaults to time stamp
  if(missing(title)) title <- Sys.Date()

  # if data has no column name, defaults to the n first letters of the alphabets
  if(is.null(colnames(data))) colnames(data) <- LETTERS[1:ncol(data)]
  # defaults will be the column names of the matrix
  if(missing(samples)) samples <- colnames(data)

  # if groups is not passed, set to NA: the plot will be modified accordingly.
  #if(missing(groups)) groups <- NA

  # test if data is a matrix or a DESeq2 object; adjust accordingly
  if(is(data, "DESeqTransform")) data <- SummarizedExperiment::assay(data)
  if(is(data, "DESeqDataSet")) data <- SummarizedExperiment::assay(rlog(data))

  # dendrogram object
  dd.row <- stats::as.dendrogram(stats::hclust(stats::dist(t(data))))
  ddata_x <- ggdendro::dendro_data(dd.row)

  # set y axis limits
  ymaxdendro <- max(ddata_x$segments$y)
  ylimits <- c(0-ymaxdendro/5, ymaxdendro)

  # add columns for samples and groups (if exists)
  if(any(!is.na(expGroups))){
    df_colnames <- data.frame(col_names=colnames(data), samples=samples, groups=expGroups)
  }else{
    df_colnames <- data.frame(col_names=colnames(data), samples=samples)
  }

  labs <- label(ddata_x)
  labs <- merge(labs, df_colnames, by.x="label", by.y="col_names", all=T, sort=FALSE)

  p2 <- ggplot2::ggplot(segment(ddata_x)) +
    ggplot2::geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(ylimits) +
    ggplot2::ggtitle(paste("Hierarchical clustering / dendrogram", title))

  if(any(!is.na(expGroups))){
    p3 <- p2 + ggplot2::geom_text(data=label(ddata_x),
                         aes(label=labs$label, x=x, y=0, colour=labs$groups), angle=90, hjust=1) + scale_colour_discrete(name="Experimental\nGroup")
  }else{
    p3 <- p2 + ggplot2::geom_text(data=label(ddata_x),
                         aes(label=labs$label, x=x, y=0), angle=90, hjust=1)
  }

  # adapt size of file according to number of samples
  pdf_width <- ifelse(ncol(data) <= 10, 7, ifelse(ncol(data) <= 30, 9, ifelse(ncol(data) <= 50, 12, ifelse(ncol(data) <= 80, 14, 20))))

  # save plot in pdf format
    grDevices::pdf(paste0("Dendrogram_", gsub(" ", "_", title), ".pdf"), height=7, width=pdf_width)
    print(p3)
    grDevices::dev.off()

}
