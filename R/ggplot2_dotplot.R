#### Dot plot per counts / genes ####

#' Title
#'
#' @param data (compulsory) matrix: each column is a sample. Row names should be gene names.
#' @param genes (compulsory) vector of gene names that you want to create dot plots for. They should be found in data's row names.
#' @param expGroups (optional) vector containing the name of the experimental expGroups (same order as in the columns of data). no default
#' @param samples (optional) vector containing the sample names. Default: column names of data.
#' @param title (optional) main title of the plot and the name of the file. Default: time stamp.
#'
#' @return a PDF file
#' @export
#' @import ggplot2 reshape2
#'
#' @examples
#' dat <- matrix(rnorm(6), ncol=2, dimnames=list(c("A", "B", "C"), 1:2))
#' ggplot2_dotplot(dat, genes=c("A", "C"), samples=c("sample1", "sample2"))

ggplot2_dotplot <- function(data, genes, expGroups, samples, title){

  # check if ggplot2 is installed; if not, install it
  #list.of.packages <- c("ggplot2", "cowplots", "ggdendro", "DESeq2", "reshape2")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages)

  # load ggplot2 and cowplots
  #require(ggplot2); require(cowplots); require(ggdendro); require("DESeq2"); require("reshape2")

  # check if the data argument was passed: no input!
  if (missing(data))
    stop("Need to input a data argument (matrix or data frame)")

  # title defaults to time stamp
  if(missing(title)) title <- Sys.Date()

  # if data has no column name, defaults to the n first letters of the alphabets
  if(is.null(colnames(data))) colnames(data) <- LETTERS[1:ncol(data)]
  # defaults will be the column names of the matrix
  if(missing(samples)) samples <- colnames(data)
  colnames(data) <- samples

  # if groups is not passed, set to NA: the plot will be modified accordingly.
  if(missing(expGroups)) expGroups <- rep("all", ncol(data))

  # test if data is a matrix or a DESeq2 object; adjust accordingly
  if(is(data, "DESeqTransform")) data <- assay(data)
  if(is(data, "DESeqDataSet")) data <- assay(rlog(data))

  # subset data with only genes of interest (row names)
  sub_data <- data[rownames(data) %in% genes,]

  # wide to long format: "Var1": gene name; "Var2": sample/column name; "value": count
  sub_lg0 <- melt(sub_data)

  # add group and sample names
  sub_lg <- merge(data.frame(sample=samples, group=expGroups), sub_lg0, by.x="sample", by.y="Var2", all=F)

  # function to produce dot plots: one plot per gene
  dot_func <- function(long_data, gene){
    # select sample to be plotted
    mygene <- long_data[long_data$Var1 == gene,]

    ggplot(mygene, aes(x=group, y=value, label=sample, color=group)) +
      geom_point() +
      xlab("Group(s)") +
      ylab("counts") +
      ggtitle(paste("Counts of", gene, "in", title)) +
      theme_bw() +
      geom_text(aes(label=sample),hjust=-0.1, vjust=0)
  }


  # save plot in pdf format
  grDevices::pdf(paste0("Dotplots_", gsub(" ", "_", title), ".pdf"), height=7, width=9)
  for(i in genes){
    print(dot_func(sub_lg, i))
  }
  grDevices::dev.off()

}

