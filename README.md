# Functions for plots useful in HTS data analysis, using [ggplot2](http://ggplot2.org/) R package


## What is done and what is pending

- [x] PCA from a matrix or a DESeq2 object
- [x] Density plot from a data frame or a matrix
- [x] Hierarchical clustering from a matrix or a DESeq2 object
- [x] Dotplot for counts / gene from a matrix or a DESeq2 object
- [ ] Heatmap from a matrix or a DESeq2 object

## Install package

```
library(devtools)
install_github("sarahbonnin/ggplot2Functions")
```

## Run functions

### Principal component analysis (function ggplot2_pca)

Example data set:

```
dat <- matrix(rnorm(1200), ncol=6)
```

Run Function:

```
ggplot2_pca(dat, first_pc=1, second_pc=3, samples=1:6, expGroups=rep(c("A", "B"), 3), title="test")
```

### Density plot (function ggplot2_density)

Example data set:

```
dat <- data.frame(A=rnorm(200), B=rnorm(200))
```

Run Function:

```
ggplot2_density(dat, title="test")
```

### Hierarchical clustering / dendrogram (function ggplot2_dendrogram)

Example data set:

```
dat <- matrix(rnorm(1200), ncol=6)
```

Run Function:

```
ggplot2_dendrogram(dat, samples=1:6, expGroups=rep(c("A", "B"), 3), title="test")
```

### Dot plots

Example data set:

```
dat <- matrix(rnorm(6), ncol=2, dimnames=list(c("A", "B", "C"), 1:2))
```

Run Function:

```
ggplot2_dotplot(dat, genes=c("A", "C"), samples=c("sample1", "sample2"))
```
