# Installation

```R
# Install devtools from CRAN

install.packages("devtools")
# then SettleR from GitHub
devtools::install_github("lefeverde/settleR")

```

# Overview

This package allows [ggplot2](https://ggplot2.tidyverse.org) based UpSet style figures to be easily created. In a nutshell, these are plots which allow for set intersections to be visualized intuitively. These work the same as Venn/Euler diagrams but are much easier to understand when the number of sets is >4.

![settler_plot](https://github.com/lefeverde/settleR/blob/master/vignettes/example_settleR_plot.pdf?raw=true)

I created this package because at the time, I routinely needed to compare many different lists of genes and found that UpSet style plots were readily interpretable to a diverse audience but found the original UpSetR package too limited. I therefore developed this package primarily for my own use case. Since I originally created this package, several others have created similar R packages [see here for a list and comparison of the other packages](https://github.com/krassowski/complex-upset). There are a lot of overlapping features between these packages. In comparison, I've made extensive use of the S4 class system for SettleR to robustly organize all of the data and plots. This allowed me to build  in customizability not available in other packages, while being convenient to use for one-off visualizations.



# Quick start

```R

# Loading the package and example data
library(settleR)
test_data <-
  system.file('extdata','test_data.RData',package = 'settleR')
load(test_data)

# The input data is a named list of vectors gene_setList passed to the constructor function
set_obj <- SettleR(gene_setList)
set_obj %>% settleR_plot()

```

For a more in depth tutorial, see [Introduction to Settler ](https://github.com/lefeverde/settleR/blob/master/doc/Introduction_to_settleR.pdf).


# SettleR explanation
![grid_explanation](settleR_explanation.pdf?raw=true)

For illustration, [here](grid_explanation) are 3 sets shown as Venn diagrams labelled 1-3, and subsets are labelled A-C. The grid shows the subset exclusive to a particular set or sets. For example, the subset A is the exclusive portion to set 1, B is exclusive to sets 1 & 2, and C is exclusive to sets 1-3. The advantage of this system over Venn diagrams is seen when >3 groups are being compared.
