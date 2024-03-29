% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_helpers.R
\name{cowplot_merge_upset_list}
\alias{cowplot_merge_upset_list}
\title{Merges list of ggplot upset into gtable object using cowplot}
\usage{
cowplot_merge_upset_list(plt_list, margSize = 5)
}
\arguments{
\item{plt_list}{list of ggplots created from \link[settleR]{make_upset_plots} or similiarly structured}

\item{margSize}{sets height or width of plots in margin size in cm}
}
\value{
a merged settleR plot
}
\description{
I'm leaving this function as is, but it is deprioritized over the
patchwork based merging.
}
\keyword{internal}
