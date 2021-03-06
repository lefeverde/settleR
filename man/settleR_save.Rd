% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SettleR-methods.R
\name{settleR_save}
\alias{settleR_save}
\title{wrapper to save the upset plot created by settleR_plot}
\usage{
settleR_save(settleRObject, fn)
}
\arguments{
\item{settleRObject}{\link[settleR]{SettleR} object created by the constructor function.}

\item{fn}{file name}
}
\value{
saved settleR plot
}
\description{
Essentially, this is just a wrapper around ggsave. The difference is that
plot size is automatically calculated.
}
