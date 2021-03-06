% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/food-demand-plots.R
\name{mc.make.byyear.plot}
\alias{mc.make.byyear.plot}
\title{Make the by-year plot for a set of monte carlo results by sampling the distribution}
\usage{
mc.make.byyear.plot(
  mc.data,
  obsdata,
  bias.correction = NULL,
  region = NULL,
  nsamp = 30,
  pltrgn = NULL
)
}
\arguments{
\item{mc.data}{Monte Carlo results data}

\item{obsdata}{Data frame of observed food demand data}

\item{bias.correction}{Regional bias correction factors (default = none)}

\item{region}{Vector of regions to plot (default = all)}

\item{nsamp}{Number of samples to draw from the Monte Carlo distribution}

\item{pltrgn}{Regions to include in the plot.  If \code{NULL}, include them
all.}
}
\value{
by-year plot for a set of monte carlo results by sampling the distribution
}
\description{
Make the by-year plot for a set of monte carlo results by sampling the distribution
}
