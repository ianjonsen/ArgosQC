% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_filter.R
\name{multi_filter}
\alias{multi_filter}
\title{multi-filter}
\usage{
multi_filter(
  x,
  vmax = 4,
  ang = c(15, 25),
  distlim = c(2500, 5000),
  min.dt = 60,
  model = "crw",
  ts = 2,
  map = NULL,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{\code{sf}-projected diag file of locations to be filtered}

\item{vmax}{for prefilter}

\item{ang}{for prefilter}

\item{distlim}{for prefilter}

\item{min.dt}{for prefilter}

\item{model}{\code{aniMotum} model ("rw" orr "crw)}

\item{ts}{\code{foieGrsa} time.step}

\item{map}{params to fix}

\item{verbose}{turn on/off furrr::future_map progress indicator}

\item{...}{additional arguments to be passed to \code{aniMotum::fit_ssm}}
}
\description{
apply SSM filter to diag data across multiple processors
}
