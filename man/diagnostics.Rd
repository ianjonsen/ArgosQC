% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{diagnostics}
\alias{diagnostics}
\title{run QC diagnostics for WC or SMRU tag workflows}
\usage{
diagnostics(
  fit,
  fit1,
  what = "p",
  cut,
  data,
  ssm,
  meta,
  lines = FALSE,
  obs = FALSE,
  mpath = NULL,
  dpath = NULL,
  QCmode = "nrt",
  cid
)
}
\arguments{
\item{fit}{the final aniMotum fit object from QC process}

\item{fit1}{the initial aniMotum fit object from QC process}

\item{what}{the SSM-estimated or rerouted locations to be used}

\item{cut}{logical; should predicted locations be dropped if keep = FALSE - ie. in a large data gap}

\item{data}{the standardized WC Locations or SMRU diag file (prior to truncation by metadata CTD start and end dates)}

\item{ssm}{the ssm-annotated WC/SMRU tables}

\item{meta}{metadata}

\item{lines}{add track lines to map (default = FALSE)}

\item{obs}{add observed locations to map (default = FALSE)}

\item{mpath}{path to write map file}

\item{dpath}{path to write all other diagnostic files}

\item{QCmode}{specify whether QC is near real-time (nrt) or delayed-mode (dm),
in latter case start end end of dive data are displayed rather than ctd data}

\item{cid}{SMRU campaign id (from config file)}
}
\description{
produces a map of all QC'd tracks and generates various diagnostics to assess QC run
}
