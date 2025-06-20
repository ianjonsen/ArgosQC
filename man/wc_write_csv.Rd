% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wc_write_csv.R
\name{wc_write_csv}
\alias{wc_write_csv}
\title{write annotated WC tag datafiles & annotated metadata}
\usage{
wc_write_csv(
  wc_ssm,
  fit,
  what,
  meta,
  program = "atn",
  path = NULL,
  dropIDs = NULL,
  suffix = "_nrt",
  pred.int = 6
)
}
\arguments{
\item{wc_ssm}{SSM-appended WC tag datafiles - output of \code{append_wc_ssm}}

\item{fit}{final SSM fit object}

\item{what}{specify whether predicted or rerouted locations are to be used}

\item{meta}{metadata}

\item{program}{Determines structure of output metadata. Currently, either \code{imos} or \code{atn}.}

\item{path}{path to write .csv files}

\item{dropIDs}{individual WC DeploymentID's to be dropped}

\item{suffix}{suffix to add to .csv files (_nrt, _dm, or _hist)}

\item{pred.int}{prediction interval to use for sub-sampling predicted locations
(default = 6 h)}
}
\description{
subsample SSM-predicted locations to 6-h intervals, write annotated files to .csv
}
