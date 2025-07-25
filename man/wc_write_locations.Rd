% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wc_write_csv.R
\name{wc_write_locations}
\alias{wc_write_locations}
\title{WC Locations datafile write to .csv}
\usage{
wc_write_locations(
  wc_ssm,
  meta,
  program = "atn",
  path = NULL,
  dropIDs = NULL,
  suffix = "_nrt"
)
}
\arguments{
\item{wc_ssm}{SSM-appended WC tag datafile - output of \code{append_ssm}}

\item{meta}{metadata}

\item{program}{Determines structure of output metadata. Currently, only \code{atn}.}

\item{path}{path to write .csv files}

\item{dropIDs}{individual ids to be dropped}

\item{suffix}{suffix to add to .csv files (_nrt, _dm, or _hist)}
}
\description{
Write WC Locations datafile to .csv - format specific to ATN program
}
\keyword{internal}
