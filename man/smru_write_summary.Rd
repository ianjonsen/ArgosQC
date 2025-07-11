% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smru_write_csv.R
\name{smru_write_summary}
\alias{smru_write_summary}
\title{SMRU summary table tests & write to .csv}
\usage{
smru_write_summary(
  smru_ssm,
  meta,
  program = "imos",
  path = NULL,
  dropIDs = NULL,
  suffix = "_nrt"
)
}
\arguments{
\item{smru_ssm}{SSM-appended SMRU table file - output of \code{append_ssm}}

\item{meta}{metadata}

\item{program}{Determines structure of output metadata. Currently, either \code{imos} or \code{atn}.}

\item{path}{path to write .csv files}

\item{dropIDs}{individual ids to be dropped}

\item{suffix}{suffix to add to .csv files (_nrt, _dm, or _hist)}
}
\description{
Apply AODN tests to SMRU summary table, write to .csv - format depends on program (IMOS, ATN)
}
\keyword{internal}
