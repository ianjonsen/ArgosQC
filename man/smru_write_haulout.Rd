% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smru_write_csv.R
\name{smru_write_haulout}
\alias{smru_write_haulout}
\title{SMRU haulout table tests & write to .csv}
\usage{
smru_write_haulout(
  smru_ssm,
  meta,
  program = "imos",
  test = TRUE,
  path = NULL,
  dropIDs = NULL,
  suffix = "_nrt"
)
}
\arguments{
\item{smru_ssm}{SSM-appended SMRU table file - output of \code{append_ssm}}

\item{meta}{metadata}

\item{program}{Determines structure of output metadata. Currently, either \code{imos} or \code{atn}.}

\item{test}{should variables be tested for standards compliance, default is TRUE.
Standards compliance is specific to the program. Currently, only program = \code{imos}
has defined variable standard against which output compliance is tested.}

\item{path}{path to write .csv files}

\item{dropIDs}{individual ids to be dropped}

\item{suffix}{suffix to add to .csv files (_nrt, _dm, or _hist)}
}
\description{
Apply AODN tests to SMRU haulout table, write to .csv - format depends on program (IMOS, ATN)
}
\keyword{internal}
