% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_local_data.R
\name{pull_local_data}
\alias{pull_local_data}
\title{Pull tables from local tag datafiles}
\usage{
pull_local_data(path2data, cids = NULL, tag_mfr)
}
\arguments{
\item{path2data}{path to local datafile(s)}

\item{cids}{SMRU campaign ids. If not specified then the cids are built from
the directory or filenames present in the \code{path2data} directory. Ignored if
\code{tag_mfr = "wc"}.}

\item{tag_mfr}{either "smru" or "wc"}
}
\description{
reads SMRU or WC tag datafiles & combines in a unified list
}
