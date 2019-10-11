% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_smru_tables.R
\name{pull_smru_tables}
\alias{pull_smru_tables}
\title{Pull tables from SMRU .mdb files}
\usage{
pull_smru_tables(cids, path2mdb, tables = c("diag", "haulout", "ctd",
  "dive", "summary"))
}
\arguments{
\item{cids}{SMRU campaign ids}

\item{path2mdb}{path to SMRU .mdb file(s)}

\item{tables}{specify which tables to extract, default is to extract all tables}
}
\description{
extracts specified tables from SMRU .mdb files, using Hmisc::mdb.get
}
\examples{

}