% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_org_databases.R
\name{.mapping2output}
\alias{.mapping2output}
\title{Formats mapping from select methods}
\usage{
.mapping2output(
  mapping,
  keys,
  database,
  paste = NULL,
  rm.na = F,
  fill.na = F,
  to.vector = F,
  as.T2G = F,
  ...
)
}
\arguments{
\item{mapping}{}

\item{keys}{keys}

\item{database}{database of origin ("org" or "UniProt")}

\item{paste}{}

\item{rm.na}{}

\item{fill.na}{}

\item{to.vector}{}

\item{as.T2G}{}

\item{...}{arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")}
}
\description{
Formats mapping from select methods
}
