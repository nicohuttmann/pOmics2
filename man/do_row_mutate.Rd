% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_data_manipulation.R
\name{do_row_mutate}
\alias{do_row_mutate}
\title{dplyr mutate function for list_ objects}
\usage{
do_row_mutate(
  data_,
  texpr,
  calc_with,
  group_by = "All",
  ...,
  input.name,
  output.name = "_rowmutate"
)
}
\arguments{
\item{data_}{list or tibble}

\item{texpr}{tidy expression to to use for a new column}

\item{calc_with}{}

\item{group_by}{}

\item{...}{additional arguments for the mutate function}

\item{input.name}{name of input data}

\item{output.name}{name of output data}
}
\description{
dplyr mutate function for list_ objects
}
