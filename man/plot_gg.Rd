% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_plot_functions.R
\name{plot_gg}
\alias{plot_gg}
\title{Evaluates data cell-wise}
\usage{
plot_gg(
  data_,
  mapping = aes(),
  ...,
  pivot.longer = T,
  input.name,
  output.name = "_plot_gg"
)
}
\arguments{
\item{data_}{data list}

\item{mapping}{define the variable mapping with the aes() function}

\item{...}{ggplot functions such as geom_s, themes, etc.}

\item{pivot.longer}{should the data be transformed to long format}

\item{input.name}{if data_ is list: name of data to use}

\item{output.name}{if data_ is list: name of output data to save in list 
under}
}
\description{
Evaluates data cell-wise
}
