#' Startup functions for pOmics2 package
#'
#' @param libname libname
#' @param pkgname pkgname
#'
#'
#' 
.onAttach <- function(libname, pkgname) {
  #packageStartupMessage("Hi, thanks for using pOmics2.")
  options(pOmics2_list_long_names = F, 
          pOmics2_dataset_as_index = F)
}