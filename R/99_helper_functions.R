#' Returns column names from data frame of specific column class
#'
#' @param data data frame
#' @param data.class data type/s
#'
#' @return
#' @export
#'
.colnames_class <- function(data, data.class = "numeric") {
  
  col.class <- unlist(lapply(data, class))
  
  col.names <- names(.in_(col.class, data.class))
  
  return(col.names)
  
}


#' Intersects x and y, but keeps names from x
#'
#' @param x vector 1
#' @param y vector 2
#'
#' @return
#' @export
#'
#'
.in_ <- function(x, y) {
  
  return(x[x %in% y])
  
}
