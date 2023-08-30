#' Transforms tibble to matrix
#'
#' @param tibble tibble
#' @param from.row.names row to use as new row names
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.tibble2matrix <- function(tibble, from.row.names, cols.keep) {
  
  # Check input
  if (!tibble::is_tibble(tibble)) stop("<tibble> is not a tibble.", 
                                       call. = FALSE)
  
  # Determine row.names
  if (!hasArg(from.row.names)) {
    from.row.names <- colnames(tibble)[1]
  }
    
  # Determine columns to keep
  if (!hasArg(cols.keep)) {
    # Make sure data type does not change
    class.k <- tibble %>%
      dplyr::select(-dplyr::any_of(from.row.names)) %>%
      sapply(class) %>%
      table() %>%
      sort(decreasing = T) %>%
      names() %>%
      dplyr::first()
    
    keep <- c(from.row.names,
              .colnames_class(data = tibble, data.class = class.k))
  }
  
  # Transform to tibble
  data <- tibble %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    tibble::column_to_rownames(var = from.row.names) %>%
    as.matrix()

  # Transform and return
  return(data)
  
}


#' Transforms tibble to data frame
#'
#' @param tibble tibble
#' @param from.row.names column to use as new row names
#'
#' @return
#' @export
#'
.tibble2data_frame <- function(tibble, from.row.names) {
  
  # Check input
  if (!tibble::is_tibble(tibble)) stop("<tibble> is not a tibble.", 
                                       call. = FALSE)
  
  # Determine row.names
  if (!hasArg(from.row.names)) {
    from.row.names <- colnames(tibble)[1]
  }
  
  # Transform to data frame
  data <-  tibble::column_to_rownames(tibble, var = from.row.names)
  
  # Transform and return
  return(data)
  
}


#' Transforms matrix to tibble and adds column for row names
#'
#' @param data matrix with row names
#' @param to.row.names name for row names vector
#'
#' @return
#' @export
#'
#'
.matrix2tibble <- function(data, to.row.names) {
  
  # Check input
  if (!is.matrix(data)) stop("<data> is not a matrix.", 
                             call. = FALSE)
  
  # Determine row.names
  if (!hasArg(to.row.names)) {
    row.names <- "rows"
  }
  
  # Transform to tibble
  data <- data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()
  
  # Transform and return
  return(data)
  
}


#' Transforms data frames to tibble and adds column for row names
#'
#' @param data data frame with row names
#' @param to.row.names name for row names vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.data_frame2tibble <- function(data, to.row.names) {
  
  # Determine row.names
  if (!hasArg(to.row.names)) {
    to.row.names <- "rows"
  } 
  
  # Transform data.frame
  data <- data %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()
  
  # Transform and return
  return(data)
  
}


#' Transforms any data type to a tibble
#'
#' @param data supported data types: matrix, data.frame
#' @param to.row.names name for row names vector
#'
#' @return
#' @export
#'
#'
.data2tibble <- function(data, to.row.names) {
  
  # Already a tibble
  if (tibble::is_tibble(data)) {
    return(data)
    
    # Matrix
  } else if (is.matrix(data)) {
    
    data <- .matrix2tibble(data, to.row.names)
    
    # Data frame
  } else if (is.data.frame(data)) {
    
    data <- .data_frame2tibble(data, to.row.names)
    
    # Other data types
  } else {
    stop("Data type not a data.frame or a matrix.", call. = FALSE)
  }
  
  # Return
  return(data)
  
}


#' Transposes tibble and uses first column as column names
#'
#' @param tibble tibble
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.transpose_tibble <- function(tibble,
                              from.row.names,
                              to.row.names) {
  
  # Identify from.row.names if not given
  if (!hasArg(from.row.names)) from.row.names <- colnames(tibble)[1]
  
  # Use rows as new column name
  if (!hasArg(to.row.names)) {
    if (colnames(tibble)[1] == "observations") {
      to.row.names <- "variables"
    } else if (colnames(tibble)[1] == "variables") {
      to.row.names <- "observations"
    } else {
      to.row.names <- "rows"
    }
  }
  
  # Define columns to keep (Most common column type)
  data.class.1 <- tibble %>%
    sapply(class) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    dplyr::first()
  
  keep <- c(from.row.names,
            .colnames_class(tibble, data.class.1))
  
  
  # Transpose
  tibble.t <- tibble %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    .tibble2matrix(from.row.names = from.row.names) %>%
    t() %>%
    .matrix2tibble(to.row.names = to.row.names)
  
  # Return transposed tibble
  return(tibble.t)
  
}


#' Transforms a list to tibble logical indication for variables
#'
#' @param x list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
.list2logical_df <- function(x, identifier = "variables") {
  
  #
  df <- tibble::tibble(!!identifier := unique(unlist(x)))
  
  # Add logical columns
  for (column in names(x)) {
    df <- df %>%
      dplyr::mutate(!!column := variables %in% x[[column]])
  }
  
  # Return
  return(df)
  
}

