#' Imputes values based on normal distribution
#'
#' @param data 
#' @param shift shift in standard deviations
#' @param width width in standard deviations
#' @param seed seed
#'
#' @return
#' @export
#'
#'
impute_norm <- function(data, shift = 1.8, width = 0.3, seed = 123) {
  
  # Set seed
  set.seed(seed = seed)
  
  # Impute values
  if (tibble::is_tibble(data) | is.data.frame(data)) {
    
    data <- data %>%
      dplyr::mutate(
        across(.cols = where(function(x) is.numeric(x) & any(x == 0)), 
               .fns = function(x) {
                 x.log2 <- log2(x)
                 x.log2[is.infinite(x.log2)] <- NA
                 x[x == 0] <- 
                   round(2 ^ rnorm(n = sum(x == 0),
                                   mean = mean(x.log2, na.rm = TRUE) - shift * sd(x.log2, na.rm = TRUE),
                                   sd = width * sd(x.log2, na.rm = TRUE)),
                         digits = -2)
                 x
               }
        )
      )
    
  } else if (is.matrix(data)) {
    
    # Prepare log2 data frame
    data.log2 <- data
    data.log2[data.log2 == 0] <- NA
    data.log2 <- log2(data.log2)
    
    # Impute
    for (i in which(apply(X = data, MARGIN = 2, FUN = function(x) any(x == 0)))) {
      
      data[data[, i] == 0, i] <- 
        round(2 ^ rnorm(n = sum(data[, i] == 0),
                        mean = mean(data.log2[, i], na.rm = TRUE) - shift * sd(data.log2[, i], na.rm = TRUE),
                        sd = width * sd(data.log2[, i], na.rm = TRUE)),
              digits = -2)
    }
    
  } else {
    
    stop("Please provide a tibble, data.frame or a matrix as data input.")
    
  }
  
  # Return
  return(data)
  
}


#' Helper function to combine technical replicates (only returns mean if all values are > 0)
#'
#' @param x numeric vector
#'
#' @return
#' @export
#'
#' 
mean_or_0 <- function(x) {
  
  # Return 0 if any entry is 0, otherwise the mean of the number
  return(ifelse(any(x == 0), 0, mean(x)))
  
}


#' Title
#'
#' @param data 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
#' 
norm_vsn <- function(data, verbose = F, ...) {
  
  # Prepare data
  rows <- names(data)[1]
  
  if (rows == "observations") {
    data_m <- data %>% 
      .tibble2matrix(from.row.names = rows) %>% 
      t() 
      
  } else {
    data_m <- data %>% 
      .tibble2matrix(from.row.names = rows)
  }
  
  # Apply normalization method
  data_norm_log2 <- vsn::justvsn(data_m, verbose = verbose, ...)

  # Reformat the data
  data_norm <- 2^data_norm_log2 
  
  if (rows == "observations") data_norm <- t(data_norm)
  
  data_norm <- .matrix2tibble(data_norm, to.row.names = rows)
  
  # Return normalized data
  return(data_norm)
  
}


#' Title
#'
#' @param data 
#' @param group.column 
#' @param ... 
#'
#' @return
#' @export
#'
#' 
norm_vsn_grouped <- function(data, group.column, ...) {
  
  # Check data format
  if (names(data)[1] != "observations") stop("First row must contain columns.", 
                                             call. = F)
  if (!hasArg(group.column) || !group.column %in% names(data)) 
    stop("<group.column> must be specified and match a column name in <data>.", 
         call. = F)
  
  # Separately normalize data
  groups <- dplyr::pull(data, group.column)
  data_list <- list()
  for (i in unique(groups)) {
    data_list[[i]] <- data %>% 
      filter(!!sym(group.column) == i) %>% 
      dplyr::select(-all_of(group.column)) %>% 
      norm_vsn(...)
  }
  
  # Combine separately normalized data
  data_norm <- dplyr::bind_rows(data_list) %>% 
    dplyr::mutate(!!group.column := groups, .after = 1)
  
  # Return data
  return(data_norm)
}


#' Title
#'
#'
#' @param data 
#' @param method 
#' @param targets 
#' @param cyclic.method 
#' @param ... 
#'
#' @return
#' @export
#'
#' 
norm_limma_normalizeBetweenArrays <- function(data, 
                                              method = "scale", 
                                              targets = NULL, 
                                              cyclic.method = "fast",
                                              ...) {
  
  # Prepare data
  rows <- names(data)[1]
  
  if (rows == "observations") {
    data_m <- data %>% 
      .tibble2matrix(from.row.names = rows) %>% 
      t() 
    
  } else {
    data_m <- data %>% 
      .tibble2matrix(from.row.names = rows)
  }
  
  # Apply normalization method
  data_norm <- limma::normalizeBetweenArrays(object = data_m, 
                                             method = method, 
                                             targets = targets, 
                                             cyclic.method = cyclic.method, 
                                             ...)
  
  # Reformat the data
  if (rows == "observations") data_norm <- t(data_norm)
  
  data_norm <- .matrix2tibble(data_norm, to.row.names = rows)
  
  # Return normalized data
  return(data_norm)
  
}







