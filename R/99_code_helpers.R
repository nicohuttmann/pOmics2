#' Prints vectors to console in as character vector
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
.cat_character <- function(...) {
  
  cat(paste0('c("',
             paste(..., collapse = '",\n\t"'),
             '")'))
  
}


#' Prints vectors to console in as character vector
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
.cat_numeric <- function(...) {
  
  cat(paste0('c(',
             paste(..., collapse = ',\n\t'),
             ')'))
  
}


#' Prints function snippet to console and clipboard
#'
#' @param FUN function
#' @param add.equal.sign add equal sign to add arguments
#' @param print print function snippet to console
#' @param copy2clipboard copy function snippet to clipboard
#'
#' @return
#' @export
#'
#'
.cat_function <- function(FUN, add.equal.sign = T, print = T, copy2clipboard = T) {
  
  #
  if (!hasArg(FUN) || !is.function(FUN)) return(FALSE)
  
  
  
  FUN.str <- deparse(args(FUN))
  
  # Remove last element of vector
  FUN.str <- FUN.str[FUN.str != "NULL"]
  
  
  # Collapse vector to one string
  FUN.str <-  paste(FUN.str, collapse = "")
  
  # Catch function name
  name <- deparse(substitute(FUN))
  
  FUN.str <- gsub(pattern = "function ",
                  replacement = name,
                  x = FUN.str)
  
  # Remove double spaces
  while (nchar(FUN.str) != nchar(gsub(pattern = "  ",
                                      replacement = " ",
                                      x = FUN.str))) {
    FUN.str <- gsub(pattern = "  ",
                    replacement = " ",
                    x = FUN.str)
  }
  
  
  # Separate arguments
  FUN.str <- unlist(strsplit(FUN.str, split = ", "))
  
  
  # Add = sign
  if (add.equal.sign) {
    
    for (i in setdiff(which(!grepl("=", FUN.str)), length(FUN.str))) {
      
      FUN.str[i] <- paste0(FUN.str[i], " = ")
      
    }
    
    # Modify last argument if no default is given
    if (!grepl("=", FUN.str[length(FUN.str)])) {
      FUN.str[length(FUN.str)] <-
        paste0(substring(FUN.str[length(FUN.str)],
                         1,
                         nchar(FUN.str[length(FUN.str)]) - 2),
               " = ",
               substring(FUN.str[length(FUN.str)],
                         nchar(FUN.str[length(FUN.str)]) - 1))
    }
    
  }
  
  
  # Combine arguments with linebreaks and tabs
  FUN.str <- paste(FUN.str, collapse = ", \n\t")
  
  
  # Copy to clipboard
  if (copy2clipboard) {
    cat(FUN.str, file = "clipboard")
  }
  
  # print to console
  if (print) {
    cat(FUN.str)
  }
  
  
  return(invisible(FUN.str))
  
}
