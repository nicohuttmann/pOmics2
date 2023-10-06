#' Title
#'
#' @return
#' @export
#'
#' @examples
#' # list_() returns an empty list with an additional class attribute
#' list_()
#' 
#' # it can be used to officially start an analysis prior to get_data_frame()
#' list_() %>% 
#'   get_data_frame(...)
list_ <- function() {
  
  list_object <- list()
  
  class(list_object) <- c("list_", "list")
  
  return(list_object)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_nothing <- function(data_, input.name, output.name = "_nothing") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Literally do nothing
  data <- data
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param FUN function to apply to data
#' @param ... specific arguments
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_fun <- function(data_, FUN, ..., input.name, output.name = "_fun") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Test input function
  if (!hasArg(FUN) | !is.function(FUN)) {
    stop("No function given for <FUN> argument.")
  }
  
  # Apply function
  data <- do.call(what = FUN,
                  args = list(data, ...))
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  if (any(deparse(substitute(FUN)) %in% c("save_dataset", 
                                          "save_data_frame", 
                                          "save_observations_data", 
                                          "save_variables_data"))) {
    data_ <- data_
    
  } else if (any(deparse(substitute(FUN)) %in% c("data_frame2new_dataset"))) {
    data_ <- .pack_data(data, data_, NULL, output.name)
    # Pack data as usual
  } else {
    data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  }
  
  # Return
  return(data_)
  
}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param FUN function to apply to data
#' @param ... specific arguments
#' @param input.names name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_fun_m <- function(data_, FUN, ..., input.names, output.name = "_fun_m") {
  
  # Check input
  data <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data, attributes)
  
  ####
  
  # Test input function
  if (!hasArg(FUN) | !is.function(FUN)) {
    stop("No function given for <FUN> argument.")
  }
  
  # Apply function
  data <- do.call(what = FUN,
                  args = list(data, ...))
  
  # Output name
  if (substr(output.name, 1, 1) == "_")
    output.name <- paste0(
      paste(input.names, collapse = "-"), 
      output.name)
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = 
                        .merge_data_attributes(data_attributes_list), 
                      output.name = output.name, 
                      overwrite = F)
  
  # Return
  return(data_)
  
}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param FUN function to apply to data
#' @param ... specific arguments
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_fun_grouped <- function(data_, 
                           group.column, 
                           FUN, 
                           ..., 
                           input.name, 
                           output.name = "_fun") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Test input function
  if (!hasArg(FUN) | !is.function(FUN)) {
    stop("No function given for <FUN> argument.")
  }
  
  if (!hasArg(group.column) || !group.column %in% names(data)) 
    stop("<group.column> must be specified and match a column name in <data>.", 
         call. = F)
  
  # Separately apply function
  groups <- dplyr::pull(data, group.column)
  
  data_list <- list()
  
  for (i in unique(groups)) {
    data_list[[i]] <- do.call(
      what = FUN,
      args = list(data %>% 
                    filter(!!sym(group.column) == i) %>% 
                    dplyr::select(-all_of(group.column)), ...))
  }
  
  # Combine data
  data <- dplyr::bind_rows(data_list) %>% 
    dplyr::mutate(!!group.column := groups, .after = 1)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  if (any(deparse(substitute(FUN)) %in% c("save_dataset", 
                                          "save_data_frame", 
                                          "save_observations_data", 
                                          "save_variables_data"))) {
    data_ <- data_
    
  } else if (any(deparse(substitute(FUN)) %in% c("data_frame2new_dataset"))) {
    data_ <- .pack_data(data, data_, NULL, output.name)
    # Pack data as usual
  } else {
    data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  }
  
  # Return
  return(data_)
  
}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param expr expression what should happen inside the data frame
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_with <- function(data_, expr, ..., input.name, output.name = "_with") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Apply function
  data <- eval(substitute(expr), envir = data)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, "list_", F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell or column (must contain x as
#' variable, e.g. x > 2)
#' @param FUN alternative argument to supply what to do with each value or column
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param eval.rowwise evaluate columns row by row (default = T)
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_expr <- function(data_, 
                    expr, 
                    FUN, 
                    modify, 
                    ignore, 
                    eval.rowwise = T, 
                    input.name, 
                    output.name = "_expr") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Save dimensions of data frame
  dimension <- dim(data)
  
  
  # Define columns to modify
  
  # ignore
  if (!hasArg(ignore)) {
    ignore <- c()
  }
  
  # modify
  if (!hasArg(modify)) {
    modify <- .data_columns(data, data_attributes)
  }
  
  # Subtract columns to ignore from columns to modify
  modify <- setdiff(modify, ignore)
  
  
  # Apply defined expr or function
  if (!hasArg(expr) && !hasArg(FUN)) {
    stop("Please provide an expression <expr> or a function <FUN> to evaluate the data.", call. = FALSE)
  }
  
  
  # Rowwise
  if (eval.rowwise) data <- dplyr::rowwise(data)
  
  # Form function from expression
  if (hasArg(expr)) {
    FUN <- function(x) rlang::eval_tidy(rlang::enexpr(expr))
  }
  
  # Apply expression
  data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(modify),
                    FUN))
  
  # Correct if output of function is matrix
  if ("matrix" %in% unlist(lapply(data[modify], class))) {
    data <- data %>% 
      dplyr::mutate(
        dplyr::across(dplyr::all_of(modify),
                      function(x) c(x)))
  }
 
  # Rowwise
  if (eval.rowwise) data <- dplyr::ungroup(data)
  
  
  # Check dimensions after data manipulation
  if (!all(dimension == dim(data))) {
    
    warning("Attention: The applied function changed the dimensions ",
            "of the data.")
    
  }
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Transposes tibble and uses first column as column names
#'
#' @param data_ data_ list
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_transpose <- function(data_,
                         from.row.names,
                         to.row.names,
                         input.name,
                         output.name = "_t") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Transpose data
  data <- .transpose_tibble(tibble = data,
                            from.row.names = from.row.names,
                            to.row.names = to.row.names)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' dplyr mutate function for list_ objects
#'
#' @param data_ list or tibble
#' @param column name of new column (as string)
#' @param texpr tidy expression to to use for a new column
#' @param eval.rowwise evaluate columns row by row
#' @param ... additional arguments for the mutate function
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_mutate <- function(data_, 
                      column, 
                      texpr, 
                      eval.rowwise = T, 
                      ..., 
                      input.name, 
                      output.name = "_mutate") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  if (!hasArg(column) || !is.character(column))
    stop("Please specify the <column> column name as a string.", 
         call. = FALSE)
  
  if (!hasArg(texpr)) 
    stop("Please specify the <texpr> (tidy expression) to mutate a column.", 
         call. = FALSE)
  
  # Position
  data_cols <- .data_columns(data, data_attributes)
  
  # Rowwise
  if (eval.rowwise) data <- dplyr::rowwise(data)
  
  # Mutate
  if (any(c(".before", ".after") %in% names(list(...))) || length(data_cols) == 0) {
    data <- data %>% 
      dplyr::mutate(!!column := !!dplyr::enquo(texpr), ...)
  } else {
    data <- data %>% 
      dplyr::mutate(!!column := !!dplyr::enquo(texpr), 
                    .before = dplyr::any_of(data_cols), 
                    ...)
  }
  
  # Rowwise
  if (eval.rowwise) data <- dplyr::ungroup(data)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' dplyr select function for list_ objects
#'
#' @param data_ list or tibble
#' @param texpr tidy expression to to use for a new column
#' @param select.data.only only affect columns columns containing data?
#' @param input.name name of input data
#' @param output.name name of output data
#' 
#' @return
#' @export
#'
#'
do_select <- function(data_, 
                      texpr, 
                      select.data.only = T, 
                      input.name, 
                      output.name = "_select") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Check argument
  if (!hasArg(texpr)) 
    stop("Please specify the <texpr> (tidy expression) to select which columns to keep.", 
         call. = FALSE)
  
  # Data column names
  data_cols <- .data_columns(data, data_attributes)
  
  # Select data = T (only affect data)
  if (select.data.only) {
    # save non-data columns
    data_1 <- data %>% 
      dplyr::select(!dplyr::all_of(data_cols))
    # Use only data columns and select based on tidy-expression <texpr>
    data <- data %>% 
      dplyr::select(dplyr::all_of(data_cols)) %>% 
      dplyr::select(!!dplyr::enquo(texpr)) %>% 
      # Add first column for merge
      dplyr::mutate(!!names(data_1)[1] := data_1[[1]], .before = 1)
    # Merge non-data columns and selected data columns
    data <- data_1 %>% 
      dplyr::full_join(data, by = names(data_1)[1])
    # select from all columns
  } else {
    data <- data %>% 
      dplyr::select(!!dplyr::enquo(texpr))
  }
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' dplyr filter function for list_ objects
#'
#' @param data_ list or tibble
#' @param texpr tidy expression to use to filter data
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_filter <- function(data_, texpr, input.name, output.name = "_filter") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Check tidy expression input
  if (!hasArg(texpr)) 
    stop("Please specify by which <texpr> (tidy expression) to filter the data.", 
         call. = FALSE)
  
  # Mutate
  data <- data %>% 
    dplyr::filter(!!dplyr::enquo(texpr))
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' dplyr join functions for list_ objects
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
#' @param FUN function to be applied to each cell
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_join <- function(data_, 
                   join.type = "full", 
                   by, 
                   suffix, 
                   ..., 
                   input.names, 
                   output.name) {
  
  # Check input
  data <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data, attributes)
  
  
  ####
  
  data_x <- data[[1]]
  
  data_y <- data[[2]]
  
  # by argument
  # by <- match.arg(arg = by, 
  #                 choices = c("variables", 
  #                             "observations")) 
  
  # Suffix argument
  if (!hasArg(suffix)) suffix <- paste0("_", input.names)
  else if (length(unique(suffix)) != length(input.names)) 
    stop("<input.names> must have the same length as <suffix>.")
  
  
  # Join data frames
  
  # join.type = full
  if (join.type == "full") {
    
    data <- dplyr::full_join(x = data_x, 
                             y = data_y, 
                             by = by,   
                             suffix = suffix, 
                             ...)
    # join.type = inner
  } else if (join.type == "inner") {
    
    data <- dplyr::inner_join(x = data_x, 
                              y = data_y, 
                              by = by,   
                              suffix = suffix, 
                              ...)
    # join.type = left
  } else if (join.type == "left") {
    
    data <- dplyr::left_join(x = data_x, 
                              y = data_y, 
                              by = by,   
                              suffix = suffix, 
                              ...)
    # join.type = right
  } else if (join.type == "right") {
    
    data <- dplyr::right_join(x = data_x, 
                             y = data_y, 
                             by = by,   
                             suffix = suffix, 
                             ...)
    
  } else {
    stop("Please use a know join type from the dplyr join-functions.", 
         call. = FALSE)
  }
  
  
  # Output name
  if (!hasArg(output.name)) 
    output.name <- paste0(
      paste(input.names, collapse = "-"), 
      "_", 
      join.type, 
      "joined")
  
  ####
  
  
  # Prepare return
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = 
                        .merge_data_attributes(data_attributes_list), 
                      output.name = output.name, 
                      overwrite = F)
  
  # Return
  return(data_)
  
}


#' dplyr join functions for more than two objects in a list_ objects
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
#' @param FUN function to be applied to each cell
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_join_m <- function(data_, 
                      join.type = "full", 
                      by, 
                      suffix, 
                      ..., 
                      input.names, 
                      output.name) {
  
  # Check input
  data <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data, attributes)
  
  
  ####
  
  data_all <- data
  
  data <- data[[1]]
  
  # by argument
  by <- match.arg(arg = by, 
                  choices = c("variables", 
                              "observations")) 
  
  # suffix argument
  if (!hasArg(suffix)) suffix <- paste0("_", input.names)
  else if (length(unique(suffix)) != length(data.names)) 
    stop("<input.names> must have the same length as <suffix>.")
  
  
  # Join data frames
  
  # First join
  data <- dplyr::full_join(x = data, 
                           y = data_all[[2]], 
                           by = by,   
                           suffix = suffix[1:2], 
                           ...)
  
  # All other
  if (length(data_all) > 2) {
    
    for (i in 3:length(data_all)) {
      
      data <- dplyr::full_join(x = data, 
                               y = data_all[[i]], 
                               by = by,   
                               suffix = c(suffix[i - 1], suffix[i]), 
                               ...)
    }
  }
  
  
  
  # Output name
  if (!hasArg(output.name)) 
    output.name <- paste0(
      paste(input.names, collapse = "-"), 
      "_", 
      join.type, 
      "joined")
  
  ####
  
  
  # Prepare return
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = 
                        .merge_data_attributes(data_attributes_list), 
                      output.name = output.name, 
                      overwrite = F)
  
  # Return
  return(data_)
  
}


#' Add observations data to data frame
#'
#' @param data_ data frame
#' @param which observations data
#' @param name name of new column (default = which)
#' @param dataset dataset
#' @param input.name data frame to be modified
#' @param output.name data frame to return in list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_observations_data <- function(data_,
                                  which,
                                  name,
                                  dataset,
                                  input.name,
                                  output.name = "_obs") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  if (!hasArg(name)) name <- which
  
  # Check dataset
  if (!hasArg(dataset)) dataset <- data_attributes[["dataset"]]
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get observations from data frame
  observations <- dplyr::pull(data, var = "observations")
  
  # Get observations_data
  observations_data <- get_observations_data(which = which, 
                                             observations = observations, 
                                             output.type = "tibble", 
                                             dataset = dataset)
  
  # Check observations 
  if (any(!observations %in% observations_data$observations))
    stop("Data does not contain all observations.")
  
  # Check name argument
  if (!is.character(name) || name %in% names(data)) {
    stop(paste0("<name> must be a string and cannot exist in the data.frame already."))
  }
  
  
  # Add column
  data <- dplyr::right_join(observations_data, 
                            data, 
                            by = "observations") %>% 
    dplyr::rename_with(~ name, dplyr::all_of(which))
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_attributes[["data_info"]] <- c(data_attributes[["data_info"]], which)
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' Add variables data to data frame
#'
#' @param data_ data frame
#' @param which variables data
#' @param name name of new column (default = which)
#' @param dataset dataset
#' @param input.name data frame to be modified
#' @param output.name data frame to return in list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
add_variables_data <- function(data_,
                               which,
                               name,
                               dataset,
                               input.name,
                               output.name = "_var") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  if (!hasArg(name)) name <- which
  
  # Check dataset
  if (!hasArg(dataset)) dataset <- data_attributes[["dataset"]]
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get variables from data frame
  variables <- dplyr::pull(data, var = "variables")
  
  # Get variables_data
  variables_data <- get_variables_data(which = which, 
                                       variables = variables, 
                                       output.type = "tibble", 
                                       dataset = dataset)
  
  # Check variables 
  if (any(!variables %in% variables_data$variables))
    stop("Data does not contain all variables.")
  
  # Check name argument
  if (!is.character(name) || name %in% names(data)) {
    stop(paste0("<name> must be a string and cannot exist in the data.frame already."))
  }
  
  
  # Add column
  data <- dplyr::right_join(variables_data, 
                            data, 
                            by = "variables") %>% 
    dplyr::rename_with(~ name, dplyr::all_of(which))
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_attributes[["data_info"]] <- c(data_attributes[["data_info"]], which)
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' Assemble data from dataset and return in list
#'
#' @param data_ 
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param output.type data type (default = "list_"; combination of
#' "tibble", "data.frame" or "matrix" to define data type and "_inlist" or ""
#' if data should be put into a list or not)
#' @param output.name name to save in list (default = which)
#' @param dataset dataset name 
#'
#' @return
#' @export
#'
#'
put_data <- function(data_, 
                     data, 
                     data_attributes = list(), 
                     output.name = "data") {
  
  # Check input
  if (!hasArg(data)) {
    stop("<data> must contain something.", 
         call. = FALSE)
  }
  
  data <- data
  
  #### 
  
  # Pack data
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = data_attributes, 
                      output.name = output.name, 
                      output.type = "list_", 
                      overwrite = F)
  
  # Return
  return(data_)
  
}


#' Collapses rows of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups
#' @param by column specifying the groups
#' @param input name of input data frame
#' @param new.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_row_summary <- function(data_, 
                           FUN = mean, 
                           by = "All", 
                           mod.data.columns = T, 
                           mod.columns, 
                           input.name, 
                           output.name = "_rowgrouped") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Add dummy variable for all
  if (by == "All" & !(by %in% names(data)))
    data <- dplyr::mutate(data, All = "all")
  if (!by %in% names(data)) {
    
    stop("Group column not found in data frame.")
    
  }
  
  # Indicate data columns to modify
  if (mod.data.columns) {
    if (names(data)[1] == "observations") {
      data_columns <- data_attributes[["variables"]]
      col_name_1 <- "observations"
    } else if (names(data)[1] == "variables") {
      data_columns <- data_attributes[["observations"]]
      col_name_1 <- "variables"
    } else {
      stop("No observations or variables column found.")
    }
  } else {
    data_columns <- c()
  }
  
  if (hasArg(mod.columns)) {
    data_columns <- c(mod.columns, 
                      data_columns)
  }
  
  
  # Save initial data frame
  data0 <- data
  
  # Add new data frame
  data <- data %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = dplyr::all_of(data_columns),
                            .fns = FUN))
  
  # Collapse character
  data0 <- data0 %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = -dplyr::any_of(data_columns),
                            .fns = function(x) if (length(unique(x)) == 1) x[1]
                            else NA)) %>%
    dplyr::select(where(~!any(is.na(.))))
  
  # Combine data frames
  data <- dplyr::full_join(data0, data, by = by)
  
  # Rename grouping column
  data <- data %>%
    dplyr::rename_with(~ col_name_1, dplyr::all_of(by)) %>% 
    dplyr::relocate(dplyr::all_of(col_name_1), .before = 1)
  
  
  # Add new observations to data attributes
  data_attributes[[col_name_1]] <- data[[1]]
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Collapses rows of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups with (default sum)
#' @param name (optional) name of new column
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param input.name name of input data frame
#' @param output.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_column_summary <- function(data_,
                              FUN = sum,
                              name,
                              modify,
                              ignore,
                              input.name,
                              output.name = "_colgrouped") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Name of new column
  if (!hasArg(name)) {
    name <- deparse(substitute(FUN))
  }
  
  
  # Define columns to modify
  
  # ignore
  if (!hasArg(ignore)) {
    ignore <- c()
  }
  
  # modify
  if (!hasArg(modify)) {
    modify <- .data_columns(data, data_attributes)
  }
  
  modify <- setdiff(modify, ignore)
  
  
  # Summarize data
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!name := FUN(dplyr::c_across(cols = dplyr::all_of(modify))),
                  .after = 1) %>%
    dplyr::ungroup()
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' dplyr mutate function for list_ objects
#'
#' @param data_ list or tibble
#' @param column name of new column (as string)
#' @param texpr tidy expression to to use for a new column
#' @param eval.rowwise evaluate columns row by row
#' @param ... additional arguments for the mutate function
#' @param input.name name of input data
#' @param output.name name of output data
#'
#' @return
#' @export
#'
#'
do_row_mutate <- function(data_, 
                          texpr, 
                          calc_with, 
                          group_by = "All", 
                          ..., 
                          input.name, 
                          output.name = "_rowmutate") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  if (!hasArg(calc_with) || !is.character(calc_with))
    stop("Please specify the <calc_with> column name as a string.", 
         call. = FALSE)
  
  if (!hasArg(texpr)) 
    stop("Please specify the <texpr> (tidy expression) to define the operations.", 
         call. = FALSE)
  
  
  # Add dummy variable for all
  if (group_by == "All" & !(group_by %in% names(data)))
    data <- dplyr::mutate(data, All = "all")
  if (!group_by %in% names(data)) {
    
    stop("Group column not found in data frame.")
    
  }
  
  
  # Collect arguments
  col_names <- c(names(data)[1], group_by)
  
  groups <- unique(data[["tissue"]]) %>% 
    setNames(., .)
  
  
  # Do operations within each row group
  data <- purrr::map(
    .x = setNames(groups, groups), 
    .f = ~ 
      data %>%
      dplyr::filter(!!dplyr::sym(group_by) == .x) %>% 
      dplyr::select(!all_of(col_names)) %>% 
      .transpose_tibble() %>% 
      dplyr::mutate(!!.x := !!dplyr::enquo(texpr), .keep = "unused") %>% 
      .tibble2matrix(., from.row.names = names(.)[1]) %>%
      t() %>%
      .matrix2tibble(to.row.names = col_names[1])) %>% 
    list_rbind()
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}



