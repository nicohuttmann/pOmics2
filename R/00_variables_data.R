#' Return variables
#'
#' @param variables vector of variables
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables <- function(variables, dataset) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # No variables specified
  if (!hasArg(variables)) {
    variables_output <- Datasets[[dataset]][["Variables"]] %>%
      dplyr::pull(var = "variables", name = NULL)
    return(variables_output)
  }
  
  
  # Check if input is vector
  is_vector_input <- tryCatch(is.atomic(variables),
                              error = function(cond) FALSE)
  
  
  # if variables input expression
  if (!is_vector_input) {
    variables_output <- Datasets[[dataset]][["Variables"]] %>%
      dplyr::filter(!!dplyr::enquo(variables)) %>%
      dplyr::pull(var = "variables", name = NULL)
    return(variables_output)
    
    # input given as vector
    # intersect given proteins with proteins in dataset
  } else {
    variables_output <- intersect(variables,
                                  Datasets[[dataset]][["Variables"]] %>%
                                    dplyr::pull(var = "variables", name = NULL))
    return(variables_output)
    
  }
  
  
}


#' Return variables data
#'
#' @param data_
#' @param which which variables data to pull (multiple supported)
#' @param variables (optional) vector of variables or expression
#' @param output.type output type (default = "vector" or "tibble_inlist" for
#' multiple arguments; combination of "vector", "list", "tibble", or
#' "data.frame" to define data type and "_inlist" or "" if data should be put
#' into a list or not)
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
get_variables_data <- function(data_, 
                               which,
                               variables,
                               output.type = "list_",
                               output.name = "variables_data", 
                               dataset) {
  
  # Check dataset
  dataset <- get_dataset(dataset = dataset)
  
  
  ### Variables
  
  # No variables defined
  if (!hasArg(variables)) {
    
    variables <- get_variables(dataset = dataset)
    
    # Variables defined
  } else {
    
    # Check if input is vector
    is_vector_input <- tryCatch(is.atomic(variables),
                                error = function(cond) FALSE)
    
    
    # if variables input is vector
    if (!is_vector_input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables),
                                 dataset = dataset)
    }
    
  }
  
  
  # Which data to pull
  
  # No argument given (which)
  if (!hasArg(which)) 
    stop("Not defined <which> variables data column to pull.", call. = FALSE)
  
  
  # Check if names of data were not found in variables_data
  if (any(!which %in% get_variables_data_names(dataset = dataset))) 
    stop(paste0("Following variables data column names were not found: ", 
                paste(setdiff(which, 
                              get_variables_data_names(dataset = dataset)), 
                      collapse = ", "), "."), 
         call. = FALSE)
  
  
  
  
  # Check number of columns and output.type
  if (length(which) > 1 && grepl(pattern = "vector", x = output.type)) {
    stop("Multiple variables data columns can not be stored in a vector.")
  }
  
  
  # One variables data column
  if (length(which) == 1) {
    
    # Tibble
    if (grepl(pattern = "tibble|list_", x = output.type)) {
      
      data <- Datasets[[dataset]][["Variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::select("variables", !!which)
      
      # Vector
    } else if (grepl(pattern = "vector", x = output.type)) {
      
      data <- Datasets[[dataset]][["Variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables")
      
      
      # List
    } else if (regexpr(pattern = "list", text = output.type) == 1) {
      
      data <- Datasets[[dataset]][["Variables"]] %>%
        dplyr::filter(.data[["variables"]] %in% !!variables) %>%
        dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "variables") %>%
        as.list()
      
    } 
    
    # Multiple variables data names
  } else if (grepl(pattern = "tibble|list_", x = output.type)) {
    
    data <- Datasets[[dataset]][["Variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which)
    
    # Data frame
  } else if (grepl(pattern = "data.frame", x = output.type)) {
    
    data <- Datasets[[dataset]][["Variables"]] %>%
      dplyr::filter(.data[["variables"]] %in% !!variables) %>%
      dplyr::arrange(match(.data[["variables"]], !!variables)) %>%
      dplyr::select("variables", !!which) %>%
      .tibble2data_frame(from.row.names = "variables")
    
    # Output type not found
  } else {
    
    stop(paste0("Output type <",
                output.type,
                "> not supported. Use <vector> and <list> for single ",
                "variables data calls or <tibble> and <data.frame> for ",
                "single and multiple variables data calls with the ",
                "optional suffix <_inlist>."))
    
  }
  
  #### 
  
  # Pack data
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = list(dataset = dataset, 
                                             variables = variables, 
                                             observations = NULL, 
                                             data_info = NULL), #setdiff(which, "variables")), 
                      output.name = output.name, 
                      output.type = output.type, 
                      overwrite = F)
  
  # Return data
  return(data_)
  
}


#' Returns variables data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_variables_data_names <- function(dataset) {
  
  # Get dataset name
  dataset <- get_dataset(dataset)
  
  variables_data_names <- names(Datasets[[dataset]][["Variables"]])
  
  return(variables_data_names)
  
}


#' Creates template for new variables data
#'
#' @param dataset dataset
#' @param fill entries of vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_variables_template <- function(dataset, fill = NA) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get names vector
  template <- Datasets[[dataset]][["Variables"]] %>%
    dplyr::pull(var = variables, name = variables)
  
  # Fill template
  template[] <- fill
  
  return(template)
  
}



#' Add data to variables data frame
#'
#' @param data variables data as tibble or vector
#' @param column column names to save
#' @param name name
#' @param dataset dataset
#' @param fill value to fill non-existing variables with
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
save_variables_data <- function(data,
                                column, 
                                name,
                                dataset, 
                                fill = NA) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get template
  template <- get_variables_template(dataset,)
  
  
  # Check data input
  if (!hasArg(data)) stop("No data given.", call. = FALSE)
  
  # Check data name
  if (!hasArg(name)) {
    if (hasArg(column)) {
      name <- column
    } else {
      stop("Please provide a <name> for the new variables data.", 
           call. = FALSE)
    }
  }
  
  
  # Check if given data is tibble or vector
  if (tibble::is_tibble(data)) {
    
    # Check data and arguments 
    if (names(data)[1] != "variables") 
      stop("First column of a tibble must be named variables.", 
           call. = FALSE)
    
    if (!hasArg(column)) 
      stop("Please specify which <column> to save as variables data.", 
           call. = FALSE)
    
    if (any(!column %in% names(data))) 
      stop(paste0("Following column names were not found in the data: ", paste(setdiff(column, names(data)), collapse = ", ")), 
           call. = FALSE)
    
    if (any(!data[["variables"]] %in% Datasets[[dataset]][["Variables"]][["variables"]]))
      stop(paste0("The data contains variables that do not exist in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    
    if (any(!Datasets[[dataset]][["Variables"]][["variables"]] %in% data[["variables"]])) 
      warning(paste0("Some variables in the <dataset> ", 
                     dataset, " were not present in the data."), call. = FALSE)
    
    if (any(name %in% get_variables_data_names(dataset)))
      warning(paste0("Following variables data names already exist in the <dataset> ", 
                     dataset, ": ", 
                     paste(intersect(name, get_variables_data_names(dataset)), 
                           collapse = ", "), ". Use a new <name>."), 
              call. = FALSE)
    
    if (length(column) != length(name)) 
      stop("Please provide one name for each column specified.", 
           call. = FALSE)
    
    # Add
    Datasets[[dataset]][["Variables"]] <<- 
      Datasets[[dataset]][["Variables"]] %>% 
      dplyr::select(-dplyr::any_of(name)) %>% 
      dplyr::left_join(y = data %>% 
                         dplyr::select(dplyr::all_of(c("variables", column))) %>% 
                         dplyr::rename(all_of(setNames(column, name))), 
                       by = "variables")
    
    return(invisible(data))
    
    # Save data from vector input
  } else if (is.atomic(data)) {
    
    # Check new name
    if (name %in% get_variables_data_names(dataset)) 
      warning(paste0("Name of new variables data already exists in the <dataset> ", 
                     dataset, ". Use a new <name>."), 
              call. = FALSE)
    
    # Check if data is named
    if (is.atomic(data) && is.null(names(data))) 
      stop("Data must be named.", call. = FALSE)
    # Check if names exist in variables
    if (all(!names(data) %in% names(template)))
      stop(paste0("None of the variables in the data were found in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    if (any(!names(data) %in% names(template)))
      stop(paste0("The data contains variables that do not exist in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    if (any(!names(template) %in% names(data))) 
      warning(paste0("Some variables in the <dataset> ", 
                     dataset, " were not present in the data."), call. = FALSE)
    
    
    # Fill template with data
    if (is.factor(data)) {
      template[] <- as.character(data[names(template)])
      template <- factor(template, levels = levels(data))
    } else {
      template[] <- data[names(template)]
    }
    
    
    # Add 
    Datasets[[dataset]][["Variables"]] <<-
      Datasets[[dataset]][["Variables"]] %>%
      dplyr::mutate(!!name := unname(template))
    
    return(invisible(NULL))
    
  } else {
    
    stop("Variables data must be given as a tibble with a variables column or a named vector.", 
         call. = FALSE)
    
  }
  
}
