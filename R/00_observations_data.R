#' Return observations
#'
#' @param observations vector of observations
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
get_observations <- function(observations, dataset) {
  
  # check dataset
  dataset <- get_dataset(dataset)
  
  # No observations specified
  # Return all
  if (!hasArg(observations)) {
    observations_output <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::pull(var = "observations", name = NULL)
    
    return(observations_output)
  }
  
  
  # Check if input is vector
  is_vector_input <- tryCatch(is.atomic(observations),
                           error = function(cond) FALSE)
  
  # if observations input expression
  if (!is_vector_input) {
    observations_output <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::filter(!!dplyr::enquo(observations)) %>%
      dplyr::pull(var = "observations", name = NULL)
    
    return(observations_output)
    
    # Return all as specified in other function
  } else if (length(observations) == 1 && observations == "all") {
    
    observations_output <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::pull(var = "observations", name = NULL)
    
    return(observations_output)
    
    # input given as vector
    # intersect given proteins with proteins in dataset
  } else {
    # All specified observations in observations data
    
    observations_all <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::pull(var = "observations", name = NULL)
    
    if (all(observations %in% observations_all)) {
      observations_output <- observations_output <- 
        intersect(observations, observations_all)
      
      return(observations_output)
    # Some specified observations match the existing ones
    } else if (any(observations %in% observations_all)) {
      
      stop("Not all specified <observations> were found in the observations table.", 
           call. = FALSE)
      
    } else {
      stop("None of the specified observations were found in the observations table. ", 
           "Try using a tidy-friendly expression.", 
           call. = FALSE)
    }
    
  }
  
}


#' Return observations data
#'
#' @param data_
#' @param which which observations data to pull
#' @param observations (optional) vector of observations or expression
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
#'
get_observations_data <- function(data_, 
                                  which,
                                  observations,
                                  output.type = "list_",
                                  output.name, 
                                  dataset) {
  
  # Check dataset
  dataset <- get_dataset(dataset = dataset)
  
  
  ### Observations
  
  # No observations defined
  if (!hasArg(observations)) {
    
    observations <- get_observations(dataset = dataset)
    
    # Observations defined
  } else {
    
    # Check if input is vector
    is_vector_input <- tryCatch(is.atomic(observations),
                                error = function(cond) FALSE)
    
    # if observations input is vector
    if (!is_vector_input) {
      observations <- get_observations(observations = !!dplyr::enquo(observations),
                                 dataset = dataset)
    }
  }
  
  
  # Which data to pull
  
  # No argument given (which)
  if (!hasArg(which)) 
    stop("Not defined <which> observations data column to pull.", call. = FALSE)
  
  
  # Check if names of data were not found in observations_data
  if (any(!which %in% get_observations_data_names(dataset = dataset))) 
    stop(paste0("Following observations data column names were not found: ", 
                paste(setdiff(which, 
                              get_observations_data_names(dataset = dataset)), 
                      collapse = ", "), "."), 
         call. = FALSE)
  
  # Check number of columns and output.type
  if (length(which) > 1 && grepl(pattern = "vector", x = output.type)) {
    stop("Multiple observations data columns can not be stored in a vector.")
  }
  
  
  # One observations data column
  if (length(which) == 1) {
    
    # Tibble
    if (grepl(pattern = "tibble|list_", x = output.type)) {
      
      data <- Datasets[[dataset]][["Observations"]] %>%
        dplyr::filter(.data[["observations"]] %in% !!observations) %>%
        dplyr::arrange(match(.data[["observations"]], !!observations)) %>%
        dplyr::select("observations", !!which)
      
    # Vector 
    } else if (grepl(pattern = "vector", x = output.type)) {
      
      data <- Datasets[[dataset]][["Observations"]] %>%
        dplyr::filter(.data[["observations"]] %in% !!observations) %>%
        dplyr::arrange(match(.data[["observations"]], !!observations)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "observations")
      
      
    # List
    } else if (regexpr(pattern = "list", text = output.type) == 1) {
      
      data <- Datasets[[dataset]][["Observations"]] %>%
        dplyr::filter(.data[["observations"]] %in% !!observations) %>%
        dplyr::arrange(match(.data[["observations"]], !!observations)) %>%
        dplyr::pull(var = !!dplyr::enquo(which), name = "observations") %>%
        as.list()
      
    } 
    
  # Multiple observations data names
  } else if (grepl(pattern = "tibble|list_", x = output.type)) {
    
    data <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::filter(.data[["observations"]] %in% !!observations) %>%
      dplyr::arrange(match(.data[["observations"]], !!observations)) %>%
      dplyr::select("observations", !!which)
    
    
  } else if (grepl(pattern = "data.frame", x = output.type)) {
    
    data <- Datasets[[dataset]][["Observations"]] %>%
      dplyr::filter(.data[["observations"]] %in% !!observations) %>%
      dplyr::arrange(match(.data[["observations"]], !!observations)) %>%
      dplyr::select("observations", !!which) %>%
      .tibble2data_frame(from.row.names = "observations")
    
  # Output type not found
  } else {
    
    stop(paste0("Output type <",
                output.type,
                "> not supported. Use <vector> and <list> for single ",
                "observations data calls or <tibble> and <data.frame> for ",
                "single and multiple observations data calls with the ",
                "optional suffix <_inlist>."))
    
  }
  
  # Output name
  if (!hasArg(output.name)) {
    output.name <- paste("observations_data", dataset, sep = "_")
  }
  
  #### 
  
  # Pack data
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = list(dataset = dataset, 
                                             variables = NULL, 
                                             observations = observations, 
                                             data_info = NULL), # setdiff(which, "observations")), 
                      output.name = output.name, 
                      output.type = output.type, 
                      overwrite = F)
  
  # Return
  return(data_)
  
}

#' Returns observations data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_observations_data_names <- function(dataset) {
  
  # Get dataset
  dataset <- get_dataset(dataset)
  
  observations_data_names <- names(Datasets[[dataset]][["Observations"]])
  
  return(observations_data_names)
  
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
get_observations_template <- function(dataset, fill = NA) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get names vector
  template <- Datasets[[dataset]][["Observations"]] %>%
    dplyr::pull(var = observations, name = observations)
  
  # Fill template
  template[] <- fill
  
  return(template)
  
}


#' Adds data to observations data frame
#'
#' @param data observations data as tibble or vector
#' @param column column names to save
#' @param name name
#' @param dataset dataset
#' @param fill value to fill non-existing observations with
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
save_observations_data <- function(data,
                                   column,
                                   name,
                                   dataset, 
                                   fill = NA) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get template
  template <- get_observations_template(dataset,)
  
  
  # Check data input
  if (!hasArg(data)) stop("No data given.", call. = FALSE)
  
  # Check data name
  if (!hasArg(name)) {
    if (hasArg(column)) {
      name <- column
    } else {
      stop("Please provide a <name> for the new observations data.", 
         call. = FALSE)
    }
  }
    
  
  
  # Check if given data is tibble or vector
  if (tibble::is_tibble(data)) {
    
    # Check data and arguments 
    if (names(data)[1] != "observations") 
      stop("First column of a tibble must be named observations.", 
           call. = FALSE)
    
    if (!hasArg(column)) 
      stop("Please specify which <column> to save as observations data.", 
           call. = FALSE)
    
    if (any(!column %in% names(data))) 
      stop(paste0("Following column names were not found in the data: ", paste(setdiff(column, names(data)), collapse = ", ")), 
           call. = FALSE)
    
    if (any(!data[["observations"]] %in% Datasets[[dataset]][["Observations"]][["observations"]]))
      stop(paste0("The data contains observations that do not exist in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    
    if (any(!Datasets[[dataset]][["Observations"]][["observations"]] %in% data[["observations"]])) 
      warning(paste0("Some observations in the <dataset> ", 
                     dataset, " were not present in the data."), call. = FALSE)
    
    if (any(name %in% get_observations_data_names(dataset))) 
      warning(paste0("Following observations data names already exist in the <dataset> ", 
                  dataset, ": ", 
                  paste(intersect(name, get_observations_data_names(dataset)), 
                        collapse = ", "), ". Use a new <name>."), 
           call. = FALSE)
    
    if (length(column) != length(name)) stop("Please provide one name for each column specified.", 
                                             call. = FALSE)
    
    # Add
    Datasets[[dataset]][["Observations"]] <<- 
      Datasets[[dataset]][["Observations"]] %>% 
      dplyr::select(-dplyr::any_of(name)) %>% 
      dplyr::left_join(y = data %>% 
                         dplyr::select(dplyr::all_of(c("observations", column))) %>% 
                         dplyr::rename(all_of(setNames(column, name))), 
                       by = "observations")
    
    return(invisible(data))
    
    # Save data from vector input
  } else if (is.atomic(data)) {
    
    # Check new name
    if (name %in% get_observations_data_names(dataset)) 
      warning(paste0("Name of new observations data already exists in the <dataset> ", 
                     dataset, ". Use a new <name>."), 
              call. = FALSE)
    
    # Check if data is named
    if (is.atomic(data) && is.null(names(data))) 
      stop("Data must be named.", call. = FALSE)
    # Check if names exist in observations
    if (all(!names(data) %in% names(template)))
      stop(paste0("None of the observations in the data were found in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    if (any(!names(data) %in% names(template)))
      stop(paste0("The data contains observations that do not exist in the <dataset> ", 
                  dataset, "."), call. = FALSE)
    if (any(!names(template) %in% names(data))) 
      warning(paste0("Some observations in the <dataset> ", 
                     dataset, " were not present in the data."), call. = FALSE)
    
    
    # Fill template with data
    if (is.factor(data)) {
      template[] <- as.character(data[names(template)])
      template <- factor(template, levels = levels(data))
    } else {
      template[] <- data[names(template)]
    }
    
    
    # Add 
    Datasets[[dataset]][["Observations"]] <<-
      Datasets[[dataset]][["Observations"]] %>%
      dplyr::mutate(!!name := unname(template))
    
    return(invisible(NULL))
    
  } else {
    
    stop("Observations data must be given as a tibble with an observations column or a named vector.", 
         call. = FALSE)
    
  }
  
}
