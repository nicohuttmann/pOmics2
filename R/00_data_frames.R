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
get_data_frame <- function(data_, 
                           which,
                           variables,
                           observations,
                           output.type = "list_", 
                           output.name,
                           dataset) {
  
  # Checks correct name of dataset
  dataset <- get_dataset(dataset)
  
  # Default data name
  if (!hasArg(which)) {
    stop("Specify the data frame name with <which>.", call. = FALSE)
  } else if (!is.character(which)) {
    stop("Provide a character string as data frame name with <which>.", call. = FALSE)
  } else if (length(which) != 1) {
    stop("Provide one data frame name with <which>.", call. = FALSE)
  }
  
  # Check if data frame exists
  if (!which %in% get_data_frame_names(dataset = dataset)) {
    
    stop("The data frame name <which> could not be found. Check for spelling or if it has been extracted from the raw data.")
    
  }
  
  
  
  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)
  
  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   dataset = dataset)
  
  
  
  # Grab data
  data <- Datasets[[dataset]][["Data_frames"]][[which]]
  
  
  # Tibble 
  if (grepl(pattern = "tibble|list_", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables)))
    
    # Data frame
  } else if (grepl(pattern = "data.frame", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      .tibble2data_frame()
    
    # Matrix
  } else if (grepl(pattern = "matrix", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      .tibble2matrix()
    
  } else {
    
    stop("Data type <",
         output.type,
         "> is not supported. Use <tibble>, <data.frame> ",
         "or <matrix> instead with the optional suffix <_inlist>.", 
         call. = FALSE)
    
  }
  
  # Output name
  if (!hasArg(output.name)) {
    output.name <- paste(which, dataset, sep = "_")
  }
  
  #### 
  
  # Pack data
  data_ <- .pack_data(data = data, 
                      data_ = data_, 
                      data_attributes = list(dataset = dataset, 
                                             variables = variables, 
                                             observations = observations), 
                      output.name = output.name, 
                      output.type = output.type, 
                      overwrite = F)
  
  # Return
  return(data_)
  
}


#' Prints or returns data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_data_frame_names <- function(dataset) {
  
  # dataset
  dataset <- get_dataset(dataset)
  
  data_frame_names <- names(Datasets[[dataset]][["Data_frames"]])
  
  # Return
  return(data_frame_names)
  
}



#' Adds new data
#'
#' @param data_frame new data
#' @param name data name
#' @param dataset dataset name
#' name
#'
#' @return
#' @export
#'
#'
save_data_frame <- function(data_frame,
                            name,
                            dataset) {
  
  # Check input
  if (!hasArg(data_frame)) stop("No <data_frame> provided.", call. = FALSE)
  
  if (!hasArg(name)) stop("No <name> argument provided.", call. = FALSE)
  
  
  # Checks dataset
  dataset <- get_dataset(dataset)
  
  # Check data name
  if (name %in% get_data_frame_names(dataset)) 
    warning("Data frame <name> already exists and will be overwritten.", 
            call. = FALSE)
  
  
  # Check that a tibble is added
  if (!tibble::is_tibble(data_frame)) 
    stop("<data_frame> must be a tibble.", call. = FALSE)
  
  
  # Check column names
  if (colnames(data_frame)[1] != "observations")
    stop('First column must be named "observations".', call. = FALSE)
    
  # Check variables
  if (any(!colnames(data_frame)[-1] %in% get_variables(dataset = dataset))) 
    stop(paste0("Following variables do not exist in the dataset:", 
                paste(setdiff(colnames(data_frame)[-1], 
                              get_variables(dataset = dataset)), 
                      collapse = ", ")), call. = FALSE)
  
  # Check observations
  if (any(!data_frame[["observations"]] %in% 
          get_observations(dataset = dataset))) 
      stop(paste0("Following observations do not exist in the dataset:", 
                  paste(setdiff(data_frame[["observations"]], 
                                get_observations(dataset = dataset)), 
                        collapse = ", ")), call. = FALSE)
    
  
  # Save data data
  Datasets[[dataset]][["Data_frames"]][[name]] <<- data_frame
  
  return(invisible(NULL))
  
}
