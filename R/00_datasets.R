#' Title
#'
#' @param variables_data_frame tibble with "variables" as first column and 
#' additional after
#' @param observations_data tibble with "observations" as first column and 
#' additional after
#' @param data_frame_list named list of data frames
#'
#' @return
#' @export
#'
#' @examples
new_dataset <- function(variables_data_frame, 
                        observations_data, 
                        data_frame_list) {
  
  # Put dataset together
  dataset <- list(Variables = variables_data_frame, 
                  Observations = observations_data, 
                  Data_frames = data_frame_list)
  
  
  # Test stuff
  
  
  # Return
  return(dataset)
  
}


#' Title
#'
#' @param dataset dataset list
#' @param name name of dataset
#'
#' @return
#' @export
#'
#' @examples
save_dataset <- function(dataset, name) {
  
  # Check input
  if (!hasArg(dataset)) 
    stop("No <dataset> provided.")
  
  if (!hasArg(name)) 
    stop("Please provide a <name> for the dataset.", call. = FALSE)
  
  
  # Check if name is already present in datasets
  if (name %in% names(Datasets)) 
    warning("Dataset name exists already.", call. = FALSE)
  
  # Check dataset structure
  # if (!check_dataset(dataset))
  #   stop("")
  
  
  # Add dataset
  Datasets[[name]] <<- dataset
  
  # Indicate if new info list was created
  return(invisible(TRUE))
  
}


#' Title
#'
#' @param raw.data 
#' @param variable.identifiers 
#' @param variables.data 
#' @param observations 
#' @param data.frames 
#'
#' @return
#' @export
#'
#' @examples
import2new_dataset <- function(raw.data = NULL, 
                               variable.identifiers, 
                               variables.data = character(), 
                               observations = character(), 
                               data.frames = character()) {
  
  # ---- Check which inputs exist and notify user 
  if (is.null(raw.data)) 
    warning("No raw data supplied.", 
            call. = FALSE)
  
  if (missing(variable.identifiers)) 
    warning("No column or expression for variable identifiers provided. Row indices will be used as identifier.", 
            call. = FALSE)
  
  if (!is.character(variables.data)) 
    stop("The variables.data argument only accepts a string or a vector of strings with column names.")
  
  if (!is.character(observations))
    stop("The observations argument only accepts vector of strings with observation names.")
  
  
  # ---- Build variables data frame with identifiers of variables
  
  # Identifiers
  if (!missing(variable.identifiers)) {
    
    # Check if identifiers can be made from input
    variable.ids <- 
      tryCatch(expr = 
                 {
                   raw.data %>% 
                     dplyr::mutate(variables = !!dplyr::enquo(variable.identifiers)) %>% 
                     dplyr::pull("variables")
                 }, 
               error = function(cond) NULL)
    # 
    if (is.null(variable.ids)) {
      stop("The variable.identifiers input did not work. Try a different tidy-friendly input.", 
           call. = FALSE)
    }
  } else {
    variable.ids <- as.character(1:nrow(raw.data))
  }
  
  # Check if variables are unique
  if (nrow(raw.data) != length(unique(variable.ids))) 
    stop("Declared identifiers are not unique and must be changed. A different column can be declared or several columns combined.", 
         call. = FALSE)
  
  # Begin building variables.data frame with variable ids                     
  variables_data_frame <- tibble::tibble(variables = as.character(variable.ids))
  
  # Add variables.data columns
  if (length(variables.data) > 0) {
    variables.data.names <- intersect(variables.data, colnames(raw.data))
    
    if (length(variables.data) != length(variables.data.names))
      warning("Following variables.data columns were not found: ", 
              paste(setdiff(variables.data, variables.data.names), 
                    collapse = ", "), 
              call. = FALSE)
    
    for (i in variables.data.names) {
      variables_data_frame <- variables_data_frame %>% 
        dplyr::mutate(!!i := raw.data[[i]])
    }
  }
  
  
  # ---- Observations 
  # Give a warning if any observations cannot be matched in the raw data
  if (length(observations) > 0 && !is.null(raw.data)) {
    
    observations_found <- purrr::map_lgl(observations, 
                                         ~ any(grepl(.x, names(raw.data))))
    
    if (any(!observations_found)) {
      warning("Following observations were not found in the column names of the raw data: ", 
              paste(observations[!observations_found], collapse = "; "), 
              call. = FALSE)
    } else {
      observations_data <- tibble::tibble(observations = observations)
    }
  } else {
    observations_data <- tibble::tibble()
  }
  
  
  # Extract data frames based on data column names and observations
  data_frame_list <- list()
  
  if (length(data.frames) > 0) {
    
    # Check if observations are given
    if (length(observations) == 0) 
      stop("Observation names are needed to extract data frames from the raw data.", 
           call. = FALSE)
    
    # Extract data frames from raw.data
    for (i in data.frames) {
      observation_cols <- purrr::map(observations, 
                                     ~ which(grepl(.x, names(raw.data)))) %>% 
        unlist()
      
      data_frame_cols <- which(grepl(i, names(raw.data)))
      
      cols_to_extract <- intersect(observation_cols, data_frame_cols)
      
      
      if (length(cols_to_extract) == 0) 
        stop("No columns matching the data frame column name ", i, " found.", 
             call. = FALSE)
      
      # Transpose tibble, add variable ids as colnames and 
      # add observations column
      
      data_frame_add <- raw.data[cols_to_extract] %>% 
        as.matrix() %>% 
        t()
      
      colnames(data_frame_add) <- variable.ids
      
      # observations_order <- 
      #   purrr::map_int(observations, 
      #                  ~ which(grepl(.x, rownames(data_frame_add))))
      
      data_frame_list[[i]] <- data_frame_add %>% 
        tibble::as_tibble() %>% 
        dplyr::mutate(observations = observations, # observations[observations_order]
                      .before = 1)
      
    }
    
  }
  
  # Put dataset together
  dataset <- new_dataset(variables_data_frame = variables_data_frame, 
                         observations_data = observations_data, 
                         data_frame_list = data_frame_list)
  
  return(dataset)
  
}



#' Checks and returns correct dataset identifier
#'
#' @param dataset dataset name 
#'
#' @return
#' @export
#'
#'
get_dataset <- function(dataset) {
  
  # Check if Datasets list exist
  if (!exists("Datasets")) 
    stop("There must be a list named Datasets.", call. = FALSE)
  
  # Default if not given
  if (!hasArg(dataset) && length(Datasets) == 1) {
    
    dataset <- names(Datasets)
    
    return(dataset)
    
  } else if (!hasArg(dataset)) {
    stop("Please specify a dataset.", call. = FALSE)
  }
  
  # Name correct
  if (dataset %in% names(Datasets))
    return(dataset)

  # Incorrect dataset
  stop("Dataset could not be found.", call. = FALSE)
  
}


#' Prints or returns datasets
#'
#' @return
#' @export
#'
#'
get_dataset_names <- function() {
  
  # Return
  return(names(Datasets))
  
}


#' Imports any file type using the file extension and returns list
#'
#' @param files file paths
#' @param dir directory to import files from
#' @param ext specific file extensions to imports
#' @param silent suppresses messages
#'
#' @importFrom magrittr %>% 
#'
#' @return
#' @export
#'
#'
import_files <- function(files, silent = F) {
  
  # Select files if no path given
  if (!hasArg(files) & !hasArg(dir)) {
    
    stop("File names(s) have to be specified.")
    
  } 
  
  
  # Create list to store imported files
  list.import <- list()
  
  
  
  # Import all files
  for (i in seq_along(files)) {
    
    # Import data file
    if (silent) data <- suppressWarnings(
      suppressMessages(
        vroom::vroom(file = files[i])))
    
    else data <- suppressWarnings(vroom::vroom(file = files[i]))
    
    
    # Rename columns to avoid spaces
    names(data) <- gsub(pattern = " ", replacement = ".", names(data))
    names(data) <- gsub(pattern = "\\.+", replacement = "\\.", names(data))
    
    
    
    # Add file to list
    list.import[[length(list.import) + 1]] <- tibble::as_tibble(data)
    
    
  }
  
  # Modify file names
  if (!is.null(names(files))) {
    if (any(duplicated(names(files)))) {
      stop("Names of the files argument vector must be unique.")
    } else {
      file.names <- names(files)
    }
  } else {
    file.names <- files %>%
    strsplit(split = "/") %>% 
    lapply(function(x) x[length(x)]) %>% 
      unlist() %>%
      tools::file_path_sans_ext()
    # Check if names are unique
    if (any(duplicated(file.names))) {
      stop("File names are not unique. ", 
           "Specify the names for by naming the files vector.", 
           call. = FALSE)
    }
  }
  
  
  # Add names
  names(list.import) <- file.names
  
  # Add imported files to .import list
  .save_import(list.import)
  
  # Return list or one data frame
  return(invisible(list.import))
  
}


#' Saves imported files to .imports list
#'
#' @param import list of imported files
#'
#' @return
#' @export
#'
#'
.save_import <- function(import) {
  
  # Check for imports list
  if (exists("Info")) {
    if("Imports" %in% names(Info)) {
    } else {
      stop("The <Imports> entry does not exist in the <Info> list. Create it with: 
           Info$Import <- list()", 
           call. = FALSE)
    }
  } else {
    stop("The <Info> list does not exist. Create it with:
         Info <- list(Imports = list())", 
         call. = FALSE)
  }
  
  # All imported files in list
  for (i in seq_along(import)) {
    
    Info[["Imports"]][[names(import)[i]]] <<- import[[i]]
    
  }
  
}


#' Title
#'
#' @param data.frame 
#' @param data.name 
#'
#' @return
#' @export
#'
#' @examples
data_frame2new_dataset <- function(data_frame, data.name) {
  
  # Check input
  if (!hasArg(data_frame)) stop("No <data_frame> provided.", 
                                call. = FALSE)
  
  
  # Check input type for R7 class
  ###
  
  
  # Check for columns (assumes "observations" column)
  if (names(data_frame)[1] != "observations") 
    stop("First column must be called 'observations'.", 
         call. = FALSE)
  
  data_frame_list <- list()
  data_frame_list[[data.name]] <- data_frame
  
  # Make new dataset
  dataset <- new_dataset(variables_data_frame = 
                           tibble(variables = names(data_frame)[-1]), 
                         observations_data = 
                           tibble(observations = as.character(data_frame[["observations"]])), 
                         data_frame_list = 
                           data_frame_list)

  return(dataset)
  
}



