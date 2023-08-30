#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_ComplexHeatmap <-function(data_, 
                               dend_x, 
                               dend_y, 
                               ..., 
                               input.name, 
                               output.name = "_CompHeatm") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Define arguments for heatmap
  hm_args <- list()
  hm_args[["matrix"]] <- .tibble2matrix(data)
  
  input_args <- list(...)
  
  
  ## Precomputed dendrograms
  
  # Columns
  if (hasArg(dend_x)) {
    hm_args[["cluster_columns"]] <- .unpack_data(data_, dend_x)
  }
  
  # Rows
  if (hasArg(dend_y)) {
    hm_args[["cluster_rows"]] <- .unpack_data(data_, dend_y)
  }
  
  
  #### Extract and modify arguments from input
  
  
  
  # Row names
  if (!is.null(input_args[["show_row_names"]])) {
    hm_args[["show_row_names"]] <- input_args[["show_row_names"]]
  } else {
    if (dim(data)[1] > 21) hm_args[["show_row_names"]] <- FALSE
  }
  
  # Column names
  if (!is.null(input_args[["show_column_names"]])) {
    hm_args[["show_column_names"]] <- input_args[["show_column_names"]]
  } else {
    if (dim(data)[2] > 20) hm_args[["show_column_names"]] <- FALSE
  }
  
  
  
  # Assemble arguments
  for (i in names(input_args)) {
    if (!i %in% names(hm_args)) {
      hm_args[[i]] <- input_args[[i]]
    }
  }
  
  # Make plot of nothing
  p <- do.call(ComplexHeatmap::Heatmap, 
               args = hm_args)
  
  # Literally plot nothing
  ComplexHeatmap::draw(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(invisible(data_))
  
}





