#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_pca <- function(data_, 
                  input.name, 
                  output.name = "_pca") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Compute PCA
  data.prcomp <- data %>%
    dplyr::select(dplyr::all_of(.data_columns(data, data_attributes))) %>%
    prcomp()
  
  data <- data %>%
    dplyr::select(-dplyr::all_of(.data_columns(data, data_attributes))) %>%
    cbind(data.prcomp[["x"]]) %>% 
    tibble::as_tibble()
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_cor <-function(data_, 
                  x, 
                  y,
                  use = "everything", 
                  method = "pearson", 
                  input.name, 
                  output.name = "_cor") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  if (hasArg(y) && !all(y %in% .data_columns(data_attributes))) {
    col_name <- "rows"
  } else {
    col_name <- switch (colnames(data)[1],
                        variables = "observations", 
                        observations = "variables", 
                        "rows")
  }

    
  # Check input x
  if (!x %in% names(data) || !is.numeric(data[[x]])) {
    stop("<x> must be the name of a numeric column.", 
         call. = FALSE)
  } 
  
  # Check input y
  if (!hasArg(y)) {
    y <- .data_columns(data, data_attributes)
  } else if (any(!y %in% names(data))) {
    stop("<y> arguments: ", paste(y[!y %in% names(data)], collapse = ", "), 
    "were not found in the data.", 
         call. = FALSE)
  } else if(any(!is.numeric(data[[y]]))) {
    stop("<y> must contain names for numeric columns.", 
         call. = FALSE)
  }
      
  # Compute correlations
  data <- purrr::map_dbl(data[y], ~ cor(data[x], 
                                        .x, 
                                        use = use, 
                                        method = method)) %>% 
    tibble::tibble(!!col_name := names(.), cor = .)
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_t.test <-function(data_, 
                     group.column,
                     control.group,
                     paired = F,
                     var.equal = T,
                     p.value.cutoff = 0.05,
                     fc.threshold = 0, 
                     pAdjustMethod = "BH",
                     input.name, 
                     output.name = "_t.test") {
  
  # Check input
  data <- .unpack_data(data_)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Prepare results data frame
  data_t.test <- dplyr::tibble(variables = .data_columns(data, data_attributes),
                               log2.fc = NA_real_,
                               sig.log2.fc = NA,
                               p.value = NA_real_,
                               sig.p.value = NA,
                               significant = NA,
                               regulated = "not")
  
  # Check group information
  if (!hasArg(group.column) || 
      (length(group.column) != 1) || 
      !is.character(group.column)) {
    stop("The <group.column> must be a character indicating which column", 
    " contains information of group membership for each observation.", 
    call. = FALSE)
  }
  
  # Group information
  group_info <- data[[group.column]]
  
  if (length(unique(group_info)) != 2) {
    stop("The <group.column> column must contain only two different groups.", 
         call. = FALSE)
  }
  
  
  if (!hasArg(control.group)) {
    stop("Please specify the <control.group> of your comparison.", 
         call. = FALSE)
  } else if (!control.group %in% group_info) {
    stop("The <group.column> column must contain the <control.group>.", 
         call. = FALSE)
  }
  
  group_info <- factor(group_info, 
                   levels = c(control.group, 
                              setdiff(unique(group_info), control.group)))
  
  
  list_t.test <- list()
  
  
  for (i in data_t.test[["variables"]]) {
    
    # t-test model
    list_t.test[[i]] <- t.test(log2(data[[i]][group_info == levels(group_info)[2]]),
                               log2(data[[i]][group_info == levels(group_info)[1]]),
                               var.equal = var.equal,
                               paired = paired)
    
    # log2 fold-change
    data_t.test[match(i, data_t.test[["variables"]]), "log2.fc"] <-
      log2(mean(data[[i]][group_info == levels(group_info)[2]]) /
             mean(data[[i]][group_info == levels(group_info)[1]]))
    
    # p-value
    data_t.test[match(i, data_t.test[["variables"]]), "p.value"] <-
      list_t.test[[i]]$p.value
    
  }
  
  
  ## Determine significant effects
  
  # Numeric threshold
  if (is.numeric(fc.threshold)) {
    
    # Absolute value given
    if (length(fc.threshold) == 1) {
      
      data_t.test[, "sig.log2.fc"] <- abs(data_t.test[, "log2.fc"]) > fc.threshold
      
      # Values for positive and negative threshold given
    } else {
      
      data_t.test[, "sig.log2.fc"] <-
        ifelse(data_t.test[, "log2.fc"] > 0,
               data_t.test[, "log2.fc"] > fc.threshold[2],
               data_t.test[, "log2.fc"] < fc.threshold[1])
      
    }
    
    
    # Determine significant effect size by confidence interval
  } else if (is.character(fc.threshold) && grep(fc.threshold, "confidence|interval")) {
    
    for (i in data_t.test[["variables"]]) {
      data_t.test[match(i, data_t.test[["variables"]]), "sig.log2.fc"] <-
        sum(list_t.test[[i]]$conf.int > 0) != 1
    }
    
  } else {
    stop('The given <fc.threshold> is not supported. Either provide a numeric threshold (e.g. 1), a vector containing a negative and a positive" threshold (e.g. c(-1, 2)) or indicate "interval".', 
         call. = FALSE)
  }
  
  # p-value
  if (is.character(p.value.cutoff)) stop("Please provide a numeric p-value cutoff.")
  
  # Adjust p-value
  data_t.test <- data_t.test %>%
    # Adjust p-value
    dplyr::mutate(p.adjust = p.adjust(p.value, method = pAdjustMethod),
                  .after = p.value) %>%
    # Significant p-values
    dplyr::mutate(sig.p.value = p.adjust < p.value.cutoff,
                  .after = p.adjust) %>%
    # Final significance
    dplyr::mutate(significant = sig.log2.fc & sig.p.value) %>%
    #
    dplyr::rowwise() %>%
    dplyr::mutate(regulated = if (significant) ifelse(log2.fc > 0, "up", "down")
                  else "not") %>%
    dplyr::ungroup()
  
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data_t.test, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_hclust_x <- function(data_, 
                        distance.method = "euclidean",
                        clustering.method = "complete",
                        input.name, 
                        output.name = "_hclustx") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # ---- Dendrogram of columns  ---- 
  dend <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    .tibble2matrix() %>%
    t() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)
  
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(dend, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_hclust_y <- function(data_, 
                        distance.method = "euclidean",
                        clustering.method = "complete",
                        input.name, 
                        output.name = "_hclusty") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # ---- Dendrogram of columns  ---- 
  dend <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    .tibble2matrix() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)
  
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(data_attributes[["input.name"]], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(dend, data_, data_attributes, output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_reorder_x <- function(data_, 
                         dend = NULL, 
                         input.names, 
                         output.name = "_reorderx") {
  
  # Check input
  data <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data, attributes)
  
  ####
  
  data0 <- data[[1]]
  
  data_order <- data[[2]]
  
  
  # ---- Reorder data based on dendrograms (simplifies analysis later) ----
  if (class(data_order) == "hclust") {
    data0 <- data0 %>%
      dplyr::relocate(c(setdiff(colnames(data0), data_order[["labels"]][data_order[["order"]]]),
                        data_order[["labels"]][data_order[["order"]]]))
  }
  
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(input.names[1], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data0, data_, list(), output.name, overwrite = F)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
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
do_reorder_y <- function(data_, 
                         dend = NULL, 
                         input.names, 
                         output.name = "_reordery") {
  
  # Check input
  data <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data, attributes)
  
  ####
  
  data0 <- data[[1]]
  
  data_order <- data[[2]]
  
  
  # ---- Reorder data based on dendrograms (simplifies analysis later) ----
  if (class(data_order) == "hclust") {
    data0 <- data0 %>%
      dplyr::arrange(match(.[[1]], data_order[["labels"]][data_order[["order"]]]))
  }
  
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    output.name <- paste0(input.names[1], output.name)
  } 
  
  ####
  
  # Prepare return
  data_ <- .pack_data(data0, data_, list(), output.name, overwrite = F)
  
  # Return
  return(data_)
  
}

