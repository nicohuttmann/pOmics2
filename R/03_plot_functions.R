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
plot_nothing <- function(data_, input.name, output.name = "_plot") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Make plot of nothing
  p <- ggplot2::ggplot()
  
  # Literally plot nothing
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data data
#' @param columns which columns to include in the plot
#' @param fontsize size of labels
#' @param linewidth thickness of ellipses
#' @param ref.size size of reference ellipse
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.plot_euler <- function(data, 
                        columns, 
                        fontsize = 8,
                        linewidth = 0.5, 
                        ref.size) {
  
  
  # Check eulerr package
  if (!requireNamespace("eulerr", quietly = TRUE))
    stop("Please install the eulerr package to draw eulerr diagrams.", 
         call. = FALSE)
  
  # Check arguments
  if (!hasArg(columns)) 
    stop("Please specify which <columns> to use.", call. = FALSE)
  if (any(!columns %in% names(data))) 
    stop(paste0("Following <columns> were not found in the data: ", 
                paste(setdiff(columns, names(data)), collapse = ", ")), 
         call. = FALSE)
  if (any(!sapply(data[columns], is.logical))) 
    stop(paste0("Data <columns> must be logical to be interpreted. Following ", 
                "<columns> are not logical: ", 
                paste(names(which(!sapply(data, is.logical))), 
                      collapse = ", ")), call. = FALSE)
  if (any(sapply(data[columns], function(x) any(is.na(x))))) 
      stop("The data must be logical and not contain NAs.", call. = FALSE)
  
  # Select data columns
  data <- data %>% 
    dplyr::select(dplyr::all_of(columns))
  
  # Add reference ellipse
  if (hasArg(ref.size)) {
    if (!is.numeric(ref.size)) stop("Please provide a number for <ref.size>.", 
                                    call. = FALSE)
    
    data <- data %>% 
      dplyr::mutate(reference_channel = 1:nrow(.)) %>% 
      dplyr::full_join(
        y = tibble::tibble(reference_channel = -(1:ref.size), 
                           ref = T), 
        by = "reference_channel") %>% 
      dplyr::mutate(dplyr::across(.fns = ~ ifelse(is.na(.x), F, .x))) %>% 
      dplyr::select(- reference_channel)
  }
  
  
  # Make plot
  p <- data %>%
    eulerr::euler() %>%
    plot(fill = "transparent",
         labels = list(fontsize = fontsize),
         quantities = list(fontsize = fontsize),
         lwd = linewidth)
  
  # Return
  return(p)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param columns which columns to include in the plot
#' @param fontsize size of labels
#' @param linewidth thickness of ellipses
#' @param ref.size size of reference ellipse
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_euler <-function(data_, 
                      columns, 
                      fontsize = 8,
                      linewidth = 0.5, 
                      ref.size, 
                      input.name, 
                      output.name = "_ploteuler") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Make plot
  p <- .plot_euler(data = data, 
                   columns = columns, 
                   fontsize = fontsize,
                   linewidth = linewidth, 
                   ref.size = ref.size)
  
  
  # Print to Plots panel
  plot(p)
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(invisible(data_))
  
}


#' Evaluates data cell-wise
#'
#' @param data data
#' @param columns which columns to include in the plot
#' @param show_percentage 
#' @param digits 
#' @param fill_color 
#' @param fill_alpha 
#' @param stroke_color 
#' @param stroke_alpha 
#' @param stroke_size 
#' @param stroke_linetype 
#' @param set_name_color 
#' @param set_name_size 
#' @param text_color 
#' @param text_size 
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.plot_venn <- function(data, 
                       columns, 
                       show_percentage = FALSE, 
                       digits = 1, 
                       fill_color = c("blue", "yellow", "green", "red"), 
                       fill_alpha = 0.5, 
                       stroke_color = "black", 
                       stroke_alpha = 1, 
                       stroke_size = 1, 
                       stroke_linetype = "solid", 
                       set_name_color = "black", 
                       set_name_size = 6, 
                       text_color = "black", 
                       text_size = 4) {
  
  
  # Check eulerr package
  if (!requireNamespace("ggvenn", quietly = TRUE))
    stop("Please install the ggvenn package to draw Venn diagrams.", 
         call. = FALSE)
  
  # Check arguments
  if (!hasArg(columns)) 
    stop("Please specify which <columns> to use.", call. = FALSE)
  if (any(!columns %in% names(data))) 
    stop(paste0("Following <columns> were not found in the data: ", 
                paste(setdiff(columns, names(data)), collapse = ", ")), 
         call. = FALSE)
  if (any(!sapply(data[columns], is.logical))) 
    stop(paste0("Data <columns> must be logical to be interpreted. Following ", 
                "<columns> are not logical: ", 
                paste(names(which(!sapply(data, is.logical))), 
                      collapse = ", ")), call. = FALSE)
  if (any(sapply(data[columns], function(x) any(is.na(x))))) 
    stop("The data must be logical and not contain NAs.", call. = FALSE)
  
  
  # Select data columns
  data <- data %>% 
    dplyr::select(dplyr::all_of(columns))
  
  
  # Make plot
  p <- data %>%
    ggvenn::ggvenn(columns = NULL, 
                   show_elements = FALSE, 
                   show_percentage = show_percentage, 
                   digits = digits, 
                   fill_color = fill_color, 
                   fill_alpha = fill_alpha, 
                   stroke_color = stroke_color, 
                   stroke_alpha = stroke_alpha, 
                   stroke_size = stroke_size, 
                   stroke_linetype = stroke_linetype, 
                   set_name_color = set_name_color, 
                   set_name_size = set_name_size, 
                   text_color = text_color, 
                   text_size = text_size, 
                   label_sep = ",") 
  
  # Return
  return(p)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param columns which columns to include in the plot
#' @param show_percentage 
#' @param digits 
#' @param fill_color 
#' @param fill_alpha 
#' @param stroke_color 
#' @param stroke_alpha 
#' @param stroke_size 
#' @param stroke_linetype 
#' @param set_name_color 
#' @param set_name_size 
#' @param text_color 
#' @param text_size 
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_venn <- function(data_, 
                     columns, 
                     show_percentage = FALSE, 
                     digits = 1, 
                     fill_color = c("blue", "yellow", "green", "red"), 
                     fill_alpha = 0.5, 
                     stroke_color = "black", 
                     stroke_alpha = 1, 
                     stroke_size = 1, 
                     stroke_linetype = "solid", 
                     set_name_color = "black", 
                     set_name_size = 6, 
                     text_color = "black", 
                     text_size = 4, 
                     input.name, 
                     output.name = "_plotvenn") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Make plot
  p <- .plot_venn(data = data, 
                  columns = columns, 
                  show_percentage = show_percentage, 
                  digits = digits, 
                  fill_color = fill_color, 
                  fill_alpha = fill_alpha, 
                  stroke_color = stroke_color, 
                  stroke_alpha = stroke_alpha, 
                  stroke_size = stroke_size, 
                  stroke_linetype = stroke_linetype, 
                  set_name_color = set_name_color, 
                  set_name_size = set_name_size, 
                  text_color = text_color, 
                  text_size = text_size)
  
  
  # Print to Plots panel
  plot(p)
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(invisible(data_))
  
}


#' Plots numeric values from two columns on x- and y-axis
#'
#'
#' @param data data
#' @param x 
#' @param y 
#' @param labels 
#' @param size 
#' @param color 
#' @param fill 
#' @param shape 
#' @param transparency 
#' @param size.by 
#' @param color.by 
#' @param fill.by 
#' @param shape.by 
#' @param plot.theme 
#' @param use.plotly 
#' @param plotly.text.columns
#' @param x.axis.title 
#' @param y.axis.title 
#' @param label.size 
#' @param label.color 
#' @param legend.title.color 
#' @param legend.title.shape 
#' @param legend.title.fill 
#' @param legend.title.size 
#' @param legend.position 
#' @param legend.rows 
#' @param x.axis.limits vector containing lower and upper x-axis limits
#' @param y.axis.limits vector containing lower and upper y-axis limits
#' @param x.axis.breaks distance between x-axis breaks
#' @param y.axis.breaks distance between y-axis breaks
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param ggrepel.nudge_x 
#' @param ggrepel.box.padding 
#' @param ggrepel.point.padding 
#' @param ggrepel.force 
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.plot_xy <- function(data, 
                     x, 
                     y, 
                     labels, 
                     size = 1, 
                     color = "black", 
                     fill = color, 
                     shape = 16,
                     transparency = 0.8,
                     size.by = NULL,  
                     color.by = NULL,
                     fill.by = color.by,
                     shape.by = NULL, 
                     plot.theme = ggplot2::theme_classic, 
                     use.plotly = F, 
                     plotly.text.columns = c(), 
                     x.axis.title = x, 
                     y.axis.title = y, 
                     legend.title.color = color.by,
                     legend.title.shape = shape.by,
                     legend.title.fill = fill.by,
                     legend.title.size = size.by,
                     legend.position = "right",
                     legend.rows = NULL, 
                     x.axis.limits = NULL,
                     y.axis.limits = NULL,
                     x.axis.breaks = NULL,
                     y.axis.breaks = NULL,
                     aspect.ratio = 1,
                     plot.center = NULL,
                     axis.unit.ratio = NULL,
                     expand.x.axis = c(0, 0),
                     expand.y.axis = c(0, 0), 
                     label.size = 8, 
                     label.color = "black", 
                     ggrepel.nudge_x = 100, 
                     ggrepel.box.padding = 0.3, 
                     ggrepel.point.padding = 0.3, 
                     ggrepel.force = 0.2) {
  
  
  # Check arguments
  if (!hasArg(x) || !hasArg(y)) 
    stop("Please specify which columns to use with <x> and <y>.", call. = FALSE)
  if (any(!c(x, y) %in% names(data))) 
    stop(paste0("Following columns were not found in the data: ", 
                paste(setdiff(c(x, y), names(data)), collapse = ", ")), 
         call. = FALSE)
  if (any(!sapply(data[c(x, y)], is.numeric))) 
    stop(paste0("Data columns must be numeric to be plotted. Following ", 
                "columns are not numeric: ", 
                paste(names(which(!sapply(data[c(x, y)], is.numeric))), 
                      collapse = ", ")), call. = FALSE)
  # if (any(sapply(data[c(x, y)], function(x) any(is.na(x))))) 
  #   stop("The data must be numeric and not contain NAs.", call. = FALSE)
  
  
  # size column
  if (is.null(size.by)) {
    data <- data %>%
      dplyr::mutate(size = "all")
    size.by <- "size"
  } else if (!size.by %in% names(data)) {
    stop("Column <size.by> not found in data.", call. = FALSE)
  } else if (length(unique(data[[size.by]])) > length(size)) {
    stop("Argument <size> must have the same length as different values in column <size.by>.", 
         call. = FALSE)
  }                       
  
  # color column
  if (is.null(color.by)) {
    data <- data %>%
      dplyr::mutate(color = "all")
    color.by <- "color"
  } else if (!color.by %in% names(data)) {
    stop("Column <color.by> not found in data.", call. = FALSE)
  } else if (length(unique(data[[color.by]])) > length(color)) {
    stop("Argument <color> must have the same length as different values in column <color.by>.", 
         call. = FALSE)
  } 
  
  
  # fill column
  if (is.null(fill.by)) {
    data <- data %>%
      dplyr::mutate(fill = "all")
    fill.by <- "fill"
  } else if (!fill.by %in% names(data)) {
    stop("Column <fill.by> not found in data.", call. = FALSE)
  } else if (length(unique(data[[fill.by]])) > length(fill)) {
    stop("Argument <fill> must have the same length as different values in column <fill.by>.", 
         call. = FALSE)
  }
  
  
  # shape column
  if (is.null(shape.by)) {
    data <- data %>%
      dplyr::mutate(shape = "all")
    shape.by <- "shape"
  } else if (!shape.by %in% names(data)) {
    stop("Column <shape.by> not found in data.", call. = FALSE)
  } else if (length(unique(data[[shape.by]])) > length(shape)) {
    stop("Argument <shape> must have the same length as different values in column <shape.by>.", 
         call. = FALSE)
  } 
  
  
  # Make plot
  p <- data %>%
    ggplot(data = data, 
           mapping = aes(x = .data[[x]], 
                         y = .data[[y]], 
                         color = .data[[color.by]],
                         fill = .data[[fill.by]],
                         shape = .data[[shape.by]],
                         size = .data[[size.by]])) +
    geom_point(alpha = transparency) +
    plot.theme() +
    theme(legend.position = legend.position) +
    scale_color_manual(values = color) +
    scale_fill_manual(values = fill) +
    scale_shape_manual(values = shape) +
    scale_size_manual(values = size) + 
    xlab(x.axis.title) +
    ylab(y.axis.title) +
    guides(color = if (length(color) > 1)
      guide_legend(title = legend.title.color,
                   nrow = legend.rows,
                   byrow = FALSE)
      else "none",
      fill = if (length(fill) > 1)
        guide_legend(title = legend.title.fill,
                     nrow = legend.rows,
                     byrow = FALSE)
      else "none",
      shape = if (length(shape) > 1)
        guide_legend(title = legend.title.shape,
                     nrow = legend.rows,
                     byrow = FALSE)
      else "none",
      size = if (length(size) > 1)
        guide_legend(title = legend.title.size,
                     nrow = legend.rows,
                     byrow = FALSE)
      else "none")
  
  
  # Set plot axes size 
  p <- .set_continuous_axes(p = p,
                            x.axis.limits = x.axis.limits,
                            y.axis.limits = y.axis.limits,
                            x.axis.breaks = x.axis.breaks,
                            y.axis.breaks = y.axis.breaks,
                            aspect.ratio = aspect.ratio,
                            plot.center = plot.center,
                            axis.unit.ratio = axis.unit.ratio,
                            expand.x.axis = expand.x.axis,
                            expand.y.axis = expand.y.axis)
  
  # Label points 
  if (hasArg(labels)) {
    # Check ggrepel package
    if (!requireNamespace("ggrepel", quietly = TRUE))
      stop("Please install the ggrepel package to add labels.", 
           call. = FALSE)
    # Check label input
    if (!labels %in% names(data)) stop("Column for <labels> not found in the data.", call. = FALSE)
    # Add labels
    p <- p + 
      ggrepel::geom_text_repel(mapping = aes(label = .data[[labels]]), 
                               nudge_x = ggrepel.nudge_x, 
                               box.padding = ggrepel.box.padding, 
                               point.padding = ggrepel.point.padding, 
                               force = ggrepel.force, 
                               na.rm = T, 
                               size = label.size / ggplot2::.pt, 
                               color = label.color)
  }
  
  
  # Use plotly
  if (use.plotly) {
    
    if (length(plotly.text.columns) > 0) {
      
      # Test input
      if (any(!plotly.text.columns %in% colnames(p[["data"]]))) {
        stop("All <plotly.text.columns> must be present in data.", call. = F)
      }
      
      # Combine columns to text
      p[["data"]] <- p[["data"]] %>% 
        dplyr::mutate(plotly_text = !!dplyr::sym(plotly.text.columns[1]))
      
      # More than one column given
      if (length(plotly.text.columns) > 1) {
        for (i in 2:length(plotly.text.columns)) {
          p[["data"]] <- p[["data"]] %>% 
            dplyr::mutate(plotly_text = 
                            paste0(plotly_text, 
                                   "\n", 
                                   !!dplyr::sym(plotly.text.columns[i])))
        }
      }
      
      # Add text aestetics
      p <- p + ggplot2::aes(text = plotly_text)
      
      # Transform ggplot to plotly
      p <- plotly::ggplotly(p, tooltip = c("text", "x", "y"))
      
    }
    
    # Just transform to plotly
    p <- plotly::ggplotly(p, tooltip = c("x", "y"))
    
  }
  
  
  # Return
  return(p)
  
}


#' Plots numeric values from two columns on x- and y-axis
#'
#' @param data_ data list
#' @param x 
#' @param y 
#' @param labels 
#' @param size 
#' @param color 
#' @param fill 
#' @param shape 
#' @param transparency 
#' @param size.by 
#' @param color.by 
#' @param fill.by 
#' @param shape.by 
#' @param plot.theme 
#' @param use.plotly 
#' @param plotly.text.columns
#' @param x.axis.title 
#' @param y.axis.title 
#' @param label.size 
#' @param label.color 
#' @param legend.title.color 
#' @param legend.title.shape 
#' @param legend.title.fill 
#' @param legend.title.size 
#' @param legend.position 
#' @param legend.rows 
#' @param x.axis.limits vector containing lower and upper x-axis limits
#' @param y.axis.limits vector containing lower and upper y-axis limits
#' @param x.axis.breaks distance between x-axis breaks
#' @param y.axis.breaks distance between y-axis breaks
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param ggrepel.nudge_x 
#' @param ggrepel.box.padding 
#' @param ggrepel.point.padding 
#' @param ggrepel.force 
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_xy <- function(data_, 
                    x, 
                    y, 
                    labels, 
                    size = 1, 
                    color = "black", 
                    fill = color, 
                    shape = 16,
                    transparency = 0.8,
                    size.by = NULL,  
                    color.by = NULL,
                    fill.by = color.by,
                    shape.by = NULL, 
                    plot.theme = ggplot2::theme_classic, 
                    use.plotly = F, 
                    plotly.text.columns = c(), 
                    x.axis.title = x, 
                    y.axis.title = y, 
                    label.size = 8, 
                    label.color = "black", 
                    legend.title.color = color.by,
                    legend.title.shape = shape.by,
                    legend.title.fill = fill.by,
                    legend.title.size = size.by,
                    legend.position = "right",
                    legend.rows = NULL, 
                    x.axis.limits = NULL,
                    y.axis.limits = NULL,
                    x.axis.breaks = NULL,
                    y.axis.breaks = NULL,
                    aspect.ratio = 1,
                    plot.center = NULL,
                    axis.unit.ratio = NULL,
                    expand.x.axis = c(0, 0),
                    expand.y.axis = c(0, 0), 
                    ggrepel.nudge_x = 100, 
                    ggrepel.box.padding = 0.3, 
                    ggrepel.point.padding = 0.3, 
                    ggrepel.force = 0.2, 
                    input.name, 
                    output.name = "_plotxy") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Make plot
  p <- .plot_xy(data = data, 
                x = x, 
                y = y, 
                labels = labels, 
                size = size, 
                color = color, 
                fill = fill, 
                shape = shape,
                transparency = transparency,
                size.by = size.by,  
                color.by = color.by,
                fill.by = fill.by, # if problem then color.by
                shape.by = shape.by, 
                plot.theme = plot.theme, 
                use.plotly = use.plotly, 
                plotly.text.columns = plotly.text.columns, 
                x.axis.title = x.axis.title, 
                y.axis.title = y.axis.title, 
                label.size = label.size, 
                label.color = label.color, 
                legend.title.color = legend.title.color,
                legend.title.shape = legend.title.shape,
                legend.title.fill = legend.title.fill,
                legend.title.size = legend.title.size,
                legend.position = legend.position,
                legend.rows = legend.rows, 
                x.axis.limits = x.axis.limits,
                y.axis.limits = y.axis.limits,
                x.axis.breaks = x.axis.breaks,
                y.axis.breaks = y.axis.breaks,
                aspect.ratio = aspect.ratio,
                plot.center = plot.center,
                axis.unit.ratio = axis.unit.ratio,
                expand.x.axis = expand.x.axis,
                expand.y.axis = expand.y.axis)
  
  
  # Print to Plots panel
  print(p)
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(invisible(data_))
  
}


#' Plots numeric values from two columns on x- and y-axis
#'
#' @param data_ data list
#' @param x 
#' @param y 
#' @param labels 
#' @param size 
#' @param color 
#' @param fill 
#' @param shape 
#' @param transparency 
#' @param size.by 
#' @param color.by 
#' @param fill.by 
#' @param shape.by 
#' @param plot.theme 
#' @param x.axis.title 
#' @param y.axis.title 
#' @param label.size 
#' @param label.color 
#' @param legend.title.color 
#' @param legend.title.shape 
#' @param legend.title.fill 
#' @param legend.title.size 
#' @param legend.position 
#' @param legend.rows 
#' @param x.axis.limits vector containing lower and upper x-axis limits
#' @param y.axis.limits vector containing lower and upper y-axis limits
#' @param x.axis.breaks distance between x-axis breaks
#' @param y.axis.breaks distance between y-axis breaks
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#' @param ggrepel.nudge_x 
#' @param ggrepel.box.padding 
#' @param ggrepel.point.padding 
#' @param ggrepel.force 
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_pca <- function(data_, 
                     x = "PC1", 
                     y = "PC2", 
                     include.variance = T,
                     include.ellipses = F,
                     labels, 
                     size = 1, 
                     color = "Set1", 
                     fill = color, 
                     shape = 16,
                     transparency = 0.8,
                     size.by = NULL,  
                     color.by = "observations",
                     fill.by = color.by,
                     shape.by = NULL,
                     ellipse.size = 1,
                     ellipse.transparency = 1,
                     ellipse.linetype = 1,
                     plot.theme = ggplot2::theme_classic, 
                     x.axis.title = x, 
                     y.axis.title = y, 
                     label.size = 8, 
                     label.color = "black", 
                     legend.title.color = color.by,
                     legend.title.shape = shape.by,
                     legend.title.fill = fill.by,
                     legend.title.size = size.by,
                     legend.position = "right",
                     legend.rows = NULL, 
                     x.axis.limits = NULL,
                     y.axis.limits = NULL,
                     x.axis.breaks = NULL,
                     y.axis.breaks = NULL,
                     aspect.ratio = 1,
                     plot.center = NULL,
                     axis.unit.ratio = 1,
                     expand.x.axis = c(0, 0),
                     expand.y.axis = c(0, 0), 
                     ggrepel.nudge_x = 100, 
                     ggrepel.box.padding = 0.3, 
                     ggrepel.point.padding = 0.3, 
                     ggrepel.force = 0.2, 
                     input.name, 
                     output.name = "_plotpca") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Define colors automatically
  if (length(color) == 1 && color %in% rownames(RColorBrewer::brewer.pal.info)) {
    color <- RColorBrewer::brewer.pal(max(c(3, length(unique(data[[color.by]])))), 
                                      name = color)
  }
  
  # Make plot
  p <- .plot_xy(data = data, 
                x = x, 
                y = y, 
                labels = labels, 
                size = size, 
                transparency = transparency,
                color = color, 
                fill = fill, 
                shape = shape,
                size.by = size.by,  
                color.by = color.by,
                fill.by = fill.by,
                shape.by = shape.by, 
                plot.theme = plot.theme, 
                x.axis.title = x.axis.title, 
                y.axis.title = y.axis.title, 
                label.size = label.size, 
                label.color = label.color, 
                legend.title.color = legend.title.color,
                legend.title.shape = legend.title.shape,
                legend.title.fill = legend.title.fill,
                legend.title.size = legend.title.size,
                legend.position = legend.position,
                legend.rows = legend.rows, 
                x.axis.limits = x.axis.limits,
                y.axis.limits = y.axis.limits,
                x.axis.breaks = x.axis.breaks,
                y.axis.breaks = y.axis.breaks,
                aspect.ratio = aspect.ratio,
                plot.center = plot.center,
                axis.unit.ratio = axis.unit.ratio,
                expand.x.axis = expand.x.axis,
                expand.y.axis = expand.y.axis)
  
  # Include ellipses 
  if (include.ellipses) {
    p <- p + ggplot2::stat_ellipse(size = ellipse.size,
                          alpha = ellipse.transparency,
                          linetype = ellipse.linetype)
  }
  
  # Show variance on axis titles
  if (include.variance) {
    
    data_var <- dplyr::select(data, 
                              dplyr::all_of(.data_columns(data, 
                                                          data_attributes))) %>% 
      dplyr::select(where(is.numeric)) %>% 
      apply(2, sd)
    
    p <- p +
      xlab(paste0(
        x, " (", round(
          100 * data_var[[x]]^2 / sum(data_var^2),
          digits = 1), "%)")) +
      ylab(paste0(
        y, " (", round(
          100 * data_var[[y]]^2 / sum(data_var^2),
          digits = 1), "%)"))
    
  }
  
  # Print to Plots panel
  plot(p)
  
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, 
                      overwrite = T, 
                      last.entry = data_attributes[["input.name"]])
  
  # Return
  return(invisible(data_))
  
}


#' Plots a volcano plot with ggplot2
#'
#' @param data_ data_
#' @param color column for color of points
#' @param p.value.cutoff p-value limit for coloring
#' @param pos.log2fc.cutoff positive log2 fold-change limit for coloring
#' @param neg.log2fc.cutoff negative log2 fold-change limit for coloring
#' @param highlight.variables variables to highlight by point.size
#' @param highlight.color color to use to highlight proteins
#' @param x.axis.title title of x-axis
#' @param y.axis.title title of y-axis
#' @param text.size size of text in points (5-8)
#' @param text.color color of text
#' @param point.size point size (0.5-2)
#' @param point.alpha transparency (0-1)
#' @param highlight.point.size size of point of highlighted variables
#' @param highlight.point.alpha transparency of points to be highlighted
#' @param x.axis.breaks break size between ticks of x-axis
#' @param y.axis.breaks break size between ticks of y-axis
#' @param axis.line.size width of axes lines
#' @param axis.color color of axes lines
#' @param axis.ticks.size width of axis ticks
#' @param axis.title.size size of axis title
#' @param axis.text.size size of axis labels
#' @param aspect.ratio y/x ratio
#' @param use.plotly make interactive plots with ggplotly() (default: T if html
#' document)
#' @param view view plot
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
#'
plot_volcano <- function(data_,
                         color = "regulated",
                         p.value.cutoff = 0.05,
                         pos.log2fc.cutoff = 0,
                         neg.log2fc.cutoff = 0,
                         highlight.variables = NULL,
                         highlight.color = NULL,
                         x.axis.title = "log2 fold-change",
                         y.axis.title = "-log10(p-value)",
                         text.size = 6,
                         text.color = "black",
                         point.size = 2,
                         point.alpha = 0.8,
                         highlight.point.size = 3,
                         highlight.point.alpha = 0.8,
                         x.axis.breaks = 1,
                         y.axis.breaks = 1,
                         axis.line.size = 0.5,
                         axis.color = "black",
                         axis.ticks.size = 0.3,
                         axis.title.size = 8,
                         axis.text.size = 6,
                         aspect.ratio = 0.8,
                         use.plotly = F,
                         input.name, 
                         output.name = "plot_volcano") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Add color column
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::rename(color = !!color) %>%
    {if (!is.null(highlight.color))
      dplyr::mutate(., color = ifelse(variables %in% highlight.variables,
                                      "highlight",
                                      color))
      else .} %>%
    dplyr::mutate(point.size =
                    ifelse(variables %in% highlight.variables,
                           highlight.point.size,
                           point.size)) %>%
    dplyr::mutate(alpha =
                    ifelse(variables %in% highlight.variables,
                           highlight.point.alpha,
                           point.alpha)) %>%
    dplyr::arrange(point.size, -p.value)
  
  
  
  # Add text column for interactive plots
  if (use.plotly) {
    data$text <- paste(p2g(data$variables),
                       p2n(data$variables),
                       data$variables, sep = "\n")
  } else {
    #data$text <- p2g(data$variables)
    data$text <- "" #data$variables
  }
  
  
  # Line width factor
  lwf <- 1 / (ggplot2::.pt * 72.27 / 96)
  
  
  p <- ggplot(data = data,
              aes(x = log2.fc,
                  y = -log10(p.value),
                  col = color,
                  size = point.size,
                  text = text,
                  alpha = alpha)) +
    geom_point(shape = 16, stroke = 0) +
    scale_size_continuous(range = range(data$point.size)) +
    scale_alpha_identity() +
    theme(aspect.ratio = aspect.ratio,
          axis.line = element_line(size = 0.5 * lwf, color = axis.color),
          panel.background = element_blank(),
          axis.text = element_text(size = axis.text.size, color = text.color),
          axis.ticks = element_line(size = axis.ticks.size * lwf,
                                    color = axis.color),
          axis.title = element_text(size = axis.title.size, color = text.color),
          legend.position = 0) +
    scale_color_manual(values = c("highlight" = highlight.color,
                                  "up" = "red",
                                  "down" = "blue",
                                  "not" = "grey")) +
    scale_x_continuous(
      limits = .axis_limit_breaks(plot.limits = range(data$log2.fc),
                                  break.size = x.axis.breaks)$limits,
      breaks = .axis_limit_breaks(plot.limits = range(data$log2.fc),
                                  break.size = x.axis.breaks)$breaks,
      expand = c(0, 0)) +
    scale_y_continuous(
      limits = .axis_limit_breaks(plot.limits = range(-log10(data$p.value)),
                                  break.size = y.axis.breaks)$limits,
      breaks = .axis_limit_breaks(plot.limits = range(-log10(data$p.value)),
                                  break.size = y.axis.breaks)$breaks,
      expand = c(0, 0)) +
    xlab(x.axis.title) +
    ylab(y.axis.title)
  
  
  # Generate interactive plot with ggplotly()
  if (use.plotly) {
    p <- plotly::ggplotly(p, tooltip = "text")
  }
  
  # Print to Plots panel
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, 
                      overwrite = T, 
                      last.entry = data_attributes[["input.name"]])
  
  # Return
  return(invisible(data_))
  
}


.plot_vsn_meanSdPlot <- function(data, log2 = F) {
  
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
  
  if (log2) data_m <- log2(data_m)
  
  p <- vsn::meanSdPlot(data_m, plot = F)$gg
  
  # Return data
  return(p)
  
}


.plot_vsn_meanSdPlot_m <- function(data_list, log2 = F, adjust.limits = T) {
  
  # Plot individual datasets
  plot_list <- purrr::map(data_list, ~ .plot_vsn_meanSdPlot(.x, log2 = log2))
  
  # Set same limits for all plots
  if (adjust.limits) {
    
    limits_list <- purrr::map(plot_list, ~ .get_plot_limits(.x))
    
    limits_df <- list2DF(limits_list)
    
    limits <- c(xmin = min(unlist(limits_df[1, ])), 
                xmax = max(unlist(limits_df[2, ])), 
                ymin = min(unlist(limits_df[3, ])), 
                ymax = max(unlist(limits_df[4, ])))
    
    plot_list <- purrr::map(
      plot_list, 
      ~ .x + 
        ggplot2::scale_x_continuous(limits = limits[1:2]) +
        ggplot2::scale_y_continuous(limits = limits[3:4]))
  }
  
  # Name individual plots
  for (i in names(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] + ggplot2::ggtitle(i)
  }
  
  # Assemble plots
  p <- patchwork::wrap_plots(plot_list)
  
  # Return plot 
  return(p)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param log2 
#' @param add.title 
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_vsn_meanSdPlot <- function(data_, 
                                log2 = F, 
                                add.title = T, 
                                input.name, 
                                output.name = "_plot_vsn_meanSdPlot") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  if (!hasArg(input.name)) 
    input.name <- data_attributes[["input.name"]]
  
  ####
  
  
  # Make plot
  p <- .plot_vsn_meanSdPlot(data, log2 = log2)
  if (add.title) 
    p <- p + ggplot2::ggtitle(ifelse(is.null(names(input.name)), 
                                     input.name, 
                                     names(input.name)))
  
  # Plot
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param log2 
#' @param add.title 
#' @param input.names if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_vsn_meanSdPlot_m <- function(data_, 
                                log2 = F, 
                                input.names, 
                                output.name = "_plot_vsn_meanSdPlot_m") {
  
  # Check input
  data_list <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data_list, attributes)
  if (!is.null(names(input.names))) 
    names(data_list) <- names(input.names)
  
  ####
  
  
  # Make plot
  p <- .plot_vsn_meanSdPlot_m(data_list, log2 = log2)
  
  # Plot
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    #if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(paste(input.names, collapse = "_"), output.name)
   # else
   #   output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, 
                      data_, 
                      .merge_data_attributes(data_attributes_list), 
                      output.name, 
                      overwrite = T)
  
  # Return
  return(data_)
  
}


#' Title
#'
#' @param data data frame 
#' @param mapping define the variable mapping with the aes() function
#' @param ... ggplot functions such as geom_s, themes, etc.
#'
#' @return
#'
#'
#'
.plot_gg <- function(data, mapping = aes(), ...) {
  
  
  p <- ggplot2::ggplot(data = data, 
                       mapping = {{mapping}})
  
  for (i in list(...)) {
    p <- p + i
  }
  
  # Return plot
  return(p)
  
}



#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param mapping define the variable mapping with the aes() function
#' @param ... ggplot functions such as geom_s, themes, etc.
#' @param pivot.longer should the data be transformed to long format
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_gg <- function(data_, 
                    mapping = aes(), 
                    ..., 
                    pivot.longer = T, 
                    input.name, 
                    output.name = "_plot_gg") {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  # Pivot data
  if (pivot.longer) {
    data <- tidyr::pivot_longer(data, 
                              cols = .data_columns(data, 
                                                   data_attributes))
  }
  
  
  # Make plot
  p <- .plot_gg(data, mapping = mapping, ...)
  
  # Literally plot nothing
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(data_attributes[["input.name"]], output.name)
    else
      output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}


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
plot_patchwork <- function(data_, 
                           ncol = NULL, 
                           nrow = NULL, 
                           byrow = NULL, 
                           widths = NULL, 
                           heights = NULL, 
                           guides = NULL, 
                           tag_level = NULL, 
                           design = NULL,
                           input.names, 
                           output.name = "_patchwork") {
  
  # Check input
  data_list <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data_list, attributes)
  if (!is.null(names(input.names))) 
    names(data_list) <- names(input.names)
  
  ####
  
  # Make plot of nothing
  p <- patchwork::wrap_plots(data_list, 
                             ncol = ncol, 
                             nrow = nrow, 
                             byrow = byrow, 
                             widths = widths, 
                             heights = heights, 
                             guides = guides, 
                             tag_level = tag_level, 
                             design = design)
  
  # Literally plot nothing
  plot(p)
  
  # Output name
  if (substr(output.name, 1, 1) == "_") {
    #if (getOption("pOmics2_list_long_names"))
      output.name <- paste0(paste(input.names, collapse = "_"), output.name)
    #else
    #  output.name <- paste0(data_attributes[["input.position"]], output.name)
  }
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes_list, output.name, overwrite = T)
  
  # Return
  return(data_)
  
}
  
  
  
  
  