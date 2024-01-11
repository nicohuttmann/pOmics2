#' Returns a randomly generated vector of colors sorted by hue
#'
#' @param n 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
rcol <- function(n = 1, seed = 42) {
  
  set.seed(seed)
  
  cols <- sample(x = colours(), size = n, replace = F)
  
  cols_h <- cols[order(rgb2hsv(col2rgb(cols))[1, ])]
  
  return(cols)
}


#' Helps make nice axis limit breaks
#'
#' @param plot.limits vector containing plot limits
#' @param break.size space between breaks
#'
#' @return
#' @export
#'
#'
.axis_limit_breaks <- function(plot.limits, break.size = NULL) {


  output <- list()

  # find break size automatically (target = 5 breaks)
  if (is.null(break.size)) {
    opt_break_size <- c(50, 20, 10, 5, 2, 1, 0.5, 0.2, 0.1)
    limit_range <- plot.limits[2] - plot.limits[1]

    mag <- round(log10(limit_range)) - 1
    n_breaks <- limit_range / (opt_break_size * 10^mag)
    opt_break_size <- opt_break_size[n_breaks >= 3]
    n_breaks <- n_breaks[n_breaks >= 3]

    best_break <- which(c(min(abs(4 - n_breaks)) == abs(4 - n_breaks)))[1]
    break.size <- opt_break_size[best_break] * 10^mag
  }

  output[["limits"]] <- c(floor(plot.limits[1] / break.size) * break.size,
                          ceiling(plot.limits[2] / break.size) * break.size)

  output[["breaks"]] <- seq(output[["limits"]][1],
                            output[["limits"]][2],
                            break.size)

  # Return
  return(output)

}

#' Title
#'
#' @param plot 
#'
#' @return
#' @export
#'
#' 
.get_plot_limits <- function(plot) {
  gb <- ggplot2::ggplot_build(plot)
  xmin <- gb$layout$panel_params[[1]]$x.range[1]
  xmax <- gb$layout$panel_params[[1]]$x.range[2]
  ymin <- gb$layout$panel_params[[1]]$y.range[1]
  ymax <- gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

#' Defines ggplot panel by ratio, size, unit size, center, and axis breaks
#'
#' @param p plot
#' @param x.axis.limits vector containing lower and upper x-axis limits
#' @param y.axis.limits vector containing lower and upper y-axis limits
#' @param x.axis.breaks distance between x-axis breaks
#' @param y.axis.breaks distance between y-axis breaks
#' @param aspect.ratio absolute length of x-axis/y-axis
#' @param plot.center vector for center of plot
#' @param axis.unit.ratio ratio between x- and y-axis units
#' @param expand.x.axis expand x.axis (see scale_x_continuous)
#' @param expand.y.axis expand x.axis (see scale_y_continuous)
#'
#' @return
#' @export
#'
#'
.set_continuous_axes <- function(p,
                                 x.axis.limits = NULL,
                                 y.axis.limits = NULL,
                                 x.axis.breaks = NULL,
                                 y.axis.breaks = NULL,
                                 aspect.ratio = 1,
                                 plot.center = NULL,
                                 axis.unit.ratio = NULL,
                                 expand.x.axis = c(0, 0),
                                 expand.y.axis = c(0, 0)) {



  



  plot_limits0 <- .get_plot_limits(p)
  plot_limits <- plot_limits0



  # Given plot center
  if (!is.null(plot.center)) {

    x <- max(abs(unlist(plot_limits0[1:2]) - plot.center[1]))

    plot_limits[["xmin"]] <- - x + plot.center[1]
    plot_limits[["xmax"]] <- + x + plot.center[1]

    y <- max(abs(unlist(plot_limits0[3:4]) - plot.center[2]))

    plot_limits[["ymin"]] <- - y + plot.center[2]
    plot_limits[["ymax"]] <- + y + plot.center[2]


  } else {

    plot.center <- c(mean(unlist(plot_limits[1:2])),
                     mean(unlist(plot_limits[3:4])))

  }




  # Given limits
  if (!is.null(x.axis.limits) & !is.null(y.axis.limits)) {


    if (length(x.axis.limits) != 2 | length(y.axis.limits) != 2)
      stop("x.axis.limits and y.axis.limits must be a numeric vector of length 2 like c(-1, 1).")


    plot_limits[["xmin"]] <- x.axis.limits[1]
    plot_limits[["xmax"]] <- x.axis.limits[2]
    plot_limits[["ymin"]] <- y.axis.limits[1]
    plot_limits[["ymax"]] <- y.axis.limits[2]


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[1:2]),
                                                     break.size = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[3:4]),
                                                     break.size = y.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      #
      coord_fixed(ratio = axis.unit.ratio)

    # Given axis.unit.ratio
  } else if (!is.null(axis.unit.ratio)) {

    x <- plot_limits[["xmax"]] - plot.center[1]
    y <- (plot_limits[["ymax"]] - plot.center[2]) * axis.unit.ratio

    if (x > y) {

      plot_limits[["ymin"]] <- plot_limits[["ymin"]] - (x - y) / axis.unit.ratio
      plot_limits[["ymax"]] <- plot_limits[["ymax"]] + (x - y) / axis.unit.ratio

    } else if (x < y) {

      plot_limits[["xmin"]] <- plot_limits[["xmin"]] - (y - x)
      plot_limits[["xmax"]] <- plot_limits[["xmax"]] + (y - x)

    }


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[1:2]),
                                                     break.size = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[3:4]),
                                                     break.size = y.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      #
      coord_fixed(ratio = axis.unit.ratio)


  } else {


    #
    p <- p +
      # Aspect ratio of panel (does not care about axis units)
      theme(aspect.ratio = aspect.ratio) +
      #
      scale_x_continuous(limits = unlist(plot_limits[1:2]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[1:2]),
                                                     break.size = x.axis.breaks)$breaks,
                         expand = expand.y.axis) +
      scale_y_continuous(limits = unlist(plot_limits[3:4]),
                         breaks = .axis_limit_breaks(plot.limits = unlist(plot_limits[3:4]),
                                                     break.size = y.axis.breaks)$breaks,
                         expand = expand.y.axis)
  }

  # Return
  return(p)

}

