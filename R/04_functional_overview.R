#' Calculates composition of given annotations
#'
#' @param data data frame with variables as rows and logical values in each
#' column per observation
#' @param TERM2GENE TERM2GENE data frame
#' @param correct.fractions.by devide fractions by total annotated proteins
#'
#' @return
#' @export
#'
#'
.do_annotation_composition <- function(data,
                                       TERM2GENE,
                                       correct.fractions.by = NULL) {

  # Test TERM2GENE
  if (!is.data.frame(TERM2GENE) || !all(colnames(TERM2GENE) == c("TERM", "GENE")))
    stop("<TERM2GENE> must contain two columns named TERM and GENE.", 
         call. = FALSE)

  # Check data input
  if (!all(unlist(lapply(data[-1], typeof)) == "logical")) {
    stop("<data> must contain logical columns indicating in which samples (columns) each variables is identified.", 
         call. = FALSE)
  }
  
  # Data frame to store results
  data_count <- dplyr::tibble(TERM = unique(TERM2GENE$TERM))

  
  # Add sample data
  for (i in seq_along(data)[-1]) {

    TERMS <- TERM2GENE %>%
      dplyr::filter(GENE %in% 
                      names(which(dplyr::pull(data, i, 1)))) %>%
      dplyr::pull(TERM)


    count <- paste0("count.", names(data)[i])
    percent <- paste0("percent.", names(data)[i])

    data_count <- data_count %>%
      dplyr::rowwise() %>%
      dplyr::mutate(!!count := sum(TERM == TERMS)) %>%
      dplyr::mutate(!!percent := .data[[count]] / length(data[[i]])) %>% 
      dplyr::ungroup()
  }


  # Correct fractions
  if (!is.null(correct.fractions.by)) {
    for (i in which(grepl("percent", colnames(data_count)))) {
      data_count[[i]] <- data_count[[i]] / data_count[[i]][data_count$TERM == correct.fractions.by]
    }
  }

  # Order results
  data_count <- data_count %>%
    dplyr::arrange(-!!dplyr::sym(colnames(data_count)[2]))

  # Return
  return(data_count)

}


#' dplyr join functions for more than two objects in a list_ objects
#'
#' @param data_ data list
#' @param correct.fractions.by 
#' @param add.protein 
#' @param input.names 
#' @param output.name if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_annotation_composition <-function(data_, 
                                     correct.fractions.by = NULL, 
                                     add.protein = FALSE, 
                                     input.names, 
                                     output.name) {
  
  # Check input
  data_all <- .unpack_data_m(data_, input.names)
  
  # Save attributes
  data_attributes_list <- lapply(data_all, attributes)
  
  
  ####
  
  data <- .do_annotation_composition(data = data_all[[1]], 
                                     TERM2GENE = data_all[[2]], 
                                     correct.fractions.by = correct.fractions.by)
  
  
  # Output name
  if (!hasArg(output.name)) 
    output.name <- paste0(input.names[1], "_annotation_composition")
  
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


#' Plots data as bar plot with ggplot2
#'
#' @param data_ data_
#' @param TERMS TERMS to plot
#' @param type type of data to represent ("Fraction" or "Count")
#' @param color names vector of colors
#' @param xlab labels of x-axis
#' @param ylab label of y-axis
#' @param legend.name name of legend
#' @param legend.position position of legend ("none", "right", "bottom",
#' "top", "left")
#' @param aspect.ratio aspect ratio of x- and y-axis
#' @param custom.theme predefined theme for plot
#' @param ... arguments for custom.theme
#' @param view view plot after generation
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
.plot_annotation_composition <- function(data,
                                         TERMS = "mitochondrion",
                                         type = "Fraction",
                                         color = "Set1",
                                         xlab = "Number/Fraction of Proteins",
                                         ylab = "",
                                         legend.name = "",
                                         legend.position = "right",
                                         aspect.ratio = 1,
                                         custom.theme = ggplot2::theme_classic,
                                         ...) {
  

  data_melt_1 <- data %>%
    dplyr::filter(TERM %in% TERMS) %>%
    dplyr::select(c(TERM, starts_with("count"))) %>% 
    tidyr::pivot_longer(cols = !TERM) %>%
    dplyr::mutate(name = gsub("count.", "", name)) %>% 
    dplyr::rename(Count = value, 
                  observation = name)

  data_melt_2 <- data %>%
    dplyr::filter(TERM %in% TERMS) %>%
    dplyr::select(c(TERM, starts_with("percent"))) %>%
    tidyr::pivot_longer(cols = !TERM) %>%
    dplyr::mutate(name = gsub("percent.", "", name)) %>% 
    dplyr::rename(Fraction = value, 
                  observation = name)

  data_melt <- dplyr::full_join(data_melt_1, 
                                y = data_melt_2, 
                                by = c("TERM", "observation"))
    
  # Change names of annotations
  if (length(names(TERMS)) > 0) {

    names(TERMS)[names(TERMS) == ""] <- TERMS[names(TERMS) == ""]

    data_melt$TERM <- names(TERMS)[match(data_melt$TERM, TERMS)]

  }


  data_melt$observation <- factor(data_melt$observation,
                               levels = rev(unique(data_melt$observation)))
  
  data_melt$TERM <- factor(data_melt$TERM,
                           levels = rev(unique(data_melt$TERM)))


  # Define colors automatically
  if (length(color) == 1) {
    color <- RColorBrewer::brewer.pal(length(unique(data_melt[["observation"]])), 
                                      name = color)
  }
  
  
  
  # Plot
  p <- ggplot(data = data_melt, aes(x = TERM,
                                    y = .data[[type]],
                                    fill = observation)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(name = legend.name,
                      values = color,
                      guide = guide_legend(reverse=TRUE)) +
    custom.theme(...) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(vjust = 0.5),
          aspect.ratio = aspect.ratio,
          legend.position = legend.position) +
    coord_flip() +
    xlab(ylab) +
    ylab(xlab)

  if (type == "Fraction") {
    p <- p +
      scale_y_continuous(labels = scales::percent)
  }


  # Return
  return(p)
}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param TERMS 
#' @param type 
#' @param color 
#' @param xlab 
#' @param ylab 
#' @param legend.name 
#' @param legend.position 
#' @param aspect.ratio 
#' @param custom.theme 
#' @param ... 
#' @param input.name if data_ is list: name of data to use
#' @param output.name if data_ is list: name of output data to save in list 
#' under#'
#' 
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_annotation_composition <- function(data_, 
                                        TERMS = "mitochondrion",
                                        type = "Fraction",
                                        color = "Set1",
                                        xlab = "Number/Fraction of Proteins",
                                        ylab = "",
                                        legend.name = "",
                                        legend.position = "right",
                                        aspect.ratio = 1,
                                        custom.theme = ggplot2::theme_classic,
                                        ..., 
                                        input.name, 
                                        output.name) {
  
  # Check input
  data <- .unpack_data(data_, input.name)
  
  # Save attributes
  data_attributes <- attributes(data)
  
  ####
  
  
  # Make plot of nothing
  p <- .plot_annotation_composition(data = data,
                                    TERMS = TERMS,
                                    type = type,
                                    color = color,
                                    xlab = xlab,
                                    ylab =ylab,
                                    legend.name = legend.name,
                                    legend.position = legend.position,
                                    aspect.ratio = aspect.ratio,
                                    custom.theme = custom.theme,
                                    ...)
  
  # Literally plot nothing
  plot(p)
  
  # Output name
  if (!hasArg(output.name)) 
    output.name <- paste0(data_attributes[["input.name"]], 
                          "_plot-annotation-composition")
  
  ####
  
  # Prepare return
  data_ <- .pack_data(p, data_, data_attributes, output.name, overwrite = T)
  
  # Return
  return(invisible(data_))
  
}
