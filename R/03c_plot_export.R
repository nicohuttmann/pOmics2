#' Exports plot to pdf
#'
#' @param p plot
#' @param file filename
#' @param width width
#' @param height height
#' @param open open pdf after export
#'
#' @return
#' @export
#'
#'
export_pdf <- function(p, file = "plot.pdf", width = 4, height = 4, open = F) {
  
  file <- gsub(pattern = ":", replacement = "", x = file)
  
  if (substr(file, start = 1, stop = 2) == "C\\") {
    
    file <- paste0("C:\\", substring(file, first = 3))
    
  }
  
  #system('taskkill /f /im AcroRd32.exe')
  
  pdf(file = file, width = width, height = height)
  
  print(p)
  
  dev.off()
  
  if (open)
    system(paste0('open "', file, '"'))
  
}


#' Exports plot to tiff
#'
#' @param p plot
#' @param file filename
#' @param width width
#' @param height height
#' @param res resolution
#' @param open open pdf after export
#'
#' @return
#' @export
#'
#'
export_tiff <- function(p, 
                        file = "plot.tiff", 
                        width = 4, 
                        height = 4, 
                        res = 300, 
                        open = F) {
  
  file <- gsub(pattern = ":", replacement = "", x = file)
  
  if (substr(file, start = 1, stop = 2) == "C\\") {
    
    file <- paste0("C:\\", substring(file, first = 3))
    
  }
  
  
  tiff(filename = file, width = width, height = height, units = "in", res = res)
  
  print(p)
  
  dev.off()
  
  if (open)
    system(paste0('open "', file, '"'))
  
}


#' Exports plot to svg
#'
#' @param p plot
#' @param file filename
#' @param width width
#' @param height height
#' @param open open pdf after export
#'
#' @return
#' @export
#'
#'
export_svg <- function(p, file = "plot.svg", width = 4, height = 4, open = F) {
  
  file <- gsub(pattern = ":", replacement = "", x = file)
  
  if (substr(file, start = 1, stop = 2) == "C\\") {
    
    file <- paste0("C:\\", substring(file, first = 3))
    
  }
  
  
  svg(filename = file, width = width, height = height)
  
  print(p)
  
  dev.off()
  
  if (open)
    system(paste0('open "', file, '"'))
  
}
