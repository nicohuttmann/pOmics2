#' The EXAMPLE class
#'
#' This class contains an example. This line goes into the description
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{data}:}{List of class \code{"pOdata"}, containing data from slot1}
#'    \item{\code{results}:}{List of class \code{"pOresults"}, containing data from slot1}
#'    \item{\code{plots}:}{List of class \code{"pOplots"}, containing data from slot1}
#'    \item{\code{history}:}{List of class \code{"pOhistory"}, containing data that needs to go in slot2.}
#'  }
#'  
#'
#' 
# pOlist <- R7::new_class(
#   "pOlist",
#   properties = list(
#     data = R7::class_list, 
#     plots = R7::class_list, 
#     history = R7::class_list
#   ),
#   validator = function(self) {
#     if (length(self@data) == 0 && length(self@plots) > 0) {
#       "@plots cannot exist before adding data"
#     } else if (sum(length(self@data), 
#                    length(self@plots)) > length(history)) {
#       "the current step has not been documented in @history yet"
#     }
#   }
# )
