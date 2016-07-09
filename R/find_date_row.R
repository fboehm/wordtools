#' Find the row, for one article, in which the date is listed
#'
#' @param x a vector
#' @return a number, the index of the first entry of x that is a date
#' @export

find_date_row <- function(x, format = "%B %d, %Y"){
  indic <- FALSE
  i <- 0
  while(!indic){
    i <- i + 1
    indic <- is_date(x[i], format = format)
  }
  return(i)
}


#' Returns a logical to indicate whether input is a date, per a specific format
#'
#' @param x an input vector
#' @return a logical vector
#' @export

is_date <- function(x, format = "%B %d, %Y"){
  !is.na(lubridate::as_date(x, format = format))
}
