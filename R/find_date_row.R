#' Find the row, for one article, in which the date is listed
#'
#' @param x a vector
#' @param format a character vector of length one specifying the date format
#' @return a number, the index of the first entry of x that is a date
#' @export

find_date_row <- function(x, format = "%B %d, %Y"){
  indic <- FALSE
  i <- 0
  while(!indic){
    i <- i + 1
    indic <- is_date(trim(x[i]), format = format)
  }
  return(i)
}


#' Returns a logical to indicate whether input is a date, per a specific format
#'
#' @param x an input vector
#' @param format a character vector of length one specifying the date format
#' @return a logical vector
#' @export

is_date <- function(x, format = "%B %d, %Y"){
  !is.na(lubridate::as_date(x, format = format))
}

#' Tabulate a vector of dates
#'
#' @param directory a character vector specifying path to a directory
#' @param end_of_article a character vector specifying end of story
#' @export

tabulate_dates <- function(directory,
                           end_of_article = "All Rights Reserved"){
  out <- list()
  i <- 0
  for (fn in dir(directory)){
    i <- i + 1
    ff <- file.path(directory, fn)
    out[[i]]<- extract_dates(ff, end_of_article = end_of_article)
  }
  foo <- sapply(FUN = as.character, X = out)
  foo2 <- unlist(foo)
  dates <- lubridate::as_date(foo2)
  return(table_dates(dates))
}

#' Table dates
#'
#' @param dates a dates vector
#' @export

table_dates <- function(dates){
  tab <- table(dates)
  return(data.frame(tab))
}
