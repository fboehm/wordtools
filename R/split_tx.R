#' Partition a text file, containing multiple articles or transcripts, into distinct articles
#'
#' @param tx a character vector resulting from scan() of a text file containing multiple articles
#' @param patt a character vector that is the pattern denoting the end of each article/story
#' @return a list with length equal to the number of distinct articles
#' @export
#'
split_tx <- function(tx, pattern){
  counter <- 0
  outer <- list()
  inner <- vector(mode = "character")
  for (i in 1:length(tx)){
    inner <- append(inner, tx[i])
    if(stringr::str_detect(tx[i], pattern)){
      counter <- counter + 1
      outer[[counter]] <- inner
      inner <- vector(mode = "character")
    }
  }
  return(outer)
}

#' Remove unneeded lines of text at top & bottom of each article
#'
#' @param tx a character vector, resulting from reading, one entry per line, from a text file
#' @param headskip number of lines at top of each article to remove
#' @param tailskip number of lines at bottom of each article to remove
#' @export
trim_tx <- function(tx, headskip = 0, tailskip = 0){
  out <- tx[-c(1:headskip, (length(tx) - tailskip + 1):length(tx))]
  return(out)
}

#' Find a pattern in an element of a character vector to identify where to start trimming & trim
#'
#' @param string the character vector in which to look for pattern
#' @param pattern the pattern to find in the strings
#' @param end latter index for subset to be removed
#' @export
trim_pattern <- function(string, pattern, end = length(string)){
  start <- which(stringr::str_detect(string = string, pattern = pattern))
  return(string[-c(start:end)])
}


#' Flag transcript (a character vector) for whether it contains a key phrase
#'
#' @param tx a transcript or article that is a character vector
#' @param filter_vector a character vector containing key phrase(s)
#' @export
flag_tx <- function(tx, filter_vector = "All Rights Reserved"){
  tx_collapse <- paste(tx, collapse = " ")
  out <- stringr::str_detect(string = tx_collapse, pattern = filter_vector)
  return(sum(out) > 0) # logical to indicate whether one or more of the key phrases is in the transcript
}

