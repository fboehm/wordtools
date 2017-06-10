#' Find a pattern in an element of a character vector to identify where to start trimming & trim
#'
#' @param string the character vector in which to look for pattern
#' @param headpattern the pattern to find in the strings
#' @param tailpattern the pattern to find in the strings
#' @export
# This function strips unnecessary texts in header and footer
trim_patterns <- function(string, headpattern, tailpattern){
  start <- which(stringr::str_detect(string = string, pattern = headpattern))
  end <- which(stringr::str_detect(string = string, pattern = tailpattern))
  return(string[-c(start:end)])
}