#' Remove unneeded lines of text at top & bottom of each article
#'
#' @param tx a character vector, resulting from reading, one entry per line, from a text file
#' @param headskip number of lines at top of each article to remove
#' @param tailskip number of lines at bottom of each article to remove
#' @export
trim_tx <- function(tx, headskip = 0, tailskip = 0){
  out <- tx[-c(0:headskip, (length(tx) - tailskip + 1):length(tx))] # I changed 1:headskip to 0:headskip
  return(out)
}
