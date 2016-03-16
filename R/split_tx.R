#' Partition a text file, containing multiple articles or transcripts, into distinct articles
#'
#' @param tx a character vector resulting from scan() of a text file containing multiple articles
#' @param patt a character vector that is the pattern denoting the start of a new article
#' @return a list with length equal to the number of distinct articles
#' @export
split_tx <- function(tx, patt = "285 DOCUMENTS"){
  counter <- 0
  outer <- list()
  inner <- vector(mode = "character")
  for (i in 1:length(tx)){
    inner <- append(inner, tx[i])
    if(stringr::str_detect(tx[i], patt)){
      counter <- counter + 1
      outer[[counter]] <- inner
      inner <- vector(mode = "character")
    }
  }
  return(outer)
}

#' Wrapper to partition text file into multiple articles AND to remove unneeded text at top & bottom of each article
#'
#' @param tx a character vector, resulting from reading, one entry per line, from a text file
#' @param patt pattern denoting start of a new article
#' @param headskip number of lines at top of each article to remove
#' @param tailskip number of lines at bottom of each article to remove
#' @export
split_tx_trim <- function(tx, patt, headskip = 0, tailskip = 0){
  splitted <- split_tx(tx, patt)
  out <- lapply(X = splitted, FUN = function(x)x[-c(1:headskip, (length(x) - tailskip + 1):length(x))])
  return(out)
}
