#' Extract hashtags from a vector of tweets
#'
#' @param text a character vector of tweet texts
#' @export
extract_hashtags <- function(text){
  hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",text))
  hashtags <- unlist(hashtags)
  hashtags <- table(hashtags)
  sorted <- order(hashtags, decreasing=TRUE)
  hashtags <- hashtags[sorted]
  class(hashtags) <- "hashtags"
  return(hashtags)
}
