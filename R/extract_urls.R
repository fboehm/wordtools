#' Extract URLs from a text file that contains database query results (such as Proquest)
#'
#' @param file a character vector that specifies the filename
#' @export

extract_urls <- function(file){
  search_results <- readLines(file)
  url_lines <- search_results[str_detect(string = search_results, pattern = "URL:")]
  list_urls<- str_split(string = url_lines, pattern = "URL: ")
  urls <- sapply(FUN = function(x)x[2], X = list_urls)
  return(urls)
}
