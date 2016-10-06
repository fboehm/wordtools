#' Scrape webpage & extract a single node for online media
#'
#' @param urls a character vector of URLs
#' @param node a character vector denoting the argument to html_nodes() function
#' @export

scrape_node <- function(urls, node = ".timestamp__date--published"){
  out <- list()
  for (i in 1:length(urls)){
    try(rvest::read_html(urls[i]), silent = TRUE) -> foo
    if (class(foo) == "try-error"){
      out[i] <- NA
    } else
    {
      out[[i]] <- foo %>%
        rvest::html_nodes(node) %>%
        rvest::html_text()
    }
  }
  return(out)
}
