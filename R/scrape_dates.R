#' Scrape webpage & extract dates for online media
#'
#' @param urls a character vector of URLs
#' @param html_nodes_argument a character vector denoting the argument to html_nodes() function
#' @export

scrape_dates <- function(urls, html_nodes_argument = ".timestamp__date--published"){
  date <- rep(NA, length = length(urls))
  for (i in 1:length(urls)){
    try(rvest::read_html(urls[i]), silent = TRUE) -> foo
    if (class(foo) == "try-error"){
      date[i] <- NA
    } else
    {
      date[i] <- foo %>%
        rvest::html_nodes(html_nodes_argument) %>%
        rvest::html_text()
    }
  }
  return(date)
}
