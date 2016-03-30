#' Remove strings that contain ://
#'
#' @param string a character string
#' @export
remove_urls <- function(string){
  indic<- stringr::str_detect(string, pattern = "://")
  return(string[!indic])
}
