#' Reorganize info from html file
#' @param txt a html text.
#'



gsub_html <- function(txt){
  txt <- gsub(pattern = "Ã¼","ü",txt)
  txt <- gsub(pattern = "</strong>"," ",txt)
  txt <- gsub(pattern = "<br>"," ",txt)
  txt <- gsub(pattern = "<strong>"," ",txt)
  txt <- gsub(pattern = "Ã¶","ö",txt)
  txt <- gsub(pattern = "ÃŸ","ß",txt)
  txt <- gsub(pattern = "Ã¤","ä",txt)
  txt <- gsub(pattern = "<p>"," ",txt)
  txt <- gsub(pattern = "</p>"," ",txt)
  txt <- gsub(pattern = "</div>"," ",txt)
  txt <- gsub(pattern = "&amp","-",txt)
  txt <- gsub(pattern = "â€ž","'",txt)
  txt <- gsub(pattern = "â€œ","'",txt)
  txt <- gsub(pattern = "   "," ",txt)
  return(txt)
}
