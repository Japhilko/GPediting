#' Transform Open Question
#'
#' @param x A number.
#' @return The sum of \code{x}
#' @examples
#' xvec <- c("160.cm","1,54","345 m")
#' openquestnum(xvec)
#'
#'
#'
#'

openquestnum <- function(x){
  x <- gsub(",","",x)
  x <- gsub("[.]","",x)
  x <- gsub(" ","",x)
  x <- gsub("~","",x)
  x <- stringr::str_replace_all(x, "[[a-z]]", "")
  x <- stringr::str_replace_all(x, "[[A-Z]]", "")
  x[!(x%in%c(-77,-99,-88,-111))] <- stringr::str_replace_all(x[!(x%in%c(-77,-99,-88,-111))], "[:punct:]", "")
  return(x)
}

