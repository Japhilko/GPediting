#' Transform Months to Days
#'
#'@examples
#'x <- c("9","9 Monate")
transform2day <- function(x){
  ind <- agrep("Monat",x)
  x1 <- x[ind]
  xex <- stringr::str_extract(x1, "[[:digit:]]+")
  x[ind] <- as.numeric(xex)*20

  ind <- agrep("Woche",x)
  x1 <- x[ind]
  xex <- stringr::str_extract(x1, "[[:digit:]]+")
  x[ind] <- as.numeric(xex)*5

  x[agrep("Jahr",x)] <- 365

  return(x)
}
