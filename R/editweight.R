#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)


# x <- c("65-78","65-66","67")

editweight <- function(x){
  ind <- grep("-",x)
  x1 <- x[ind]
  x2 <- strsplit(x1,"-")
  x3 <- lapply(x2,as.numeric)
  x4 <- lapply(x3,mean)
}
