#' Correct height
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

correctheight <- function(x){
  fch <- as.numeric(substring(x, 1, 1))
  x[nchar(x)==1] <- "-111"
  x[nchar(x)<3 & fch>1] <- "-111"
  x[nchar(x)==2 & fch==1] <- paste0(x[nchar(x)==2 & fch==1],"0")
  x <- as.numeric(x)
  x[x<=151 & x>0] <- 151
  x[x>=198] <- 198
  return(x)
}
