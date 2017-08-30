#' Transform Open Question about Frequency
#'
#' @param x A frequency.
#' @return The frequency of \code{x}
#' @examples
#' xvec <- c("ca. 2","20","-77","0.5")
#' openquestfreq(xvec)
#'
#' x <- c("ca.4-6","-77","2-3 x","1 mal","2 mal","ein","zwei","Nie","sehr oft / ca. 20 mal")
#' openquestfreq(x)
#'
#' x <- c("4,5","4.5","nein","einmal","ein halbes Jahr","ja","JA","02.3.")
#' openquestfreq(x)
#'
openquestfreq <- function(x){
  x <- tolower(x)
  ind <- grep("-",x)
  x_0 <- x[-ind]
  xex <- stringr::str_extract(x_0, "[[:digit:]]+")
  x[-ind] <- xex
  x[grep("nein",x)] <- 0
  x[agrep("ein",x)] <- 1
  x[grep("ja",x)] <- 1
  x[agrep("zwei",x)] <- 2
  x[agrep("drei",x)] <- 3
  x[agrep("zehn",x)] <- 10
  x <- stringr::str_replace_all(x, "[[a-z]]", "")
  x <- stringr::str_replace_all(x, "[[A-Z]]", "")
  x <- gsub("[(]","",x)
  x <- gsub("[)]","",x)
  x <- gsub("[*]","",x)
  x <- gsub("[%]","",x)
  x <- gsub("[/]","",x)
  x <- gsub(",",".",x)

  x_1 <-  x[!(x%in%c(-77,-99,-88,-111))]
  x1 <- strsplit(x_1,split = "-")
  x2 <- unlist(lapply(x1,function(x_)mean(as.numeric(x_),na.rm=T)))
  xfin <- as.numeric(x)

  xfin[!(x%in%c(-77,-99,-88,-111))] <- x2
  xfin[is.na(xfin)] <- 0
  return(xfin)
}
