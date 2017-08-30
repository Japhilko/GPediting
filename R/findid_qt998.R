#' Find commentaries in document
#' @param text a html document.
# text <- dat_i

findid_qt998 <- function(text){
  indid <- gregexpr(pattern ="id=",text)
  indidf <- gregexpr(pattern ="><div class",text)
  indqnameq <- strsplit(pattern ="qnameq",text)

  id <- substr(text,indid[[1]][1]+4,indidf[[1]][1]-2)
  divid <- substr(text,inddivid[[1]][2],inddivid[[1]][1])
}
