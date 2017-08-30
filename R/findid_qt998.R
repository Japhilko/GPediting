#' Find commentaries in document
#' @param htmldat a html document.
# htmldat <- dat_i

findid_qt998 <- function(htmldat){
  info <- agrep("qt998",htmldat)
  textlist <- htmldat[info]
  idtype <- unlist(lapply(textlist,function(x)sub(".*div id=\\\" *(.*?) *\\\".*", "\\1", x)))
  divclass <- unlist(lapply(textlist,function(x)sub(".*div class=\\\" *(.*?) *\\\".*", "\\1",x)))
  qnameq <- unlist(lapply(textlist,function(x)sub(".*qnameq *(.*?) *\\\".*", "\\1", x)))
  qnametxt <- unlist(lapply(strsplit(textlist,"qnameq"),function(x)x[2]))
  qnametxt <- unlist(lapply(qnametxt,function(x)sub(".*\\\"> *(.*?)", "\\1", x)))
  return(data.frame(idtype,divclass,qnameq,qnametxt))
}


