find_qt311 <- function(htmldat){
  info <- grep("qt311",htmldat)
  textlist <- htmldat[info]
  idtype <- unlist(lapply(textlist,function(x)sub(".*div id=\\\" *(.*?) *\\\".*", "\\1", x)))
  divclass <- unlist(lapply(textlist,function(x)sub(".*div class=\\\" *(.*?) *\\\".*", "\\1",x)))
  qnameq <- unlist(lapply(textlist,function(x)sub(".*qnameq *(.*?) *\\\".*", "\\1", x)))
  info3 <- info+3
  textlist3 <- htmldat[info3]
  tdclass <- unlist(lapply(textlist3,function(x)sub(".*td class=\\\" *(.*?) *\\\".*", "\\1", x)))
  aname <- unlist(lapply(textlist3,function(x)sub(".*a name=\\\" *(.*?) *\\\".*", "\\1", x)))
  qnametxt <- unlist(lapply(textlist3,function(x)sub(".*</a> *(.*?)", "\\1", x)))

  return(data.frame(txtline=info,idtype,divclass,qnameq,tdclass,aname,qnametxt))
}


