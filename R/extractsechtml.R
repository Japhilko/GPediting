#' Extract Sections from html
#'
#' @param htmldat An html document.
#' @return The sections of \code{htmldat}
#' @examples
#'

extractsechtml <- function(htmldat){
  sec_info <- agrep("class='sectionborder'",htmldat)
  sections <- htmldat[sec_info]
  secout <- agrep("sectionborder_outer",sections)
  
  sec_info <- sec_info[-secout]
  sections <- htmldat[sec_info]
  
  sechead <- gsub("<tbody><tr><td class=\"sectionborder\"><b>","",sections)
  sechead <- gsub("</td>","",sechead)
  sechead2 <- strsplit(split = "</b> &nbsp;&nbsp;",x=sechead)
  sechead3 <- data.frame(do.call(rbind,sechead2))
  sechead3$sec_info <- sec_info
  colnames(sechead3) <- c("nr","title","line")
  sechead3[] <- lapply(sechead3, as.character)
  sechead3 <- sechead3[-which(sechead3$nr==sechead3$title),]
  return(sechead3)
}
