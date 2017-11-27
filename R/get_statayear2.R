#' Correct a year variable
#' @param year a variable with year information.

get_statayear2 <- function(year){
  year1 <- substr(year,1,1)
  yearb <- year

  ind1 <- which(year1 %in% c(0,3:9) & nchar(year)==4)
  yearc1 <- substr(year[ind1],3,4)
  yearc2 <- ifelse(as.numeric(yearc1)>17,paste0(19,yearc1),paste0(20,yearc1))

  ind2 <- which(nchar(year)==2)
  yearc3 <- ifelse(as.numeric(year[ind2])>17,paste0(19,year[ind2]),paste0(20,year[ind2]))

  ind3 <- which(year1 %in% (0:9) & nchar(year)==3)
  yeard4 <- substr(year[ind3],2,3)
  yearc4 <- ifelse(as.numeric(yeard4)>17,paste0(19,yeard4),paste0(20,yeard4))

  ind4 <- which(nchar(year)>4)
  yeard5 <- substr(year[ind4],nchar(year[ind4])-1,nchar(year[ind4]))
  yearc5 <- ifelse(as.numeric(yeard5)>17,paste0(19,yeard5),paste0(20,yeard5))

  ind5 <- which(nchar(year)==1)
  yearc6 <- paste0(200,year[ind5])

  year12 <- substr(year,1,2)
  ind6 <- which(nchar(year)==4 & !(year12 %in% c(19,20)))
  yeard7 <- substr(year[ind6],nchar(year[ind6])-1,nchar(year[ind6]))
  yearc7 <- ifelse(as.numeric(yeard7)>17,paste0(19,yeard7),paste0(20,yeard7))

  year_ls <- substr(year,nchar(year)-1,nchar(year))
  ind7 <- which(grepl("[[:alpha:]]", year_ls))
  yearc8 <- rep("-111",length(year[ind7]))

  dat_sr <- data.frame(stri=year[ind1],repl=yearc2)
  for (i in 2:7){
    ind <- eval(parse(text=paste0("ind",i)))
    if (length(ind)>0){
      yearc <- eval(parse(text=paste0("yearc",i+1)))
      dat_sr <- rbind(dat_sr,data.frame(stri=year[ind],repl=yearc))
    }
  }

  return(dat_sr)
}
