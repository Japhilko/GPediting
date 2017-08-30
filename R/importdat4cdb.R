importdat4cdb <- function(cdbdat){
  ## Delete the empty rows
  ind_emptrow <- apply(cdbdat, 1, function(x) all(is.na(x)))
  cdbdat <- cdbdat[-ind_emptrow,1:4]


  for (i in 1:ncol(cdbdat)){
    cdbdat[,i] <- as.character(cdbdat[,i])
  }

  ind <- grepl("^[[:digit:]]",cdbdat[,1])

  ind_num <- which(ind)

  cdb_list <- list()


  for (i in 1:(length(ind_num)-1)){
    cdb_i <- data.frame(cdbdat[(ind_num[i]+1):(ind_num[i+1]-1),])
    ftch <- substr(cdb_i[,1],1,2)
    qnum_ind <- which(ftch=="v_")
    if(length(qnum_ind)<=1){
      nam <- cdb_i[1,1]
      indNA <- which(apply(cdb_i[,2:4], 1, function(x) all(is.na(x))))
      cdb_i <- cdb_i[-c(1,indNA),]
      cdb_i$nam <- nam
    }else{
      cdb_j <- list()
      qnum_ind <- c(qnum_ind,nrow(cdb_i))
      for(j in 1:(length(qnum_ind)-1)){
        nam <- cdb_i[qnum_ind[j]-1,1]
        eb_Feld<- cdb_i[qnum_ind[j],1]
        varnam<- cdb_i[qnum_ind[j],2]
        cdb_j[[j]] <- data.frame(cdb_i[(qnum_ind[j]):(ind_num[j+1]-1),])
        cdb_j[[j]]$nam <- nam
        cdb_j[[j]]$eb_Feld <- eb_Feld
        cdb_j[[j]][,2] <- varnam
      }
      cdb_i <- do.call(rbind,cdb_j)
    }

    cdb_i$nr <- cdbdat[(ind_num[i]),1]
    cdb_i$study <- cdbdat[(ind_num[i]),2]
    cdb_list[[i]] <- cdb_i
  }

  cdb_info <- do.call(rbind,cdb_list)
}
