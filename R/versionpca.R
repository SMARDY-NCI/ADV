versionpca_remrows <- function (data, A, k_ho=1000, rowrm_pctges=c(1,5,10,seq(20,80,by=20))){
  X <- data
  pca <- pcals(X, A)
  # This function will return a matrix with of n_rows (md levels) and 
  # n_columns (# repetitions), with one logical indicator matrix in each cell, 
  # indicating the missing entries for each experimental setup.
  k_ho_base<-k_ho
  nrep <- k_ho
  nlevels <- length(rowrm_pctges)
  
  para_test <- list(limspe = matrix(NA,nrep*nlevels,1), limt2 = matrix(NA,nrep*nlevels,1), 
                    log10mspe = matrix(NA,nrep*nlevels,1))
  for(a in 1:A){
    para_test <- cbind(para_test,as.data.frame(matrix(NA,nrow=nrep*nlevels,ncol=2)))
    colnames(para_test)[(ncol(para_test)-1):ncol(para_test)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  para_test$Repetition <- rep(c(1:nrep),each=nlevels)
  para_test$RWoutpctge <- rep(rowrm_pctges,times = nrep)
  # HO Complete case just to measure uncertainty
  para_base<- data.frame(limspe = matrix(NA,k_ho_base,1), limt2 = matrix(NA,k_ho_base,1), 
                         log10mspe = matrix(NA,k_ho_base,1)) 
  for(a in 1:A){
    para_base <- cbind(para_base,as.data.frame(matrix(NA,nrow=k_ho_base,ncol=2)))
    colnames(para_base)[(ncol(para_base)-1):ncol(para_base)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  print("Parameter avge (sd)")
  d.sum <- data.frame(matrix(NA,nlevels,sum(!(c("Repetition", "RWoutpctge")%in%colnames(para_base)))))
  d.sum$exp_level <- rowrm_pctges
  # LOOP Base setup with 1% of rows deleted
  for (jrep in 1:k_ho_base){
    # set.seed(123) 
    ho  <- holdout(c(1:nrow(X)), ratio = 1-rowrm_pctges[1]/100)
    Xtr <- X[ho$tr,,drop=F]
    pcamodel_base <- pcals(Xtr, A)
    Xrec_cv <- sweep(sweep(pcamodel_base$x%*%t(pcamodel_base$rotation),2,
                           pcamodel_base$scale,"*"),2,pcamodel_base$center,"+")
    for(a in c(1:A)){
      para_base[[paste0("t",a,"radius")]][jrep] <- pcamodel_base$limits_t[[paste0("pc",a)]][2]
      para_base[[paste0("p",a,"corr")]][jrep] <- abs(cor(pcamodel_base$rotation[,a],
                                                         pca$rotation[,a]))
    }
    para_base$limspe[jrep] <- pcamodel_base$limspe
    para_base$limt2[jrep] <- pcamodel_base$limt2
    para_base$log10mspe[jrep] <- log10(mean((as.matrix(Xtr)-Xrec_cv)^2))
  }
  d.sum[1,] <- paste0(round(colMeans(para_base[, !(c("Repetition", "RWoutpctge")%in%colnames(para_base)),drop=F]), 4),
                    " (",round(apply(para_base[,!(c("Repetition", "RWoutpctge")%in%colnames(para_base)),drop=F], 2, sd), 4), ")")
  print(d.sum[1,])
  # LOOP with rest of row percentage removal
  for (jmd in 2:nlevels){
    print(paste0("Rows removal at ", rowrm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    for (jrep in 1:k_ho){
      ho  <- holdout(c(1:nrow(X)), ratio = 1 - rowrm_pctges[jmd]/100)
      Xtr <- X[ho$tr,,drop=F]
      pcamodel_test <- pcals(Xtr, A)
      Xrec_cv <- sweep(sweep(pcamodel_test$x%*%t(pcamodel_test$rotation),2,
                             pcamodel_test$scale,"*"),2,pcamodel_test$center,"+")
      for(a in c(1:A)){
        para_test[[paste0("t",a,"radius")]][jrep] <- pcamodel_test$limits_t[[paste0("pc",a)]][2]
        para_test[[paste0("p",a,"corr")]][jrep] <- abs(cor(pcamodel_test$rotation[,a],
                                                           pca$rotation[,a]))
      }
      id.loc <- (para_test$Repetition==jrep) && (para_test$RWoutpctge==rowrm_pctges[jmd])
      para_test$limspe[id.loc] <- pcamodel_test$limspe
      para_test$limt2[id.loc] <- pcamodel_test$limt2
      para_test$log10mspe[id.loc] <- log10(mean((as.matrix(Xtr)-Xrec_cv)^2))
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],!(c("Repetition", "RWoutpctge")%in%colnames(para_test))]), 4), 
                                      " (", round(apply(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],!(c("Repetition", "RWoutpctge")%in%colnames(para_test))], 2, sd), 4), ")")
    print(d.sum[jmd,])
  }
  return(list(para_base = para_base, para_test = para_test, d.sum = d.sum))
}

