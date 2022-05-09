versionpca_transcols <- function (data, A, k_ho=1000,
                                 trans_pctges=c(1,5,10,seq(20,100,by=20))){
  X <- data
  pca <- pcals(X, A)
  k_ho_base<- k_ho
  nrep <- k_ho
  nlevels <- length(rowrm_pctges)
  n.results <- nrep*nlevels
  
  # Build list containing the results
  para_test <- list(limspe = matrix(NA,n.results,1), 
                    limt2 = matrix(NA,n.results,1), 
                    log10mspe = matrix(NA,n.results,1))
  
  for(a in 1:A){
    para_test <- cbind(para_test,as.data.frame(matrix(NA,nrow = n.results,ncol=2)))
    colnames(para_test)[(ncol(para_test)-1):ncol(para_test)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  para_test$Repetition <- rep(c(1:nrep),each = nlevels)
  para_test$RWoutpctge <- rep(rowrm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "RWoutpctge")))
  d.sum <- data.frame(matrix(NA,nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("rw rem ", rowrm_pctges)
  
  # LOOP with rest of row percentage removal
  for (jmd in 1:nlevels){
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
      id.loc <- and((para_test$Repetition==jrep),(para_test$RWoutpctge==rowrm_pctges[jmd]))
      para_test$limspe[id.loc] <- pcamodel_test$limspe
      para_test$limt2[id.loc] <- pcamodel_test$limt2
      para_test$log10mspe[id.loc] <- log10(mean((as.matrix(Xtr)-Xrec_cv)^2))
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum]), 4), 
                          " (", round(apply(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum], 2, sd), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum))
}

