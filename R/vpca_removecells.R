#' Fit PCA with different levels of row removal percentages
#'
#' @param data A dataframe or matrix with the data
#' @param A The dimensionality of the latent space
#' @param k_ho The number of holdout repetitions at each percentage
#' @param rm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vpca_removecells <- function (data, A, ref.P, k_ho=1000,
                              rm_pctges=c(1,5,10,seq(20,80,by=20)),
                              ho.part = NULL){
  X <- as.matrix(data)
  n.elems <- nrow(X)*ncol(X)
  nrep <- k_ho
  nlevels <- length(rm_pctges)
  n.results <- nrep*nlevels
  
  # Build list containing the results
  para_test <- list(limspe = matrix(NA,n.results,1), 
                    limt2 = matrix(NA,n.results,1), 
                    log10mspe = matrix(NA,n.results,1),
                    log10msie = matrix(NA,n.results,1))
  
  for(a in 1:A){
    para_test <- cbind(para_test,as.data.frame(matrix(NA,nrow = n.results,ncol=2)))
    colnames(para_test)[(ncol(para_test)-1):ncol(para_test)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  para_test$Repetition <- rep(c(1:nrep),each = nlevels)
  para_test$MDpctge <- rep(rm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "MD pctge")))
  d.sum <- data.frame(matrix(NA,nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("cell rem ", rm_pctges)
  if(is.null(ho.part)){
    ho.list <- vector(mode = "list", length = nlevels)
  }
  
  for (jmd in c(1:nlevels)){
    print(paste0("Cells removal at ", rm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    if(is.null(ho.part)){
      ho.list[[jmd]] <- vector(mode = "list", length = nrep)
    }
    for (jrep in c(1:nrep)){
      jrep
      if(is.null(ho.part)){
        ho.list[[jmd]][[jrep]] <- sample(c(1:n.elems), round(rm_pctges[jmd]/100*n.elems))
      }
      ho <- ho.list[[jmd]][[jrep]]
      Xtr <- as.matrix(X)
      Xtr[ho] <- NA
      mod.tsr <-pcambtsrR(Xtr,A, maxiter = 200)
      Xtr.imp <- mod.tsr$X
      
      try({
        pcamodel_test <- fit_pca(Xtr.imp, A)
        Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                               pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
        id.loc <- and((para_test$Repetition==jrep),(para_test$MDpctge==rm_pctges[jmd]))
        # print(which(id.loc))
        for(a in c(1:A)){
          para_test[[paste0("t",a,"radius")]][id.loc] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
          para_test[[paste0("p",a,"corr")]][id.loc] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                             ref.P[,a]))
        }
        # print(which(id.loc))
        para_test$limspe[id.loc] <- pcamodel_test$model$limspe
        para_test$limt2[id.loc] <- pcamodel_test$model$limt2
        para_test$log10msie[id.loc] <- log10(mean((X[ho] - Xtr.imp[ho])^2))
        para_test$log10mspe[id.loc] <- log10(pcamodel_test$loss)
      })
      # print(jrep)
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$MDpctge == rm_pctges[jmd],cols.sum], 
                                         na.rm = TRUE), 4), 
                          " (", round(apply(para_test[para_test$MDpctge == rm_pctges[jmd],cols.sum], 
                                            2, sd, na.rm = TRUE), 4), ")")
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho.list))
}
