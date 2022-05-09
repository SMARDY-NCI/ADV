#' Perform PCR, extract scores matrix and sort
#'
#' @param dat Dataset
#' @param maxA Maximum number of neurons in latent layer
#' @param kcv Number of folds used for cross-validation
#'
#' @return loss_results optimal autoencoder configuration
#' @export
pca_opt <- function(dat, A.values = c(1:10), tr = NULL, kcv = 10) {
  X <- as.matrix(dat)
  # If training and testing partitions of the cross validation are not supplied 
  # in tr, then create them:
  if (is.null(tr)){
    tr <- createFolds(c(1:nrow(X)), k = kcv, list = TRUE, returnTrain = TRUE)
    if(length(tr$Fold01)<10){
      tr <- createFolds(c(1:nrow(X)), k = nrow(X), list = TRUE, returnTrain = TRUE)
      kcv <- nrow(X)
    }
  } else {
    kcv <- length(tr)
  }
  maxA <- max(A.values)
  # Initialise the matrix storing the results of the loss function to be optimised
  loss_opt_tr <- matrix(NA, maxA, kcv)
  loss_opt_ts <- matrix(NA, maxA, kcv)
  
  for (k.A in A.values){
    for (k.cv in c(1:kcv)){
      X.tr <- X[tr[[k.cv]],,drop=F]
      X.ts <- X[(c(1:nrow(X) %in% tr[[k.cv]])),,drop=F]
      pca <-  X.tr %>%
        pcals(A = k.A)
      
      # MSE for training set
      Xrec <- sweep(sweep(pca$x%*%t(pca$rotation),2,pca$scale,"*"),2,pca$center,"+")
      E.tr <- X.tr - Xrec
      
      # MSE for test set
      X.prepo <- sweep(sweep(X.ts,2,pca$center,"-"),2,pca$scale,"/")
      Xrec.ts <- sweep(sweep(X.prepo%*%pca$rotation%*%t(pca$rotation),2,pca$scale,"*"),2,pca$center,"+")
      E.ts <- X.ts - Xrec
      
      # Evaluate the model
      loss_opt_tr[k.A, k.cv] <- mean(E.tr^2)
      loss_opt_ts[k.A, k.cv] <- mean(E.ts^2)
    }
  }
  return(loss_results = list(losstr = loss_opt_tr, 
                             lossts = loss_opt_ts,
                             A = A.values, 
                             tr = tr))
  
}

#' Fit PCA
#'
#' @param X A dataframe or matrix with the data
#' @param k.A The dimensionality of the latent space
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
fit_pca <- function(X, k.A){
  X <- as.matrix(X)
  model.A <- pcals(X, A = k.A) 
  # MSE
  X.prepo <- sweep(sweep(X,2,model.A$center,"-"),2,model.A$scale,"/")
  Xrec <- sweep(sweep(X.prepo%*%model.A$rotation%*%t(model.A$rotation),
                      2,model.A$scale,"*"),2,model.A$center,"+")
  E <- X - Xrec
  
  # Evaluate the model
  loss <- mean(E^2)
  return(list(model = model.A, loss = loss))
}

#' Fit PCA with different levels of row removal percentages
#'
#' @param data A dataframe or matrix with the data
#' @param A The dimensionality of the latent space
#' @param k_ho The number of holdout repetitions at each percentage
#' @param rowrm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vpca_removerows <- function (data, A, ref.P, k_ho=1000,
                             rowrm_pctges=c(1,5,10,seq(20,80,by=20)),
                             ho.part = NULL){
  X <- dat
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
  ho.list <- vector(mode = "list", length = nlevels)
  for (jmd in 1:nlevels){
    print(paste0("Rows removal at ", rowrm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    N.lim <- floor(nrow(X) - rowrm_pctges[jmd]/100*nrow(X))
    if(is.null(ho.part)){
      ho  <- createTimeSlices(c(1:nrow(X)), N.lim, horizon = 1, 
                              fixedWindow = TRUE, skip = 0)
      ho.list[[jmd]] <- ho$train
    } else {
      ho.list[[jmd]] <- ho.part[[jmd]]
    }
    for (jrep in c(1:length(ho.list[[jmd]]))){
      ho <- ho.list[[jmd]][[jrep]]
      Xtr <- X[ho,,drop=F]
      try({
        pcamodel_test <- fit_pca(Xtr, A)
        Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                               pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
        for(a in c(1:A)){
          para_test[[paste0("t",a,"radius")]][jrep] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
          para_test[[paste0("p",a,"corr")]][jrep] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                             ref.P[,a]))
        }
        id.loc <- and((para_test$Repetition==jrep),(para_test$RWoutpctge==rowrm_pctges[jmd]))
        para_test$limspe[id.loc] <- pcamodel_test$model$limspe
        para_test$limt2[id.loc] <- pcamodel_test$model$limt2
        para_test$log10mspe[id.loc] <- log10(model$loss)
      })
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum], 
                                         na.rm = TRUE), 4), 
                          " (", round(apply(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum], 
                                            2, sd, na.rm = TRUE), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho))
}