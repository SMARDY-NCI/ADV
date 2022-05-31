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
  Xts <- X[c((nrow(X)-50):nrow(X)),,drop=F]
  Xtr <- X[-c((nrow(X)-50):nrow(X)),,drop=F]
  if (is.null(tr)){
    tr <- createFolds(c(1:nrow(Xtr)), k = kcv, list = TRUE, returnTrain = TRUE)
    kcv <- length(tr)
    if(kcv<10){
      tr <- createFolds(c(1:nrow(Xtr)), k = nrow(X), list = TRUE, returnTrain = TRUE)
      kcv <- nrow(Xtr)
    }
  } else {
    kcv <- length(tr)
  }
  maxA <- max(A.values)
  # Initialise the matrix storing the results of the loss function to be optimised
  loss_opt_tr <- matrix(NA, maxA, kcv)
  loss_opt_ts <- matrix(NA, maxA, kcv)
  loss_opt_ts_cv <- matrix(NA, maxA, kcv)
  for (k.A in A.values){
    for (k.cv in c(1:kcv)){
      Xtr.cv <- Xtr[tr[[k.cv]],,drop=F]
      Xts.cv <- Xtr[-tr[[k.cv]],,drop=F]
      pca <-  Xtr.cv %>%
        pcals(A = k.A)
      
      # MSE for training set
      Xrec <- sweep(sweep(pca$x%*%t(pca$rotation),2,pca$scale,"*"),2,pca$center,"+")
      E.tr <- Xtr.cv - Xrec
      
      # MSE for test set of the cross-validation
      X.prepo <- sweep(sweep(Xts,2,pca$center,"-"),2,pca$scale,"/")
      Xrec.ts <- sweep(sweep(X.prepo%*%pca$rotation%*%t(pca$rotation),2,pca$scale,"*"),2,pca$center,"+")
      E.ts <- Xts - Xrec.ts
      
      # MSE for test set
      X.cv.prepo <- sweep(sweep(Xts.cv,2,pca$center,"-"),2,pca$scale,"/")
      Xrec.ts.cv <- sweep(sweep(X.cv.prepo%*%pca$rotation%*%t(pca$rotation),2,pca$scale,"*"),2,pca$center,"+")
      E.ts.cv <- Xts.cv - Xrec.ts.cv
      
      # Evaluate the model
      loss_opt_tr[k.A, k.cv] <- mean(E.tr^2)
      loss_opt_ts_cv[k.A, k.cv] <- mean(E.ts.cv^2)
      loss_opt_ts[k.A, k.cv] <- mean(E.ts^2)
    }
  }
  return(loss_results = list(losstr = loss_opt_tr, 
                             lossts = loss_opt_ts,
                             losstscv = loss_opt_ts_cv,
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
fit_pca <- function(X, k.A, xscale = FALSE){
  X <- as.matrix(X)
  model.A <- pcals(X, A = k.A, xscale = xscale) 
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
#' @param rm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vpca_removecells <- function (data, A, ref.P, k_ho=1000,
															rm_pctges=c(1,5,10,seq(20,80,by=20)),
															ho.part = NULL, xscale = FALSE){
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
				pcamodel_test <- fit_pca(Xtr.imp, A, xscale = xscale)
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
vpca_removerows <- function (data, A, ref.P, k_ho=1000,
                             rm_pctges=c(1,5,10,seq(20,80,by=20)),
                             ho.part = NULL, xscale = FALSE){
  X <- data
  nrep <- k_ho
  nlevels <- length(rm_pctges)
  n.results <- nrep*nlevels
  n.elems <- nrow(X)
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
  para_test$RWoutpctge <- rep(rm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "RWoutpctge")))
  d.sum <- data.frame(matrix(NA,nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("rw rem ", rm_pctges)

  if(is.null(ho.part)){
    ho.list <- vector(mode = "list", length = nlevels)
  } else {
    ho.list <- ho.part
  }
  for (jmd in 1:nlevels){
    print(paste0("Rows removal at ", rm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    if(is.null(ho.part)){
      ho.list[[jmd]] <- vector(mode = "list", length = nrep)
    }
    for (jrep in c(1:length(ho.list[[jmd]]))){
      if(is.null(ho.part)){
        ho.list[[jmd]][[jrep]] <- sample(c(1:n.elems), rm_pctges[jmd]/100*n.elems)
      }
      ho <- ho.list[[jmd]][[jrep]]
      Xtr <- X[-ho,,drop=F]
      Xts <- X[ho,,drop=F]
      try({
        pcamodel_test <- fit_pca(Xtr, A, xscale = xscale)
        Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                               pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
        id.loc <- ((para_test$Repetition==jrep) & (para_test$RWoutpctge==rm_pctges[jmd]))
        for(a in c(1:A)){
          para_test[[paste0("t",a,"radius")]][id.loc] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
          para_test[[paste0("p",a,"corr")]][id.loc] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                               ref.P[,a]))
        }
        
        para_test$limspe[id.loc] <- pcamodel_test$model$limspe
        para_test$limt2[id.loc] <- pcamodel_test$model$limt2
        para_test$log10mspe[id.loc] <- log10(pcamodel_test$loss)
      })
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rm_pctges[jmd],cols.sum], 
                                         na.rm = TRUE), 4), 
                          " (", round(apply(para_test[para_test$RWoutpctge == rm_pctges[jmd],cols.sum], 
                                            2, sd, na.rm = TRUE), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho.list))
}


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
vpca_removecols <- function (data, A, ref.P, k_ho=1000,
                             rm_pctges=c(1,5,10,seq(20,80,by=20)),
                             ho.part = NULL, xscale = FALSE){
  X <- data
  nrep <- k_ho
  nlevels <- length(rm_pctges)
  n.results <- nrep*nlevels
  
  # Build list containing the results
  para_test <- list(limspe = matrix(NA,n.results,1), 
                    limt2 = matrix(NA,n.results,1), 
                    log10mspe = matrix(NA,n.results,1))
  
  for(a in c(1:A)){
    para_test <- cbind(para_test,as.data.frame(matrix(NA,nrow = n.results,ncol=2)))
    colnames(para_test)[(ncol(para_test)-1):ncol(para_test)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  para_test$Repetition <- rep(c(1:nrep),each = nlevels)
  para_test$RWoutpctge <- rep(rm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "RWoutpctge")))
  d.sum <- data.frame(matrix(NA,nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("rw rem ", rm_pctges)
  ho.list <- vector(mode = "list", length = nlevels)
  for (jmd in c(1:nlevels)){
    print(paste0("Rows removal at ", rm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    N.lim <- floor(nrow(X) - rm_pctges[jmd]/100*nrow(X))
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
        pcamodel_test <- fit_pca(Xtr, A, xscale = xscale)
        Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                               pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
        for(a in c(1:A)){
          para_test[[paste0("t",a,"radius")]][jrep] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
          para_test[[paste0("p",a,"corr")]][jrep] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                             ref.P[,a]))
        }
        id.loc <- and((para_test$Repetition==jrep),(para_test$RWoutpctge==rm_pctges[jmd]))
        para_test$limspe[id.loc] <- pcamodel_test$model$limspe
        para_test$limt2[id.loc] <- pcamodel_test$model$limt2
        para_test$log10mspe[id.loc] <- log10(model$loss)
      })
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rm_pctges[jmd],cols.sum], 
                                         na.rm = TRUE), 4), 
                          " (", round(apply(para_test[para_test$RWoutpctge == rm_pctges[jmd],cols.sum], 
                                            2, sd, na.rm = TRUE), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho))
}

#' Fit PCA with different levels of variable transformations
#'
#' @param data A dataframe or matrix with the data
#' @param A The dimensionality of the latent space
#' @param k_ho The number of holdout repetitions at each percentage
#' @param rm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vpca_transcols <- function (data, A, ref.P, k_ho=1000,
                            rm_pctges=c(1,5,10,seq(20,100,by=20)),
                            ho.part = NULL, xscale = FALSE){
  X <- as.matrix(data)
  nrep <- k_ho
  nlevels <- length(rm_pctges)
  n.results <- nrep*nlevels
  n.elems <- ncol(X)
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
  para_test$Coltrans <- rep(rm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "Coltrans")))
  d.sum <- data.frame(matrix(NA,nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("rw rem ", rm_pctges)
  if(is.null(ho.part)){
    ho.list <- vector(mode = "list", length = nlevels)
  }
  for (jmd in 1:nlevels){
    print(paste0("Columns transformation at ", rm_pctges[jmd], " %"))
    print("Parameter avge (sd)")
    if(is.null(ho.part)){
      ho.list[[jmd]] <- vector(mode = "list", length = nrep)
    }
    for (jrep in c(1:nrep)){
      if(is.null(ho.part)){
        ho.list[[jmd]][[jrep]] <- sample(c(1:n.elems), round(rm_pctges[jmd]/100*n.elems))
      }
      ho <- ho.list[[jmd]][[jrep]]
      Xtr <- X
      Xtr[,ho] <- log(X[,ho,drop=F])
      # for (jk in ho){
      #   # lambda = -1. is a reciprocal transform.
      #   # lambda = -0.5 is a reciprocal square root transform.
      #   # lambda = 0.0 is a log transform.
      #   # lambda = 0.5 is a square root transform.
      #   # lambda = 1.0 is no transform.
      #   # print(jk)
      #   out <- AID::boxcoxnc(Xtr[,jk], method = "mle", lambda = seq(-2,2,0.5), verbose = F, plot = F,
      #                   lambda2 = 0)
      #   x.lambda <- out$lambda.hat
      #   Xtr[,jk] <- (Xtr[,jk] ^ x.lambda - 1) / x.lambda
      # }
      try({
        pcamodel_test <- fit_pca(Xtr, A, xscale = xscale)
        Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                               pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
        id.loc <- ((para_test$Repetition==jrep) & (para_test$Coltrans==rm_pctges[jmd]))
        for(a in c(1:A)){
          para_test[[paste0("t",a,"radius")]][id.loc] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
          para_test[[paste0("p",a,"corr")]][id.loc] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                               ref.P[,a]))
        }
        
        para_test$limspe[id.loc] <- pcamodel_test$model$limspe
        para_test$limt2[id.loc] <- pcamodel_test$model$limt2
        para_test$log10mspe[id.loc] <- log10(pcamodel_test$loss)
      })
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$Coltrans == rm_pctges[jmd],cols.sum], 
                                         na.rm = TRUE), 4), 
                          " (", round(apply(para_test[para_test$Coltrans == rm_pctges[jmd],cols.sum], 
                                            2, sd, na.rm = TRUE), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho.list))
}


#' Fit PCA with different levels of variable transformations
#'
#' @param data A dataframe or matrix with the data
#' @param A The dimensionality of the latent space
#' @param k_ho The number of holdout repetitions at each percentage
#' @param rm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vpca_rowpctge <- function (data, A, ref.P, k_ho=1000,
                           rm_pctges=c(1,5,10,seq(20,100,by=20)),
                           ho.part = NULL, xscale = FALSE){
  X <- as.matrix(data)
  nrep <- k_ho
  # nlevels <- length(rm_pctges)
  nlevels <- 1
  n.results <- nrep*nlevels
  n.elems <- ncol(X)
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
  # para_test$Coltrans <- rep(rm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition")))
  d.sum <- data.frame(matrix(NA,nrep, length(cols.sum)))
  # rownames(d.sum) <- paste0("rw rem ", rm_pctges)
  rownames(d.sum) <- paste0("rep ", c(1:nrep))
  if(is.null(ho.part)){
    ho.list <- createFolds(c(1:nrow(X)), k = k_ho, list = TRUE, returnTrain = TRUE)
  }
  for (jrep in c(1:nrep)){
    ho <- ho.list[[jrep]]
    Xtr <-  sweep(X[ho,,drop=F], 1, rowSums(X[ho,,drop=F]), "/")
    try({
      pcamodel_test <- fit_pca(Xtr, A, xscale = xscale)
      Xrec_cv <- sweep(sweep(pcamodel_test$model$x%*%t(pcamodel_test$model$rotation),2,
                             pcamodel_test$model$scale,"*"),2,pcamodel_test$model$center,"+")
      id.loc <- (para_test$Repetition==jrep)
      for(a in c(1:A)){
        para_test[[paste0("t",a,"radius")]][id.loc] <- pcamodel_test$model$limits_t[[paste0("pc",a)]][2]
        para_test[[paste0("p",a,"corr")]][id.loc] <- abs(cor(pcamodel_test$model$rotation[,a],
                                                             ref.P[,a]))
      }
      
      para_test$limspe[id.loc] <- pcamodel_test$model$limspe
      para_test$limt2[id.loc] <- pcamodel_test$model$limt2
      para_test$log10mspe[id.loc] <- log10(pcamodel_test$loss)
    })
  }
  d.sum <- paste0(round(colMeans(para_test[,cols.sum],na.rm = TRUE), 4), 
                        " (", round(apply(para_test[,cols.sum], 2, sd, na.rm = TRUE), 4), ")")
  print(d.sum)
names(d.sum) <- colnames(para_test)[cols.sum]
return(list(para_test = para_test, d.sum = d.sum, ho = ho.list))
}