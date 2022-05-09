#' Perform PCR, extract scores matrix and sort
#'
#' @param dat Dataset
#' @param maxA Maximum number of neurons in latent layer
#' @param kcv Number of folds used for cross-validation
#'
#' @return loss_results optimal autoencoder configuration
#' @export
autoenc_opt <- function(dat, A.values = c(1:10), tr = NULL, kcv = 10, 
                        act.fun = act.fun, n.epochs = n.epochs){
  X <- as.matrix(dat)
  # If training and testing partitions of the cross validation are not supplied 
  # in tr, then create them:
  Xts <- X[c((nrow(X)-50):nrow(X)),,drop=F]
  Xtr <- X[-c((nrow(X)-50):nrow(X)),,drop=F]
  if (is.null(tr)){
    tr <- createTimeSlices(c(1:nrow(Xtr)), nrow(Xtr) - kcv, horizon = 1, fixedWindow = TRUE, skip = 0)
    kcv <- length(tr$train)
    if(kcv<10){
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
  loss_opt_ts_cv <- matrix(NA, maxA, kcv)
  # library(keras)
  
  for (k.A in A.values){
    modelA <- keras_model_sequential()
    modelA %>%
      layer_dense(units=ncol(Xtr), activation = act.fun, input_shape = ncol(Xtr), 
                  use_bias = TRUE, name = "input") %>%
      layer_dense(units= k.A, activation = act.fun, input_shape = ncol(Xtr), 
                  use_bias = TRUE, name = "latent") %>%
      layer_dense(units=ncol(Xtr), activation = act.fun, input_shape = k.A, 
                  use_bias = TRUE, name = "output")
    
    summary(modelA)
    # Compile the model
    modelA %>% compile(
      loss="mean_squared_error",
      optimizer="adam"
    )
    tensorflow::tf$config$run_functions_eagerly(TRUE)
    tensorflow::tf$data$experimental$enable_debug_mode()
    for (k.cv in c(1:kcv)){
      Xtr.cv <- Xtr[tr$train[[k.cv]],,drop=F]
      Xts.cv <- Xtr[-tr$train[[k.cv]],,drop=F]
      modelA %>% fit(
        x=Xtr,
        y=Xtr,
        epochs=n.epochs,
        verbose=0,
        batch_size = 5
      )
      # Evaluate the model
      loss_opt_tr[k.A, k.cv] <- evaluate(modelA, Xtr, Xtr)
      loss_opt_ts_cv[k.A, k.cv] <- evaluate(modelA, Xts.cv, Xts.cv)
      loss_opt_ts[k.A, k.cv] <- evaluate(modelA, Xts, Xts)
    }
  }
  return(loss_results = list(losstr = loss_opt_tr, 
                             lossts = loss_opt_ts,
                             losstscv = loss_opt_ts_cv,
                             A = A.values, 
                             tr = tr))
  
}

