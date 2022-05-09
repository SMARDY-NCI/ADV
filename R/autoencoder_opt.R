#' Perform PCR, extract scores matrix and sort
#'
#' @param dat Dataset
#' @param maxA Maximum number of neurons in latent layer
#' @param kcv Number of folds used for cross-validation
#'
#' @return loss_results optimal autoencoder configuration
#' @export
autoenc_opt <- function(dat, A.values = c(1:10), tr = NULL, kcv = 10) {
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
  library(keras)
  
  for (k.A in A.values){
    modelA <- keras_model_sequential()
    modelA %>%
      layer_dense(units=ncol(X), activation = "tanh", input_shape = ncol(X), 
                  use_bias = TRUE, name = "input") %>%
      layer_dense(units= k.A, activation = "tanh", input_shape = ncol(X), 
                  use_bias = TRUE, name = "latent") %>%
      layer_dense(units=ncol(X), activation = "tanh", input_shape = k.A, 
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
      Xtr <- X[tr[[k.cv]],,drop=F]
      Xts <- X[(c(1:nrow(X) %in% tr[[k.cv]])),,drop=F]
      modelA %>% fit(
        x=Xtr,
        y=Xtr,
        epochs=50,
        verbose=0,
        batch_size = 5
      )
      # Evaluate the model
      loss_opt_tr[k.A, k.cv] <- evaluate(modelA, Xtr, Xtr)
      loss_opt_ts[k.A, k.cv] <- evaluate(modelA, Xts, Xts)
    }
  }
  return(loss_results = list(losstr = loss_opt_tr, 
                             lossts = loss_opt_ts,
                             A = A.values, 
                             tr = tr))
  
}

