#' Fit Autoencoder, extract coefficients
#'
#' @param dat Dataset
#' @param A Number of neurons in latent layer
#' @param tr Information about training
#' @param kcv Number of folds used for cross-validation
#'
#' @return loss_results optimal autoencoder configuration
#' @export
AEoptlayers <- function(dat, A, tr = NULL, kcv = 10, 
                               act.fun = act.fun, n.epochs = n.epochs){
  X <- as.matrix(dat)
  # If training and testing partitions of the cross validation are not supplied 
  # in tr, then create them:
  Xts <- X[c((nrow(X)-50):nrow(X)),,drop=F]
  Xtr <- X[-c((nrow(X)-50):nrow(X)),,drop=F]
  if (is.null(tr)){
    tr <- createFolds(c(1:nrow(Xtr)), k = kcv, list = TRUE, returnTrain = TRUE)
    kcv <- length(tr)
    if(kcv<10){
      tr <- createFolds(c(1:nrow(Xtr)), k = nrow(Xtr), list = TRUE, returnTrain = TRUE)
      kcv <- nrow(Xtr)
    }
  } else {
    kcv <- length(tr)
  }
  n_lay <- 2^seq(0,floor(sqrt(ncol(Xtr))),by=1)
  n_lay <- n_lay[and(n_lay > A, n_lay < ncol(Xtr))] 
  n_lay <- sort(c(n_lay, ncol(Xtr)),decreasing = TRUE)
  # Initialise the matrix storing the results of the loss function to be optimised
  loss_opt_tr <- matrix(NA, length(n_lay), kcv)
  loss_opt_ts <- matrix(NA, length(n_lay), kcv)
  loss_opt_ts_cv <- matrix(NA, length(n_lay), kcv)
  # library(keras)
  
  for (k.A in c(1:length(n_lay))){
    print(paste("N layers at ", n_lay[k.A]))
    modelA <- keras_model_sequential()
    modelA %>%
      layer_dense(units=ncol(Xtr), activation = act.fun, input_shape = ncol(Xtr), 
                  use_bias = TRUE, name = "input")
    if (k.A > 1){
      for(k.layer in c(2:k.A)){
        modelA %>%
          layer_dense(units=n_lay[k.layer], activation = act.fun, input_shape = n_lay[k.layer-1], 
                      use_bias = TRUE, name = paste0("hidd_in_",n_lay[k.layer]))
      }
      modelA %>%
        layer_dense(units=A, activation = act.fun, input_shape = ncol(Xtr), 
                    use_bias = TRUE, name = "latent") 
      for(k.layer in seq(k.A,2,by=-1)){
        modelA %>%
          layer_dense(units=n_lay[k.layer], activation = act.fun, input_shape = n_lay[k.layer+1], 
                      use_bias = TRUE, name = paste0("hidd_out_",n_lay[k.layer]))
      }
      modelA %>%
        layer_dense(units=ncol(Xtr), activation = act.fun, input_shape = k.A, 
                    use_bias = TRUE, name = "output")
    } else {
      modelA %>%
        layer_dense(units=A, activation = act.fun, input_shape = ncol(Xtr), 
                    use_bias = TRUE, name = "latent") %>%
        layer_dense(units=ncol(Xtr), activation = act.fun, input_shape = A, 
                    use_bias = TRUE, name = "output")
    }
    summary(modelA)
    # Compile the model
    modelA %>% compile(
      loss="mean_squared_error",
      optimizer="adam"
    )
    tensorflow::tf$config$run_functions_eagerly(TRUE)
    tensorflow::tf$data$experimental$enable_debug_mode()
    for (k.cv in c(1:kcv)){
      Xtr.cv <- Xtr[tr[[k.cv]],,drop=F]
      Xts.cv <- Xtr[-tr[[k.cv]],,drop=F]
      modelA %>% fit(
        x=Xtr,
        y=Xtr,
        epochs=n.epochs,
        verbose=0,
        batch_size = 5
      )
      # Evaluate the model
      loss_opt_tr[k.A, k.cv] <- evaluate(modelA, Xtr.cv, Xtr.cv)
      loss_opt_ts_cv[k.A, k.cv] <- evaluate(modelA, Xts.cv, Xts.cv)
      loss_opt_ts[k.A, k.cv] <- evaluate(modelA, Xts, Xts)
    }
  }
  return(loss_results = list(losstr = loss_opt_tr, 
                             lossts = loss_opt_ts,
                             losstscv = loss_opt_ts_cv,
                             A = A, 
                             tr = tr,
                             n_lay=n_lay))
  
}