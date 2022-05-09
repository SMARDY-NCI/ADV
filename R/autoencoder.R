#' Perform PCR, extract scores matrix and sort
#'
#' @param dat Dataset
#'
#' @return New dataset contain the scores matrix, ordered by the values of the
#'   first principal component
#' @export
get_pca_scores <- function(dat) {
  
  dat %>%
    # Remove non-numeric data, which the PCA can't handle
    dplyr::select_if(is.numeric) %>%
    
    # Sorting at this stage ensures that the signs of the PCA are the same
    dplyr::arrange(dplyr::across(dplyr::everything())) %>%
    
    # Run PCA analysis, centering and scaling data
    stats::prcomp(center = TRUE,
                  scale = TRUE) %>%
    
    # Extract scores matrix, and convert to dataframe
    magrittr::extract2("x") %>%
    data.frame() %>%
    
    # Sort dataset by all columns
    dplyr::arrange(dplyr::across(dplyr::everything())) %>%
    return()
  
}

#' Block averaging of non-overlapping submatrics in a given dataset
#'
#' @param dat Dataset containing only numeric colu
#' @param block_size Default is 3.
#' @param block_size_n Vertical block size. If not specified, defaults to
#'   block_size
#' @param block_size_m Horizontal block size. If not specified, defaults to
#'   block_size
#'
#' @return Dataframe containing results of 
#' @export

block_averaging <- function(dat, block_size = 3, block_size_n, block_size_m) {
  
  check_all_numeric(dat)
  
  # If vertical/horizontal block size is not specified, default to general block
  # size value
  if (!methods::hasArg(block_size_n)) {
    block_size_n <- block_size
  }
  
  if (!methods::hasArg(block_size_m)) {
    block_size_m <- block_size
  }
  
  # Convert data to a matrix of size N x M
  dat <- as.matrix(dat)
  
  # Get useful values
  n <- nrow(dat)
  m <- ncol(dat)
  
  # Argument checks
  stopifnot(m >= block_size_m,
            n >= block_size_n,
            block_size_m >1,
            block_size_n >1)
  
  # create a list of indices to use to partition the input matrix
  rowIndex <- split(seq(n), (seq(n) - 1) %/% block_size_n)
  colIndex <- split(seq(m), (seq(m) - 1) %/% block_size_m)
  
  # Create result dataframe ---
  
  
  purrr::map(colIndex, ~ dat[, .x]) %>%
    
    # This step ensures that if the split above returns a submatrix of a single
    # column, it is retained as a matrix rather than as a numeric vector
    lapply(as.matrix) %>%
    
    # Split 
    lapply(function(t) {
      purrr::map(rowIndex, ~ t[.x,],)
    }) %>%
    
    unlist(recursive = FALSE) %>%
    
    lapply(mean) %>%
    
    unlist() %>%
    
    matrix(nrow = length(rowIndex)) %>%
    # Convert to dataframe and return
    as.data.frame() %>%
    return()
  
}

#' Convert to greyscale pixel values, i.e. range {0,...,255}
#'
#' @param dat Dataset containing only numeric variables
#'
#' @return Dataframe with all variables scaled to between
#' @export
rescale_255 <- function(dat){
  
  check_all_numeric(dat)
  
  dat %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~scales::rescale(., to = c(0,255)))) %>%
    return()
}

#' Plot greyscale image of dataframe
#'
#' @param dat A dataframe scaled to range {0,...,255}, i.e. a dataframe that has
#'   already had rescale_255() applied to it
#'
#' @return A "QR" style plot of the dataframe
#' @export
#' @importFrom rlang .data

plot_qr <- function(dat){
  
  plot <- dat %>%
    # Generate id variable to help with plotting
    dplyr::mutate(id = 1:nrow(.data)) %>%
    # Pivot to long format for plotting
    tidyr::pivot_longer(cols = -.data$id, names_to = "name") %>%
    # Convert from pixel value to 
    # For greyscale, the RGB values of the hex are identical e.g. #828282, and 
    # Need to round, as as.hexmode() cannot handle decimals
    dplyr::mutate(color = paste0(
      "#",
      as.hexmode(round(.data$value)),
      as.hexmode(round(.data$value)),
      as.hexmode(round(.data$value))
    )) %>%
    
    # Create plot, using geom_raster and scale_fill_identity to colour based on
    # each cell's hex
    ggplot2::ggplot(ggplot2::aes(x = .data$name, y = .data$id, fill = .data$color)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    # Expand plot to fill image
    ggplot2::scale_x_discrete(NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0)) +
    # Remove everything else (axis, etc)
    ggplot2::theme_void()
  
  return(plot)
  
}

#' Fit autoencoder
#'
#' @param X A dataframe or matrix with the data
#' @param k.A The dimensionality of the latent layer
#' @param act.fun The activation function 
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
fit_autoencoder <- function(X, k.A, act.fun, n.epochs=50){
  X <- as.matrix(X)
  modelA <- keras_model_sequential()
  modelA %>%
    layer_dense(units=ncol(X), activation = act.fun, input_shape = ncol(X), 
                use_bias = TRUE, name = "input") %>%
    layer_dense(units= k.A, activation = act.fun, input_shape = ncol(X), 
                use_bias = TRUE, name = "latent") %>%
    layer_dense(units=ncol(X), activation = act.fun, input_shape = k.A, 
                use_bias = TRUE, name = "output")
  
  summary(modelA)
  # Compile the model
  modelA %>% compile(
    loss="mean_squared_error",
    optimizer="adam")
  tensorflow::tf$config$run_functions_eagerly(TRUE)
  tensorflow::tf$data$experimental$enable_debug_mode()
  history <- modelA %>% fit(
    x=X,
    y=X,
    epochs=n.epochs,
    verbose=0,
    batch_size = 5
  )
  # plot(history)
  # Evaluate the model
  loss <- evaluate(modelA, X, X)
  return(list(model = modelA, loss = loss, trainhist = history))
}

#' Fit Autoencoder with different levels of row removal percentages
#'
#' @param data A dataframe or matrix with the data
#' @param A The dimensionality of the latent space
#' @param k_ho The number of holdout repetitions at each percentage
#' @param rowrm_pctges The percentages of rows being removed
#'
#' @return The model and the obtained loss function (MSE)
#' @export
#' 
vautoencoder_removerows <- function (data, A, ref.P, k_ho=1000,
                                     rowrm_pctges=c(1,5,10,seq(20,80,by=20)),
                                     ho.part = NULL){
  library(caret)
  X <- as.matrix(data)
  nrep <- k_ho
  nlevels <- length(rowrm_pctges)
  n.results <- nrep*nlevels
  
  # Build list containing the results
  para_test <- list(log10mspe = matrix(NA,n.results,1))
  for(a in 1:A){
    para_test <- cbind(para_test,as.data.frame(matrix(NA,nrow = n.results,ncol=2)))
    colnames(para_test)[(ncol(para_test)-1):ncol(para_test)] <- 
      c(paste0("t",a,"radius"), paste0("p",a,"corr"))
  }
  para_test$Repetition <- rep(c(1:nrep),each = nlevels)
  para_test$RWoutpctge <- rep(rowrm_pctges,times = nrep)
  
  print("Parameter avge (sd)")
  cols.sum <- which(!(colnames(para_test)%in%c("Repetition", "RWoutpctge")))
  d.sum <- data.frame(matrix(NA, nlevels, length(cols.sum)))
  rownames(d.sum) <- paste0("rw rem ", rowrm_pctges)
  if(is.null(ho.part)){
    ho.list <- vector(mode = "list", length = nlevels)
  }
  # LOOP with rest of row percentage removal
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
      Xts <- X[-ho,,drop=F]
      model.AE_test <- fit_autoencoder(Xtr, A, "tanh")
      intermediate_layer_model <- keras_model(inputs = model.AE_test$model$input, 
                                              outputs = get_layer(model.AE_test$model,"latent")$output)
      intermediate_layer_coefs <- intermediate_layer_model$layers[[3]]$weights[[1]]
      intermediate_layer_output <- predict(intermediate_layer_model, Xtr)
      
      z <- ((nrow(Xtr) - 1) * (nrow(Xtr) - 1)/nrow(Xtr)) * stats::qbeta(1 - 0.05, 1, 
                                                (nrow(Xtr) - 3)/2)
      for(a in c(1:A)){
        para_test[[paste0("t",a,"radius")]][jrep] <- sqrt(stats::var(intermediate_layer_output[, a]) * z)
        para_test[[paste0("p",a,"corr")]][jrep] <- abs(cor(as.numeric(intermediate_layer_coefs[,a]),
                                                           as.numeric(ref.P[,a])))
      }
      id.loc <- and((para_test$Repetition==jrep),(para_test$RWoutpctge==rowrm_pctges[jmd]))
      para_test$log10mspe[id.loc] <- log10(evaluate(model.AE_test$model, Xtr, Xtr))
    }
    d.sum[jmd,] <- paste0(round(colMeans(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum]), 4), 
                          " (", round(apply(para_test[para_test$RWoutpctge == rowrm_pctges[jmd],cols.sum], 2, sd), 4), ")")
    # print(d.sum[jmd,])
  }
  colnames(d.sum) <- colnames(para_test)[cols.sum]
  return(list(para_test = para_test, d.sum = d.sum, ho = ho.list))
}