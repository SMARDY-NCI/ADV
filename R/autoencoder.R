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