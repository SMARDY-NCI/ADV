dat <- read.csv("data/BEIA_wide_format.csv")


get_pca_scores <- function(dat) {
  
  dat %>%
    # Remove non-numeric data
    select_if(is.numeric) %>%
    
    # Run PCA analysis, centering and scaling data
    prcomp(center = TRUE,
           scale = TRUE) %>%
    
    # Extract scores, and convert to dataframe
    magrittr::extract2("x") %>%
    data.frame() %>%
    
    # Sort dataset by all columns
    arrange(across(everything())) %>%
    return()
  
}

rescale_255 <- function(dat){
  
  dat %>%
    # Convert to greyscale pixel values, range 0,255
    mutate(across(everything(), ~scales::rescale(., to = c(0,255)))) %>%
    return()
}
  

#' Title
#'
#' @param dat 
#'
#' @return A "QR" style plot of the dataframe

plot_qr <- function(dat){

plot <- dat %>%
  # Generate id variable to help with plotting
  mutate(id = 1:nrow(.)) %>%
  # Pivot to long format for plotting
  tidyr::pivot_longer(cols = -id, names_to = "name") %>%
  # Convert from pixel value to 
  # For greyscale, the RGB values of the hex are identical e.g. #828282
  # Need to round, as as.hexmode() cannot handle decimals
  mutate(color = paste0(
    "#",
    as.hexmode(round(value)),
    as.hexmode(round(value)),
    as.hexmode(round(value))
  )) %>%
  # Create plot, using scale_fill_identity to colour based on each cell's hex
  ggplot(aes(x = name, y = id, fill = color)) +
  geom_raster() +
  scale_x_discrete(NULL, expand = c(0, 0)) +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_identity()

return(plot)

}


#' Title
#'
#' @param dat 
#' @param block_size 
#'
#' @return
#' @export
#'
#' @examples
block_averaging <- function(dat, block_size = 3, block_size_n, block_size_m) {
  
  
  if (!hasArg(block_size_n)) {
    block_size_n <- block_size
  }
  
  if (!hasArg(block_size_m)) {
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
  
  # Create result dataframe
  
  purrr::map(colIndex, ~ dat2[, .x]) %>%
  
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

# Write tests here
tictoc::tic()
dat %>%
  # Apply PCA with centring/scaling, extract scores matrix & sort by first PC
  get_pca_scores() %>% 
  # Get average of each 3x3 submatrix present in the data
  block_averaging(block_size = 5) %>%
  # Rescale averaged values to fall between {0,...,255} (greyscale range)
  rescale_255() %>%
  # Plot data in "QR-style" plot
  plot_qr()
tictoc::toc()

