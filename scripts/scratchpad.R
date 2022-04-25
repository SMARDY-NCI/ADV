#Write tests here
library(ADV)
read.csv("data/BEIA_wide_format.csv") %>%
  # Apply PCA with centring/scaling, extract scores matrix & sort by first PC
  get_pca_scores() %>%
  # Get average of each 3x3 submatrix present in the data
  #block_averaging(block_size = 3) %>%
  # Rescale averaged values to fall between {0,...,255} (greyscale range)
  rescale_255() %>%
  # Plot data in "QR-style" plot
  plot_qr()
