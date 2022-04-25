#' Compare the dimensions of the original and new datasets
#'
#' @description Used to decide what comparison strategy to employ, as some
#'   techniques are more sensitive to a change in a specific dimension than
#'   others
#'
#' @param dat_original Original dataset
#' @param dat_comparison Dataset to compare
#'
#' @return Indicator value: 0 = both dimensions equal, 1 = number of rows equal,
#'   2 = number of columns equal, 3 = neither dimensions equal
#'   
#' @export

compare_rows_cols <- function(dat_original, dat_comparison){
  
  m1 <- ncol(dat_original)
  n1 <- nrow(dat_original)
  
  m2 <- ncol(dat_comparison)
  n2 <- nrow(dat_comparison)
  
  if (m1 == m2 & n1 == n2) {
    return(0)
  }
  
  # At present, these cases all return FALSE, but will eventually be expanded out into their own
  if (m1 == m2 & n1 != n2) {
    return(1)
  }
  
  if (m1 != m2 & n1 == n2) {
    return(2)
  }
  
  if (m1 != m2 & n1 != n2) {
    return(4)
  }
  
}


check_all_numeric <- function(dat) {
  # Check that all columns in the input datasets are numberic
  if (any(apply(dat, 2, is.numeric)) == FALSE) {
    stop("Not all columns are numeric.")
  }
}