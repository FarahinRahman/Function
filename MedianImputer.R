
#' identify and replace any missing value in numeric
#' columns with the median of respective column
#'
#' @param data A data frame containing numeric columns with missing values.
#'
#' @return A data frame with NA values in numeric columns replaced by the median.
#' @export
#'
#' @examples data1 <-data.frame(
#' A = c(1,2,NA,4,5)
#' B = c(1,NA,3,4,5)
#' C = c(1,2,3,4,NA)
#' )
#' impute_median(data1)

detect_missing_values <- function(data){
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <-colnames(data)

  for (i in 1:ncol(data)){
    missing_counts[i] <-sum (is.na(data[[i]]))
  }
  missing_counts <-missing_counts[missing_counts >0]
  return (missing_counts)
}
sample_data <- data.frame(
  A = c(21, 30, NA, 44, 51, NA, 34, 19),
  S = c(57000, 75000, 58000, NA, 63000, 66000, 42000, 78000),
  E = c(3, 3, 9,NA , 8, 10, 5, NA)
)
detect_missing_values(sample_data)

median_impute <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

sample_data <- sapply(sample_data, median_impute)


print(sample_data)


