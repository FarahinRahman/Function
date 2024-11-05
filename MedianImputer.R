# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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
  Age = c(28, 30, NA, 45, 50, 23, 34, 19),
  Salary = c(57000, NA, 58000, NA, 63000, 66000, 42000, 78000),
  Experience = c(NA, 3, 9,NA , 8, 10, 5, NA)
)
detect_missing_values(sample_data)

median_impute <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

sample_data <- sapply(sample_data, median_impute)


print(sample_data)


