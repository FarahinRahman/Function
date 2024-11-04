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

#Calculating mean,variance,standard deviation,median and IQR

stats_mtcars <- function(dataset){
  calculate_iqr <-function(v){
    IQR(v, na.rm = TRUE)
  }
  numeric_data <-dataset[sapply(dataset,is.numeric)]

  means <-sapply(numeric_data,mean,na.rm =TRUE)
  variances <- sapply(numeric_data,var,na.rm =TRUE)
  sds <- sapply(numeric_data,sd,na.rm =TRUE)
  medians<- sapply(numeric_data,median,na.rm =TRUE)
  iqrs<- sapply(numeric_data,calculate_iqr)

  result_df <-data.frame(
    Means = means,
    Variances = variances,
    StandardDeviation =sds,
    Medians = medians,
    IQRS = iqrs
  )
 return(result_df)
}

stats_results_df <- stats_mtcars(mtcars)
print(stats_results_df)
