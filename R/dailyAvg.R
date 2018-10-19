#' @title Find an average over X values 
#' @param vec a vector of values 
#' @param every the number to average
#' @param na.rm remove NAs?
#' @return a vector 
#' @export
#' @author Mike Johnson
#' 
dailyAvg <- function (vec, every, na.rm = FALSE) {
  n <- length(vec)
  x <- .colMeans(vec, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
  x
}


