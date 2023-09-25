#' Calculate a trimmed mean for a numeric vector
#' @export
#' @param x a numeric vector
#' @param l the l largest values
#' @param s the s smallerst values

trimmed_mean <- function(x, l, s) {
  if (any(!is.numeric(x), !is.numeric(c(l, s)))) {
    stop("Numeric values/vector are required!")
  } else if (length(x) < l + s + 1) {
    stop("The length of the numeric vector entered is too short!")
  } else {
    max_l <- tail(x[order(x)], l)
    min_s <- head(x[order(x)], s)
    numbers_to_be_trimmed <- c(min_s, max_l)
    trimmed_x <- setdiff(x, numbers_to_be_trimmed)
    return(mean(trimmed_x))
  }
}


