#' Calculate a trimmed mean for a numeric vector
#' @importFrom utils head
#' @importFrom utils tail
#' @export
#' @param x a numeric vector
#' @param l the l largest values
#' @param s the s smallerst values
#' @return The trimmed mean of the numeric vector
#' @examples
#' trimmed_mean(c(1, 7, 3, 2, 5, 0.5, 9, 10), 1, 2)

trimmed_mean <- function(x, l, s) {
  if (any(!is.numeric(x), !is.numeric(c(l, s)))) {
    # Check if the values provided by the users are numeric
    stop("Numeric values/vector are required!")
  } else if (length(x) < l + s + 1) {
    # Check if the length of the numeric vector is greater than or equal to l + s + 1
    stop("The length of the numeric vector entered is too short!")
  } else {
    max_l <- tail(x[order(x)], l) # Select out the l largest values
    min_s <- head(x[order(x)], s) # Select out the s smallest values
    # Get the trimmed vector by filtering out the repetitive elements
    numbers_to_be_trimmed <- c(min_s, max_l)
    trimmed_x <- setdiff(x, numbers_to_be_trimmed)
    return(mean(trimmed_x))
  }
}


