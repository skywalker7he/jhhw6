#' Calculate the length of the hypotenuse of a right triangle
#' @export
#' @param height a numeric value of the height of a right triangle
#' @param base a numeric value of the base of a right triangle
#' @return The length of the hypotenuse of the right triangle

get_hypotenuse_length <- function(height, base) {
  # Concatenate values of the two argues to a vector
  sides <- c(height, base)
  if (!is.numeric(x = sides)) {
    # Check if the vector is numeric
    stop("Sides can not be non-numeric!")
  } else if (any(sides < 0)) {
    # Check if any elements of the vector is less than 0
    stop("Side must be positive!")
  } else {
    # Calculate and return the length of the hypotenuse
    # using the Pythagorean theorem
    return(sqrt(x = sum(sides ^ 2)))
  }
}
