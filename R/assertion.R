# Check if the length of x is a specific value
assert_length <- function(x, len = 1L) {
  name_x <- substitute(x)
  text_x <- deparse(name_x)
  name_len <- substitute(len)
  text_len <- vapply(name_len[-1], deparse, FUN.VALUE = character(1))
  if (!length(x) %in% len) {
    stop(
      "Object ",
      text_x,
      " must have length ",
      paste0(text_len, collapse = " or "),
      ".",
      call. = FALSE
    )
  }
}

# Check if both inputs have the same length
# @param allow_one if FALSE, the lengths have to be exactly the same. If TRUE, one of them can have length one.
assert_same_length <- function(a, b, allow_one = TRUE) {
  name_a <- substitute(a)
  name_b <- substitute(b)
  text_a <- deparse(name_a)
  text_b <- deparse(name_b)
  la <- length(a)
  lb <- length(b)

  if (allow_one) {
    if (la == 1 || lb == 1) {
      return()
    }
  }

  if (la != lb) {
    stop(
      "The length of arg \"",
      text_a,
      "\" should equal the length of arg \"",
      text_b,
      "\"",
      if (allow_one) " if both of them have more than one value." else ".",
      call. = FALSE
    )
  }
  return()
}


# Check if both inputs are null or not null.
# @param nonnull Logical. The value return when both of the inputs are not null.
# @value value of \code{nonnull} argument if both inputs are not NULL, \code{!nonnull} if both inputs are NULL, and error if
# one is NULL and the other isn't.
assert_same_nullness <- function(a, b, nonnull = TRUE) {
  name_a <- substitute(a)
  name_b <- substitute(b)
  text_a <- deparse(name_a)
  text_b <- deparse(name_b)
  null_a <- is.null(a)
  null_b <- is.null(b)

  if (null_a && null_b) {
    return(!nonnull)
  }

  if ((!null_a) && (!null_b)) {
    return(nonnull)
  }

  stop(
    "If one is specified, both \"",
    text_a,
    "\" and \"",
    text_b,
    "\" need to be specified.",
    call. = FALSE
  )
}

# Check if elements in x are all unique (no duplicates)
assert_unique <- function(x) {
  name_x <- substitute(x)
  text_x <- deparse(name_x)
  if (anyDuplicated(x)) {
    stop("Object ", text_x, " cannot contain duplicated values.", call. = FALSE)
  }
}

# Check if there is no missing value in x
assert_no_missing <- function(x) {
  name_x <- substitute(x)
  text_x <- deparse(name_x)
  if (anyDuplicated(x)) {
    stop("Object ", text_x, " cannot contain missing values.", call. = FALSE)
  }
}

# Check if x is of specific classes
assert_class <- function(x, class) {
  name_x <- substitute(x)
  text_x <- deparse(name_x)
  if (!inherits(x, class)) {
    stop(
      "Object ",
      text_x,
      " must be from class(es): ",
      paste0(class, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check if x is a vector (not a matrix or a data frame)
assert_vector <- function(x) {
  name_x <- substitute(x)
  text_x <- deparse(name_x)
  if (!is.vector(x)) {
    stop("Object ", text_x, " must be a vector (not a matrix)", call. = FALSE)
  }
}
