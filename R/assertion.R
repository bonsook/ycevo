# Check if both inputs have the same length
# @param allow_one if FALSE, the lengths have to be exactly the same. If TRUE, one of them can have length one.
assert_same_length <- function(a, b, allow_one = TRUE) {
  name_a <- substitute(a)
  name_b <- substitute(b)
  text_a <- deparse(name_a)
  text_b <- deparse(name_b)
  la <- length(a)
  lb <- length(b)
  
  if(allow_one) {
    if(la == 1 || lb == 1)
      return()
  }
  
  if(la != lb) {
    stop("The length of arg \"",
         text_a, 
         "\" should equal the length of arg \"", 
         text_b, "\"",
         if(allow_one) " if both of them have more than one value." else ".", 
         call. = FALSE)
  }
  return()
}

