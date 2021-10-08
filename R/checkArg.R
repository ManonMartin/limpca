checkArg = function(arg, checks, can.be.null = FALSE){
  check.list <- list(bool = c(is.logical, "a boolean"),
                     int = c(function(x) {x%%1 == 0}, "an integer"),
                     num = c(is.numeric, "a numeric"),
                     str = c(is.character, "a character string"),
                     pos = c(function(x) {x > 0 }, "positive"),
                     pos0 = c(function(x) {x >= 0}, "positive or zero"),
                     formula = c(plyr::is.formula,"a formula"),
                     data.frame = c(is.data.frame,"a dataframe"),
                     matrix = c(is.matrix,"a matrix"),
                     list = c(is.list,"a list"),
                     length1 = c(function(x) {length(x) == 1}, "of length 1"))

  if (is.null(arg))  {
    if (!can.be.null)  {
      stop(deparse(substitute(arg)), " is null.")
    }
  } else  {
    for (c in checks) {
      if (!check.list[[c]][[1]](arg)) {
        stop(deparse(substitute(arg)), " is not ", check.list[[c]][[2]],".")
      }
    }
  }
}
