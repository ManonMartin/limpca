#' @importFrom plyr is.formula

# Check of the entry arguments
checkArg <- function(arg, checks, can.be.null = FALSE) {
    check.list <- list(
        bool = c(is.logical, "a boolean"),
        int = c(
            function(x) {
                identical(
                    as.numeric(x),
                    round(as.numeric(x))
                )
            },
            "an integer"
        ),
        num = c(is.numeric, "a numeric"),
        str = c(is.character, "a character string"),
        pos = c(function(x) {
            min(x) > 0
        }, "positive"),
        pos0 = c(function(x) {
            min(x) >= 0
        }, "positive or zero"),
        formula = c(plyr::is.formula, "a formula"),
        data.frame = c(is.data.frame, "a dataframe"),
        matrix = c(is.matrix, "a matrix"),
        list = c(is.list, "a list"),
        length1 = c(function(x) {
            length(x) == 1
        }, "of length 1")
    )

    if (is.null(arg)) {
        if (!can.be.null) {
            stop(deparse(substitute(arg)), " is null.")
        }
    } else {
        for (cc in checks) {
            if (!check.list[[cc]][[1]](arg)) {
                stop(
                    deparse(substitute(arg)), " is not ",
                    check.list[[cc]][[2]], "."
                )
            }
        }
    }
}
