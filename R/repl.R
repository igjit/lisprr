#' Read–Eval–Print Loop
#'
#' @param prompt prompt
#' @param envir environment
#' @export
repl <- function(prompt = 'lisprr> ', envir = parent.frame()) {
    while(TRUE) {
        input <- readline(prompt)
        if (identical(input, "q")) {
            cat("bye.\n")
            break
        }
        val <- tryCatch(evaluate(input, envir),
                        error = function(e) e)
        str <- if (inherits(val, "error")) c(" ERROR:", val[[1]]) else to_string(val)
        cat(str)
        cat("\n")
    }
}
