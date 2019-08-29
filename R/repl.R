#' Read–Eval–Print Loop
#'
#' @param prompt prompt
#' @param envir environment
#' @export
repl <- function(prompt = "lisprr> ", envir = parent.frame()) {
  repeat {
    input <- readline(prompt)
    if (identical(input, "q")) {
      cat("bye.\n")
      break
    }
    val <- tryCatch(evaluate(input, envir),
                    error = identity)
    str <- if (inherits(val, "error")) c(" ERROR:", val[[1]]) else to_string(val)
    cat(str)
    cat("\n")
  }
}

to_string <- function(exp) {
  if (is.list(exp)) {
    sprintf("(%s)", do.call(paste, lapply(exp, to_string)))
  } else {
    tryCatch(as.character(exp),
             error = function(e) sprintf("#<%s>", typeof(exp)))
  }
}
