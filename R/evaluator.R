#' Evaluate s-expression
#'
#' @param s s-expression
#' @param ... additional arguments
#' @importFrom tailr loop_transform
#' @export
evaluate <- function(s, envir = parent.frame()) {
  exp <- translate(s)
  result <- base::eval(exp, envir)
  if (tail_call_optimization()) {
    fname <- defined_function_name(exp)
    if (is.name(fname)) {
      envir[[as.character(fname)]] <- do.call(tailr::loop_transform, list(fname))
    }
  }
  result
}

#' Translate s-expression to R
#'
#' @param s s-expression
#' @export
translate <- function(s) {
  compile(parse(s))
}

r_functions <- list(
  "begin" = quote(`{`),
  "set!" = quote(`<-`),
  "=" = quote(`==`),
  "eq?" = quote(`==`),
  "equal?" = identical,
  "not" = quote(`!`),
  "cons" = function(x, y) append(list(x), y),
  "car" = function(x) x[[1]],
  "cdr" = function(x) x[-1],
  "list?" = is.list,
  "null?" = function(x) identical(x, list()),
  "symbol?" = is.name
)

compile <- function(x) {
  if (is.character(x)) {                # variable reference
    as.name(x)
  } else if (!is.list(x)) {             # constant literal
    x
  } else if (length(x) == 0) {          # empty list
    x
  } else if (identical(x[[1]], "define")) {
    if (is.list(x[[2]])) {              # (define (var arg*) exp*)
      var <- x[[2]][[1]]
      args <- x[[2]][-1]
      exps <- x[-c(1, 2)]
      call("<-", as.name(var), compile(list("lambda", args, exps[[1]])))
    } else {                            # (define var exp)
      var <- x[[2]]
      exp <- x[[3]]
      call("<-", as.name(var), compile(exp))
    }
  } else if (identical(x[[1]], "lambda")) { # (lambda (var*) exp*)
    vars <- x[[2]]
    exps <- x[-c(1, 2)]
    args <- vector("list", length(vars))
    names(args) <- as.character(vars)
    body <- as.call(c(quote(`{`), lapply(exps, compile)))
    call("function", as.pairlist(args), body)
  } else {                              # other functions
    r_func <- if (!is.list(x[[1]])) r_functions[[x[[1]]]]
    if (is.null(r_func)) {
      as.call(lapply(x, compile))
    } else {
      as.call(c(r_func, lapply(x[-1], compile)))
    }
  }
}

tco <- function(enable) {
  options(lisprr_tail_call_optimization = as.logical(enable))
  tail_call_optimization()
}

tail_call_optimization <- function() {
  identical(getOption("lisprr_tail_call_optimization"), TRUE)
}

defined_function_name <- function(x) {
  if (identical(x[[1]], quote(`<-`)) && identical(x[[3]][[1]], quote(`function`))) x[[2]]
}
