#' Parse s-expression
#'
#' @param s s-expression
parse <- function(s) {
    read_from(tokenize(s), 1)[[1]]
}

tokenize <- function(s) {
    s <- gsub("\\(", " ( ", s)
    s <- gsub("\\)", " ) ", s)
    s <- sub("^\\s+", "", s)

    strsplit(s, "\\s+")[[1]]
}

read_from <- function(tokens, i) {
    if (length(tokens) < i) stop("unexpected EOF while reading")

    if (tokens[i] == "(") {
        L <- list()
        i <- i + 1                      # skip "("
        while(tokens[i] != ")") {
            res <- read_from(tokens, i)
            L <- append(L, res[1])
            i <- res[[2]]
        }
        i <- i + 1                      # skip ")"
        return(list(L, i))
    } else if (tokens[i] == ")") {
        stop("unexpected )")
    } else {
        return(list(atom(tokens[i]), i + 1))
    }
}

atom <- function(token) {
    num <- suppressWarnings(as.numeric(token))
    if (is.na(num)) token else num
}
