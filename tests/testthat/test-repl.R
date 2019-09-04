test_that("repl", {
  readline_mock <- function(lines) {
    i <- 0
    function(prompt) lines[i <<- i + 1]
  }
  expect_output(
    with_mock(readline = readline_mock(c("(+ 1 2)", "q")),
              repl()),
    paste("3", "bye.", sep = "\n"))
  expect_output(
    with_mock(readline = readline_mock(c(")", "q")),
              repl()),
    "ERROR: unexpected )")
})
