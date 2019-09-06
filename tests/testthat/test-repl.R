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

test_that("to_string", {
  expect_equal(to_string(1), "1")
  expect_equal(to_string(list(1, list(2))), "(1 (2))")
  expect_equal(to_string(function(x) x), "#<closure>")
})
