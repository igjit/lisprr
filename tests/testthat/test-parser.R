test_that("parse", {
  expect_equal(parse("a"), "a")
  expect_equal(parse("1"), 1)
  expect_equal(parse("(1 2)"), list(1, 2))
  expect_equal(parse("(1 (2))"), list(1, list(2)))
  expect_equal(parse("()"), list())

  expect_error(parse("("), "unexpected EOF while reading")
  expect_error(parse(")"), "unexpected )")
})
