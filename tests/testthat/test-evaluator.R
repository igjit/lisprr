context("test-evaluator.R")

test_that("arithmetic operations", {
  expect_equal(evaluate("(+ 1 2)"), 3)
  expect_equal(evaluate("(* 2 3)"), 6)
  expect_equal(evaluate("(- 10 3)"), 7)
  expect_equal(evaluate("(/ 100 4)"), 25)
})

test_that("list processing", {
  expect_equal(evaluate("(list)"), list())
  expect_equal(evaluate("(list 1 2)"), list(1, 2))
  expect_equal(evaluate("(car (list 10 20))"), 10)
  expect_equal(evaluate("(cdr (list 10 20 30))"), list(20, 30))
  expect_equal(evaluate("(cons 1 (cons 2 (quote ())))"), list(1, 2))
  expect_equal(evaluate("(cons (list 1 2) (quote ()))"), list(list(1, 2)))
})

test_that("if", {
  expect_equal(evaluate("(if T 2 3)"), 2)
  expect_equal(evaluate("(if F 2 3)"), 3)
})

test_that("quote", {
  expect_equal(evaluate("(quote abc)"), as.name("abc"))
  expect_equal(evaluate("(quote ())"), list())
})

test_that("begin", {
  expect_equal(evaluate("(begin 1)"), 1)
  expect_equal(evaluate("(begin 1 2 3)"), 3)
})

test_that("lambda", {
  expect_equal(evaluate("((lambda (a b) (+ a b)) 1 2)"), 3)
  expect_equal(evaluate("((lambda (a) (+ a 1)) 1)"), 2)
  expect_equal(evaluate("((lambda (l) (cdr l)) (list 10 20))"), list(20))
  expect_equal(evaluate("((lambda () (+ 1 2)))"), 3)
})

test_that("symbol?", {
  expect_equal(evaluate("(symbol? (quote x))"), TRUE)
  expect_equal(evaluate("(symbol? 1)"), FALSE)
})

test_that("=", {
  expect_true(evaluate("(= 1 1)"))
  expect_false(evaluate("(= 1 2)"))
})

test_that("equal?", {
  expect_equal(evaluate("(equal? (list 1 (list 2)) (list 1 (list 2)))"), TRUE)
})

test_that("set!", {
  env <- new.env()
  evaluate("(set! a 1)", env)
  expect_equal(env$a, 1)
  evaluate("(set! a 2)", env)
  expect_equal(env$a, 2)
  evaluate("(set! a (+ 1 2))", env)
  expect_equal(env$a, 3)
})

test_that("define", {
  expect_equal(evaluate("(begin (define add2 (lambda (x) (+ x 2))) (add2 40))"), 42)
  expect_equal(evaluate("(begin (define add3 (lambda (x) (set! x (+ x 1)) (set! x (+ x 2)) x)) (add3 4))"), 7)
  expect_equal(evaluate("(begin (define (add2 x) (+ x 2)) (add2 40))"), 42)
  expect_equal(evaluate("(begin (define a 1) (set! a 100) a)"), 100)
  expect_equal(evaluate("(begin (define a 1) (set! a (list 2 3)) a)"), list(2, 3))
  expect_equal(evaluate("(begin (define l (list 2 3 (list 4 5))) l)"), list(2, 3, list(4, 5)))

  env <- new.env()
  evaluate("(define (map f l) (if (null? l) (quote ()) (cons (f (car l)) (map f (cdr l)))))", env)
  expect_equal(evaluate("(map (lambda (x) (+ x 10)) (list 1 2 3))", env), list(11, 12, 13))
})

test_that("translate", {
  expect_equal(translate("(set! a 1)"), quote(a <- 1))
  expect_equal(translate("(not a)"), quote(!a))
})
