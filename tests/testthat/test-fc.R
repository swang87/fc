library(fapply)
library(testthat)

context('fc operations')

test_that("Partial function evaluation works.", {
  head3 <- fc(head, n=3)
  expect_equal(head3(iris), iris[1:3,])
})

fc( fc(tail, n=2), x=head(y) )

test_that("fc argument modifier works.", {
  # You can do this...
  expect_equal(fc(tail, x=fc(head, n=10)(x) )(iris), iris[5:10,] )

  # But this is faster...
  fc(tail, x=head(x, n=10))(iris)
})

test_that("Function composition works.", {
  head_1_to_10 <- force(fc(head, n=10))

  head_5_to_10 <- fc(tail, x=head_1_to_10(x))

  # testthat is having trouble finding head_5_to_10. So, we'll explicitly
  # assign it to head_5_to_10's environment.... Lame.

  environment(head_5_to_10)[['head_1_to_10']] <- head_1_to_10

  expect_equal(head_5_to_10(iris), iris[5:10,])
})

test_that("Allow passing in random parameters, sampled once.", {
  set.seed(5)
  v <- runif(10)
  set.seed(5)
  sumtwice <- fc(sum, x=x, y=x)
  expect_equal(sumtwice(v), sum(v)*2)
  
})

test_that("Function composition and partial function evaluation works.", {
  head_1_to_10 <- fc(head, n=10)
  head_9_to_10 <- fc(tail, x=head_1_to_10(x), n=2)

  # Same trick, different test
  environment(head_9_to_10)[['head_1_to_10']] <- head_1_to_10

  expect_equal(head_9_to_10(iris), iris[9:10,])
})

test_that("Generalized function composition works.", {
  set.seed(1)
  rand_binoms <- fc(rbinom, n=n, size=abs(round(rnorm(n, 20))),
                        prob=1/abs(round(rnorm(n, 10))) )
  rb_samples <- rand_binoms(10)
  set.seed(1)
  size <- abs(round(rnorm(10, 20)))
  prob <- 1/abs(round(rnorm(10, 10)))
  expect_equal(rb_samples, rbinom(10, size, prob))
})

test_that(
  "Generalized function composition and partial function evaluation work",
  {
    rand_f <- fc(rf, 
                 df1=abs(round(rnorm(n, 20))),
                 df2=abs(round(rnorm(n, 10))), ncp=4)
    set.seed(1)
    fc_rf_samples <- rand_f(10)
    set.seed(1)
    rf_samples <- rf(10, 
                     abs(round(rnorm(10, 20))), 
                     abs(round(rnorm(10, 10))), ncp=4)
    expect_equal(fc_rf_samples, rf_samples)
  })

test_that("Function composition with anonymous functions works.", {
  # Does this run fast?
  first <- fc(head, x = {function(x) x[1,]} (x))
  
  # Evaluate the function and stash it in the return function environment.
  first <- fc(head, x = fc(head, n=1)(x))
  expect_equal(iris[1,], first(iris))
})

test_that("Function composition with anonymous functions works 2.", {
  first <- fc(function(x) x[1,], x=x)
  expect_equal(iris[1,], first(iris))
})


test_that("We're not evaluating variables at the wrong time.", {
  x <- iris
  expect_equal(fc(head, x=tail(x))(x), tail(x))
})

test_that("Can compose functions to pass data into matrix().", {
  # matrix is kind of an exception.
  gendata <- fc(rnorm, mean=0)

  v <- fc(matrix, data=gendata(n), ncol=2)

  # Blech.
  environment(v)[['gendata']] <- gendata

  set.seed(1)
  vals <- v(200)
  set.seed(1)
  truevals <- matrix(rnorm(200, mean = 0), ncol=2)
  expect_equal(truevals, vals)
})

test_that("Can compose functions with different primary arguments.", {
  # You can do this...
  f <- fc(summary, object = fc(tail, x=head(x))(object))

  # but this is faster...
  f <- fc(summary, object=tail(head(x)))
  expect_equal(f(iris), summary(tail(head(iris))))
})

fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1")

