context('fapply operations')

test_that("Partial function evaluation works.", {
  head3 <- fapply(head, n=3)
  expect_equal(head3(iris), iris[1:3,])
})

test_that("Function composition works.", {
  head_1_to_10 <- fapply(head, n=10)
  head_5_to_10 <- fapply(tail, x=head_1_to_10(x))
  expect_equal(head_5_to_10(iris), iris[5:10,])
})
test_that("Allow passing in random parameters, sampled once.", {
  set.seed(5)
  v <- runif(10)
  set.seed(5)
  sumtwice <- fapply(sum, x=.dots, y=.dots, .dots = runif(10))
  expect_equal(sumtwice(), sum(v)*2)
})
test_that("Function composition and partial function evaluation works.", {
  head_1_to_10 <- fapply(head, n=10)
  head_9_to_10 <- fapply(tail, x=head_1_to_10(x), n=2)
  expect_equal(head_9_to_10(iris), iris[9:10,])
})

test_that("Generalized function composition works.", {
  set.seed(1)
  rand_binoms <- fapply(rbinom, size=abs(round(rnorm(n, 20))),
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
    set.seed(1)
    rand_f <- fapply(rf, df1=abs(round(rnorm(n, 20))),
                     df2=abs(round(rnorm(n, 10))), ncp=4)
    rf_samples <- rand_f(10)
    set.seed(1)
    df1 <- abs(round(rnorm(10, 20)))
    df2 <- abs(round(rnorm(10, 10)))
    ncp <- 4
    expect_equal(rf_samples, rf(10, df1, df2, ncp))
  })
test_that("Function composition with anonymous functions works.", {
  first <- fapply(head, x = function(x) x[1,])
  expect_equal(iris[1,], first(iris))
})

test_that("Function composition with anonymous functions works 2.", {
  first <- fapply(function(x) x[1,], x = function(x) x[1:16, ])
  expect_equal(iris[1,], first(iris))
})

test_that("Piping operator works for function composition.",{
  v <- fapply(head, n = ncol(x)) %>% fapply(tail, n=2) %>% nrow
  expect_equal(2, v(iris))
})



test_that("We're not evaluating variables at the wrong time.", {
  # TODO: fix this.
  x <- iris
  expect_equal(fapply(head, x=tail(x))(x), tail(x))
})

test_that("Can compose functions to pass data into matrix().", {
  set.seed(1)
  gendata <- fapply(rnorm, mean = 0)
  v <- fapply(matrix, ncol = 2, data=gendata(data))
  vals <- v(200)
  set.seed(1)
  truevals <- matrix(rnorm(200, mean = 0), ncol=2)
  expect_equal(truevals, vals)
})

test_that("Can compose functions with different primary arguments.", {
  f <- fapply(summary, object = fapply(tail, x=head(x))(object))
  expect_equal(f(iris), summary(tail(head(iris))))
})
