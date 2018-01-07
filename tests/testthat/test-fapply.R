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
  v <- fapply(head, n = ncol(x)) %>%
    fapply(tail, n=2) %>% fapply(nrow)
  expect_equal(2, v(iris))
})

test_that("Piping operator works for applying composed function to data object.",{
  val <- iris %>% (fapply(head, n = ncol(x)) %>%
    fapply(tail, n=2) %>% fapply(nrow))
  expect_equal(2, val)
})

test_that("Piping operator works to pipe multiple arguments via a list.", {
  val <- list(x = iris, n = 3) %>% (fapply(tail, x=x$x, n=x$n) %>% fapply(head, n=2))
  expect_equal(iris[148:149,], val)
})

