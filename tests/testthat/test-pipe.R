context('pipe operations')

test_that("Function composition with pipes works.", {
  `%>%` <- fapply::`%>%`
  head_5_to_10 <- fapply(head, n=10) %>%  tail
  expect_equal(head_5_to_10(iris), iris[5:10,])
})

test_that("Function composition and partial function evaluation works.", {
  `%>%` <- fapply::`%>%`
  head_9_to_10 <- fapply(head, n=10) %>% fapply(tail, n=2)
  expect_equal(head_9_to_10(iris), iris[9:10,])
})
test_that("Function composition and partial function evaluation works 2.",{
  v <- fapply(head, n = ncol(x)) %>% fapply(tail, n=2) %>% nrow
  expect_equal(2, v(iris))
})

test_that("Function composition and partial function evaluation works 3.", {
  `%>%` <- fapply::`%>%`
  head_9th <- fapply(head, n=10) %>%
    fapply(tail, n=2) %>%
    fapply(head, n=1)
  expect_equal(head_9th(iris), iris[9,])
})

