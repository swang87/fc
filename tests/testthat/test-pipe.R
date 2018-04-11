context('pipe operations')

test_that("Function composition with pipes works.", {
  `%>%` <- fapply::`%>%`
  head_5_to_10 <- fc(head, n=10) %>%  tail
  expect_equal(head_5_to_10(iris), iris[5:10,])
})

test_that("Function composition and partial function evaluation works.", {
  `%>%` <- fapply::`%>%`
  head_9_to_10 <- fc(head, n=10) %>% fc(tail, n=2)
  expect_equal(head_9_to_10(iris), iris[9:10,])
})

test_that("Function composition and partial function evaluation works 2.", {
  `%>%` <- fapply::`%>%`
  head_9th <- fc(head, n=10) %>%
    fc(tail, n=2) %>%
    fc(head, n=1)
  expect_equal(head_9th(iris), iris[9,])
})

test_that("Piping with string values as argument parameters works.", {
  `%>%` <- fapply::`%>%`
  x <- c("    <td>D159</td>      ", "    <td>B240</td>      ",
         "    <td>A166</td>      ", "    <td>A59</td>      ")
  search_sub_trim_f <- fc(grep, pattern="A", value=TRUE) %>%
    fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws
  expect_equal(search_sub_trim_f(x),
               trimws(gsub(grep(x, pattern="A", value=TRUE),
                           pattern=".*>(.*)<.*", replacement = "\\1")))
})

