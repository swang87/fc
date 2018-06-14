library(fc)
library(magrittr)
library(purrr)
library(microbenchmark)


log_sqrt_f <- function(x) log(x=sqrt(x))
log_sqrt_compose <- purrr::compose(log, sqrt)
`%>%` <- magrittr::`%>%`
log_sqrt_pipe <- . %>% sqrt %>% log
log_sqrt_fc <- fc(log, x=sqrt(x))

microbenchmark::microbenchmark(log_sqrt_f(10),
                               log_sqrt_compose(10), log_sqrt_pipe(10),
                               log_sqrt_fc(10), times = 10000)

x <- sample(3:3000)
microbenchmark::microbenchmark(log_sqrt_f(x),
                               log_sqrt_compose(x), log_sqrt_pipe(x),
                               log_sqrt_fc(x), times = 10000)
library(compiler)
c1_purr <- cmpfun(log_sqrt_compose)
c2_fc <- cmpfun(log_sqrt_fc)
c3_pipe <- cmpfun(log_sqrt_pipe)

microbenchmark::microbenchmark(log_sqrt_f(10),
                               c1_purr(10), 
                               c2_fc(10),
                               c3_pipe(10), times = 10000)
microbenchmark::microbenchmark(log_sqrt_f(x),
                               c1_purr(x), 
                               c2_fc(x),
                               c3_pipe(x), times = 10000)


### example with something that is not the primary argument
x <- c("A>2348asd<", "B>234<")
search_sub_trim <- function(v) {
  trimws(gsub(grep(v, pattern="A", value=TRUE),
                      pattern=".*>(.*)<.*", replacement = "\\1"))
}

`%>%` <- magrittr::`%>%`
search_sub_trim_mag <- . %>% grep(pattern="A", x=., value=TRUE) %>%
  gsub(".*>(.*)<.*", "\\1", x=.) %>%
  trimws
search_sub_trim_mag(x)

`%>%` <- fc::`%>%`

search_sub_trim_f <- fc(grep, pattern="A", value=TRUE) %>%
  fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws

search_sub_trim_f(x)


search_sub_trim_fc <- fc(grep, pattern="A", value=TRUE) %>%
  fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws

fc(trimws, x=fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1", x= fc(grep, pattern="A", value=TRUE)))
search_sub_trim_fc(x)

x <- paste0("    <td>", sample(LETTERS[1:5], 10000, replace=TRUE),
sample(1:250, 10000, replace=TRUE), "</td>      ")

system.time({
  search_sub_trim_f(x)
})

system.time({
  search_sub_trim_mag(x)
})

microbenchmark::microbenchmark(search_sub_trim(x),
                               search_sub_trim_f(x),
                                 search_sub_trim_mag(x), times = 100)
c1_fc <- cmpfun(search_sub_trim_f)
c2_mag <- cmpfun(search_sub_trim_mag)

microbenchmark::microbenchmark(c1_fc(x), c2_mag(x), times = 100)


