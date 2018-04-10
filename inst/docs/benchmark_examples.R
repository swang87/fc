library(fapply)

library(magrittr)
library(purrr)
library(microbenchmark)


log_sqrt_f <- function(x) log(sqrt(x))

log_sqrt_compose <- purrr::compose(log, sqrt)
`%>%` <- magrittr::`%>%`
log_sqrt_pipe <- . %>% sqrt %>% log
log_sqrt_fapply <- fapply(log, x=sqrt(x))
log_sqrt_fc <- fc(log, x=sqrt(x))

microbenchmark::microbenchmark(log_sqrt_f(10),
                               log_sqrt_compose(10), log_sqrt_pipe(10),
                               log_sqrt_fapply(10),
                               log_sqrt_fc(10), times = 10000)
library(compiler)
c1 <- cmpfun(log_sqrt_compose)
c2 <- cmpfun(log_sqrt_pipe)
c3 <- cmpfun(log_sqrt_fapply)
c4 <- cmpfun(log_sqrt_fc)

microbenchmark::microbenchmark(log_sqrt_f(10),
                               c1(10), c2(10),
                               c3(10), c4(10), times = 10000)


### example with something that is not the primary argument
x <- c("A>2348asd<", "B>234<")
search_sub_trim <- function(v) trimws(gsub(grep(v, pattern="A", value=TRUE),
                                           pattern=".*>(.*)<.*", replacement = "\\1"))
`%>%` <- magrittr::`%>%`
search_sub_trim_mag <- . %>% grep(pattern="A", x=., value=TRUE) %>%
  gsub(".*>(.*)<.*", "\\1", x=.) %>%
  trimws
search_sub_trim_mag(x)

`%>%` <- fapply::`%>%`

search_sub_trim_f <- fapply(grep, pattern="A", value=TRUE) %>%
  fapply(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws

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
                                 search_sub_trim_mag(x), times = 10)
c1 <- cmpfun(search_sub_trim_f)
c2 <- cmpfun(search_sub_trim_mag)

microbenchmark::microbenchmark(c1(x), c2(x), times = 10)
