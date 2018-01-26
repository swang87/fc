library(magrittr)
library(purrr)
library(microbenchmark)
library(fapply)


log_sqrt_f <- function(x) log(sqrt(x))

log_sqrt_compose <- purrr::compose(log, sqrt)

log_sqrt_pipe <- . %>% sqrt %>% log
log_sqrt_fapply <- fapply(log, x=sqrt(x))
microbenchmark::microbenchmark(log_sqrt_f(10),
                               log_sqrt_compose(10), log_sqrt_pipe(10),
                               log_sqrt_fapply(10), times = 10000)




