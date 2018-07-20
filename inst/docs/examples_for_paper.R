### Benchmark
library(fc)
library(purrr)
library(magrittr)
library(microbenchmark)

make_res_tbl <- function(x, units = "us") {
  N <- 10000 # num of its, for SE calc
  if (units == "us") {
    div <- 10^3
  } else if (units == "ms") {
    div <- 10^6
  } else {
    div <- 1
  }
  res <- paste0("$", round(aggregate(x$time ~ x$expr, FUN=mean)[,2]/div, 3), " (\\pm",
                round(aggregate(x$time ~ x$expr, FUN=sd)[,2]/sqrt(N)/div,3),")$")
  names(res) <- aggregate(x$time ~ x$expr, FUN=mean)[,1]
  res
}


## 1
log_sqrt_base <- function(x) log(x=sqrt(x))
log_sqrt_purrr <- purrr::compose(log, sqrt)
`%>%` <- magrittr::`%>%`

log_sqrt_mag <- . %>% sqrt %>% log
log_sqrt_fc <- fc(log, x=sqrt(x))
`%>%` <- fc::`%>%`
log_sqrt_fc_pipe <- log %>% sqrt

tmp <- microbenchmark::microbenchmark(log_sqrt_base(10),
                               log_sqrt_purrr(10), log_sqrt_mag(10),
                               log_sqrt_fc(10), log_sqrt_fc_pipe(10),
                               times = 10000)
res <- make_res_tbl(tmp, "us")

## 2

x <- c("<td class = 'address'>24 Hillhouse Ave.</td>",
       "<td class = 'city'>New Haven</td>",
       "</table>")
search_trim_base <- function(v) {
  trimws(gsub(grep(v, pattern="<[^/]*>", value=TRUE),
              pattern=".*>(.*)<.*", replacement = "\\1"))
}
`%>%` <- magrittr::`%>%`
search_trim_mag <- . %>% grep(pattern="<[^/]*>", x=., value=TRUE) %>%
  gsub(".*>(.*)<.*", "\\1", x=.) %>%
  trimws
search_trim_purrr <- purrr::compose(trimws, partial(gsub, pattern=".*>(.*)<.*",
                                                    replacement = "\\1"),
                                    partial(grep, pattern="<[^/]*>", value=TRUE))
search_trim_fc <- fc(trimws,
                     x = fc(gsub, pattern=".*>(.*)<.*",
                            replacement = "\\1",
                            x = fc(grep, pattern="<[^/]*>", value=TRUE)(x))(x))

`%>%` <- fc::`%>%`
search_trim_fc_pipe <- fc(grep, pattern="<[^/]*>", value=TRUE) %>%
  fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws

tmp2 <- microbenchmark::microbenchmark(search_trim_base(x),
                               search_trim_mag(x),
                               search_trim_purrr(x),
                               search_trim_fc(x),
                               search_trim_fc_pipe(x), times = 10000)
print(tmp2, digits = 3)
res <- rbind(res, make_res_tbl(tmp2, units = "us"))

## 3
library(fc)
library(dplyr)
library(dbplyr)
my_db <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")

library(nycflights13)
copy_to(my_db,
        flights,
        temporary = FALSE,
        indexes = list(
          c("year", "month", "day"),
          "carrier",
          "tailnum"
        )
)

flights_db <- tbl(my_db, "flights")
flights_db

flight_summary_base <- function(x) {
  filter_(summarize_(group_by_(x, .dots = list('tailnum')),
                    .dots = list(count = 'n()',
                                 dist='mean(distance, na.rm=TRUE)',
                                 delay='mean(arr_delay, na.rm=TRUE)')),
          .dots = list('count > 20', 'dist < 2000'))
}

`%>%` <- magrittr::`%>%`
flight_summary_mag <- . %>% group_by_(.dots = list('tailnum')) %>%
  summarize_(.dots = list(count = 'n()',
                                  dist='mean(distance, na.rm=TRUE)',
                                  delay='mean(arr_delay, na.rm=TRUE)')) %>%
  filter_(.dots = list('count > 20', 'dist < 2000'))

# flight_summary_mag <- . %>% group_by(tailnum) %>%
#   summarize(count = n(),
#             dist = mean(distance, na.rm=TRUE),
#             delay = mean(arr_delay, na.rm = TRUE)) %>%
#   filter(count > 20, dist < 2000)
library(fc)

flight_summary_purrr <- compose(partial(filter_, .dots = list('count > 20', 'dist < 2000')),
                                compose(partial(summarize_, .dots = list(count = 'n()',
                                                                            dist='mean(distance, na.rm=TRUE)',
                                                                            delay='mean(arr_delay, na.rm=TRUE)')),
                                        partial(group_by_, .dots = list('tailnum'))))
flight_summary_fc <- fc(filter_, .dots = list('count > 20', 'dist < 2000'),
                             .data = fc(summarize_, .dots = list(count = 'n()',
                                                      dist='mean(distance, na.rm=TRUE)',
                                                      delay='mean(arr_delay, na.rm=TRUE)'),
                             .data = fc(group_by_, .dots = list('tailnum'))(.data))(.data))
`%>%` <- fc::`%>%`
flight_summary_fc_pipe <-  fc(group_by_, .dots = list('tailnum')) %>%
  fc(summarize_, .dots = list(count = 'n()',
                              dist='mean(distance, na.rm=TRUE)',
                              delay='mean(arr_delay, na.rm=TRUE)')) %>%
  fc(filter_, .dots = list('count > 20', 'dist < 2000'))
flight_summary_base(flights_db)
flight_summary_mag(flights_db)
flight_summary_fc_pipe(flights_db)
tmp3 <- microbenchmark(flight_summary_base(flights_db),
  flight_summary_mag(flights_db),
                       flight_summary_purrr(flights_db),
                       flight_summary_fc(flights_db),
                       flight_summary_fc_pipe(flights_db), times = 10000)
print(tmp3, digits=3)



## Example 4
get_random_sepal <- function(x) head(x[sample(1:nrow(x)), grep("Sepal", colnames(x))], n=10)

get_random_sepal(iris)
`%>%` <- magrittr::`%>%`
get_random_sepal_mag <- . %>% (function(x) x[sample(1:nrow(x)), grep("Sepal", colnames(x))]) %>% head(n = 10) %>% summary

get_random_sepal_mag(iris)

get_sepal1 <- fc(summary, object = fc(head, x = (function(x) {
  x[sample(1:nrow(x)),
    grep("Sepal", colnames(x))]
}) (x), n=10)(x))

get_random_sepal_fc <- fc(summary, object=fc(head, x =
                            fc(function(x, cols) {x[sample(1:nrow(x)), cols]},
                               cols = grep("Sepal", colnames(x)))(x),
                          n=10)(x))

get_random_sepal_fc_2 <- fc(summary, object = fc(head, x =
                            (function(x) {
                              x[sample(1:nrow(x)),
                                grep("Sepal", colnames(x))]
                              }) (x), n=10)(x) )
`%>%` <- fc::`%>%`
get_random_sepal_fc_2_pipe <- { function(x) {
  x[sample(1:nrow(x)),
    grep("Sepal", colnames(x))]
}} %>% fc(head, n=10) %>% summary
get_random_sepal_fc_2_pipe(iris)
# `%>%` <- magrittr::`%>%`
# get_random_sepal_mag <- . %>% (function(x) {
#   x[sample(1:nrow(x)),
#     grep("Sepal", colnames(x))]
# } ) %>% head
get_random_sepal_mag(iris)

library(purrr)
get_random_sepal_purrr <- compose(function(x) {
  x[sample(1:nrow(x)),
    grep("Sepal", colnames(x))]
}, partial(head, n=10), summary)
get_random_sepal_purrr(iris)
tmp4 <- microbenchmark::microbenchmark(get_random_sepal(iris),
                                     get_random_sepal_mag(iris),
                                     get_random_sepal_purrr(iris),
                                     get_sepal1(iris),
                                     #get_random_sepal_fc(iris),
                                     #get_random_sepal_fc_2(iris),
                                     get_random_sepal_fc_2_pipe(iris),times = 10000)


# compile output
res <- cbind(make_res_tbl(tmp, units="us"),
             make_res_tbl(tmp2, units = "us"),
             make_res_tbl(tmp3, units = "ms"),
             make_res_tbl(tmp4, units = "ms"))

colnames(res) <- paste("Example", 1:4, paste0("(", c("us", "us", "ms", "ms"), ")"))
rownames(res) <- c("base R", "magrittr", "purrr", "fc", "fc_pipe")
library(xtable)
print(xtable(res), sanitize.text.function=function(x){x})
library(ggplot2)
autoplot(tmp)
