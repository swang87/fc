library(fc)

library(magrittr)
library(purrr)
library(microbenchmark)


# NOTE THAT log(sqrt(x)) runs faster!
log_sqrt_base <- function(x) log(x=sqrt(x))
log_sqrt_compose <- purrr::compose(log, sqrt)
`%>%` <- magrittr::`%>%`
log_sqrt_mag <- . %>% sqrt %>% log
log_sqrt_fc <- fc(log, x=sqrt(x))
`%>%` <- fc::`%>%`
log_sqrt_fc_pipe <- fc(log, x=x) %>% fc(sqrt, x=x)

microbenchmark::microbenchmark(log_sqrt_base(10),
                               log_sqrt_compose(10), log_sqrt_mag(10),
                               log_sqrt_fc(10), log_sqrt_fc_pipe(10),
                                                                 times = 10000)

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


set.seed(99)
z <- sample(10000,4,TRUE)
microbenchmark(z %>% unique %>% list, list(unique(z)),
               fc(function(y) {list(y)}, y=fc(unique)(z)))


library(quantmod)
symetf = c('XLY','XLP','XLE','XLF','XLV','XLI','XLB','XLK','XLU','SPY')
end <- format(Sys.Date(),"%Y-%m-%d")
start <- format(as.Date("2010-01-01"),"%Y-%m-%d")
retd <- list()
for (i in 1:length(symetf)) {
  dat0 = getSymbols(symetf[i], src="yahoo", from=start, to=end, auto.assign = F)
  retd[[i]] <- as.numeric(dat0[2:NROW(dat0),4])/as.numeric(dat0[1:(NROW(dat0)-1),4]) -1
}
out <- microbenchmark(retd %>% lapply(unlist), lapply(retd, unlist),
                      fc(lapply, FUN="unlist")(retd))
out
boxplot(out, names= c("One with pipes", 'One with no pipes'), unit= "ms")


library(babynames) # data package
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)   # for graphics

babynames %>%
  filter(name %>% substr(1, 3) %>% equals("Ste")) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>%
  qplot(year, total, color = sex, data = ., geom = "line") %>%
  add(ggtitle('Names starting with "Ste"')) %>%
  print

library(fc)
tmp <- fc(filter, ...=(function(x) fc(substr, x=x, start=1, stop=3)=="Ste")(name))
tmp(babynames)



### DPLYR Examples
library(microbenchmark)
library(dplyr)
library(fc)
library(nycflights13)



# example 1

`%>%` <- fc::`%>%`
a <- fc(filter_, .dots = list('month == 3', 'dest == "DFW"')) %>%
  fc(group_by_, .dots = list('carrier')) %>%
  fc(summarize_, .dots = list(avg='mean(arr_delay, na.rm=T)'))
a(flights)


`%>%` <- dplyr::`%>%`

microbenchmark(flights %>% filter(month == 3, dest == "DFW") %>% group_by(carrier) %>%
                 summarize(avg=mean(arr_delay, na.rm=T)),
               a(flights))

tmp <- flights %>% filter(month == 3, dest == "DFW") %>% group_by(carrier) %>%
  summarize(avg=mean(arr_delay, na.rm=T))
tmp



## Example 2

`%>%` <- fc::`%>%`
a <- fc(group_by_, .dots = list('tailnum')) %>%
  fc(summarize_, .dots = list(count = 'n()',
                              dist='mean(distance, na.rm=TRUE)',
                              delay='mean(arr_delay, na.rm=TRUE)')) %>%
  fc(filter_, .dots = list('count > 20', 'dist < 2000'))
a(flights)
`%>%` <- dplyr::`%>%`
microbenchmark(flights %>% group_by(tailnum) %>%
                 summarize(count = n(),
                           dist = mean(distance, na.rm=TRUE),
                           delay = mean(arr_delay, na.rm = TRUE)) %>%
                 filter(delay, count > 20, dist < 2000),
               a(flights))
tmp <- flights %>% group_by(tailnum) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm=TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(delay, count > 20, dist < 2000)


### Database Examples

library(dplyr)
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


## Example 2

`%>%` <- fc::`%>%`
a <- fc(group_by_, .dots = list('tailnum')) %>%
  fc(summarize_, .dots = list(count = 'n()',
                              dist='mean(distance, na.rm=TRUE)',
                              delay='mean(arr_delay, na.rm=TRUE)')) %>%
  fc(filter_, .dots = list('count > 20', 'dist < 2000'))
a(flights_db)
`%>%` <- dplyr::`%>%`
microbenchmark(flights_db %>% group_by(tailnum) %>%
                 summarize(count = n(),
                           dist = mean(distance, na.rm=TRUE),
                           delay = mean(arr_delay, na.rm = TRUE)) %>%
                 filter(delay, count > 20, dist < 2000),
               a(flights_db))
tmp <- flights_db %>% group_by(tailnum) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm=TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(delay, count > 20, dist < 2000)
head(tmp)
head(a(flights_db))
dim(tmp)
