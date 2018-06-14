my_group_by <- function(x, col_name) {
  attributes(x)[['groups']] <- split(x = 1:nrow(x), x[,col_name])
  x
}

my_summarize <- function(x) {
  groups <- attributes(x)$groups
  count <- unlist(lapply(groups, length))
  dist <- unlist(lapply(groups, function(g) mean(x$distance[g], na.rm=TRUE)))
  delay <- unlist(lapply(groups, function(g) mean(x$arr_delay[g], na.rm=TRUE)))
  data.frame(count = count, dist = dist, delay = delay)
}

my_filter <- function(x) {
  x[!is.na(x$delay) & x$delay & as.integer(x$count) > 20 & x$dist < 2000,]
}
library(dplyr)
`%>%` <- dplyr::`%>%`
library(nycflights13)
data(flights)
# using NSE, dplyr
proc_tidy <- . %>% group_by(tailnum) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm=TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(delay, count > 20, dist < 2000) 

system.time(proc_tidy(flights))

proc_tidy <- . %>% my_group_by("tailnum") %>%
  my_summarize %>%
  my_filter

system.time(proc_tidy(flights))

#library(fc)
devtools::document("~/projects/fc")
data(flights)
`%>%` <- fc::`%>%`

proc_fc <- fc(my_group_by, col_name = "tailnum") %>% 
  my_summarize %>%
  fc(function(ret) 
    ret[!is.na(ret$delay) & ret$delay & as.integer(ret$count) > 20 & 
        ret$dist < 2000,],
    ret = ret) 
  
system.time(proc_fc(flights))
