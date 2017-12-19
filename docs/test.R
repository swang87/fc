library(fapply)

iris %>% (fapply(head, n = ncol(x)) %>%
          fapply(tail, n=2))
