library(fapply)
iris %>% (fapply(head, n = ncol(x)) %>%
          fapply(tail, n=2) %>% fapply(nrow))
v <- fapply(head, n = ncol(x)) %>%
    fapply(tail, n=2) %>% fapply(nrow)
v
v(iris)


list(x = iris, n = 3) %>% (fapply(tail, x=x$x, n=x$n) %>% fapply(head, n=2))
