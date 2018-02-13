library(fapply)


v <- fapply(dplyr::filter, Species == "setosa")
v(iris)

v <- fapply(dplyr::select_, .dots = list("Species"))
v(iris)

f <- function(x, b) {
  head(x, n=length(b))
}
v <- fapply(head, b=list(1,2,3))
