library(fc)

head_5_to_10 <- fc(head, n=10) %>%  tail

if ( !identical(head_5_to_10(iris), iris[5:10,]) ) {
  stop("Function composition with pipes failed.")
}

head_9_to_10 <- fc(head, n=10) %>% fc(tail, n=2)

if ( !identical(head_9_to_10(iris), iris[9:10,]) ) {
}

head_9th <- fc(head, n=10) %>%
  fc(tail, n=2) %>%
  fc(head, n=1)
if ( !identical(head_9th(iris), iris[9,]) ) {
  stop("Function composition and partial evaluation failed.")
}

x <- c("    <td>D159</td>      ", "    <td>B240</td>      ",
       "    <td>A166</td>      ", "    <td>A59</td>      ")
search_sub_trim_f <- fc(grep, pattern="A", value=TRUE) %>%
  fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1") %>% trimws
if ( !identical(search_sub_trim_f(x),
                trimws(gsub(grep(x, pattern="A", value=TRUE),
                            pattern=".*>(.*)<.*", replacement = "\\1"))) ) {
  stop("Piping with string values as agument parameters failed.")
}

{function(x) {
  x[sample(1:nrow(x)),
    grep("Sepal", colnames(x))]
}} %>% fc(head, n=10) 

