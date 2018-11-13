library(fc)

head3 <- fc(head, n=3)
if ( !identical(head3(iris), iris[1:3,]) ) {
  stop("Partial function evaulation failed.")
}

if ( !identical(fc(tail, x=head(y, n=10))(iris), iris[5:10,]) ) {
  stop("Argument modifier failed.")
}

if (!identical(fc(matrix, data= data, ncol = 3)(1:9),
               fc(base::matrix, data= data, ncol = 3)(1:9))) {
  stop("Unable to accept package-qualified function names.")
}

head_1_to_10 <- fc(head, n=10)
head_5_to_10 <- fc(tail, x=head_1_to_10(x))

if ( !identical(head_5_to_10(iris), iris[5:10,]) ) {
  stop("Function composition failed.")
}

set.seed(5)
v <- runif(10)
set.seed(5)
sumtwice <- fc(sum, x=x, y=x)
if ( !identical(sumtwice(v), sum(v)*2) ) {
  stop("Passing random parameter, sampled once, failed.")
}

head_1_to_10 <- fc(head, n=10)
head_9_to_10 <- fc(tail, x=head_1_to_10(x), n=2)

if ( !identical(head_9_to_10(iris), iris[9:10,]) ) {
  stop("Function composition and partial function evaluation failed.")
}


set.seed(1)
rand_binoms <- fc(rbinom, n=n, size=abs(round(rnorm(n, 20))),
                      prob=1/abs(round(rnorm(n, 10))) )
rb_samples <- rand_binoms(10)
set.seed(1)
size <- abs(round(rnorm(10, 20)))
prob <- 1/abs(round(rnorm(10, 10)))

if ( !identical(rb_samples, rbinom(10, size, prob)) ) {
  stop("Generalized function composition failed.")
}

rand_f <- fc(rf,
             df1=abs(round(rnorm(n, 20))),
             df2=abs(round(rnorm(n, 10))), ncp=4)
set.seed(1)
fc_rf_samples <- rand_f(10)
set.seed(1)
rf_samples <- rf(10,
                 abs(round(rnorm(10, 20))),
                 abs(round(rnorm(10, 10))), ncp=4)
if ( !identical(fc_rf_samples, rf_samples) ) {
  stop(paste("Generalized function composition and partial function",
             "evaluation failed."))
}

# Evaluate the function and stash it in the return function environment.
first <- fc(head, x = fc(head, n=1)(x))
if ( !identical( iris[1,], first(iris)) ) {
  stop("Function composition with anonymous functions failed.")
}

first <- fc(function(x) x[1,], x=x)
if ( !identical(iris[1,], first(iris)) ) {
  stop("Function composition with anonymous functions failed.")
}

x <- iris
if ( !identical(fc(head, x=tail(x))(x), tail(x)) ) {
  stop("We're not evaluating variables at the right time.")
}

# matrix is kind of an exception.
gendata <- fc(rnorm, mean=0)

v <- fc(matrix, data=gendata(n), ncol=2)

set.seed(1)
vals <- v(200)
set.seed(1)
truevals <- matrix(rnorm(200, mean = 0), ncol=2)

if ( !identical(truevals, vals) ) {
  stop("Compose functions to pass data into matrix failed.")
}

# but this is faster...
f <- fc(summary, object=tail(head(x)))

if ( !identical(f(iris), summary(tail(head(iris)))) ) {
  stop("Compose functions with different primary arguments failed.")
}

#fc(gsub, pattern=".*>(.*)<.*", replacement = "\\1")

