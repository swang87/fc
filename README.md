# fc

---
![travis](https://travis-ci.org/swang87/fc.svg?branch=master)
[![](https://cranlogs.r-pkg.org/badges/fc)](https://cran.r-project.org/package=fc)

This is an R package that provides a streamlined, standard evaluation-based approach to function composition. 
Using `fc`, a sequence of functions can be composed together such that returned objects from composed functions are used as intermediate values directly passed to the next function. 

### Installation
To install this package in R, you can install it via the `devtools` package:

```
devtools::install_github("swang87/fc")
```

### Usage
The package can then be loaded using:

```
library(fc)
```

The workhorse of the package is the `fc()` function. Its first argument is either a named function or an anonymous function; subsequent arguments must be named arguments of this function.

### Getting Started

We can create a new function that uses partial function valuation to display the first 50 rows of a dataset with:

```
head50 <- fc(head, n=50)
```

The return function has a single argument `x`, inherited from the `head()` function. The function `head50()` consists of:

```
function (x)
{
  head(x, n = 50)
}
```

In order to perform function composition, multiple `fc()` calls could be used in a nested manner:

```
summary50 <- fc(summary, object=fc(head, n = 50)(object))
```

The pipe-forward operator `%>%` is also supported for defining a pipeline functions to be run from left-to-right. Note differences in usage compared to `magrittr` and other packages in the Tidyverse. 

```
summary50 <- fc(head, n=50) %>% summary
```

In particular, the pipe-forward operator supported by `fc` cannot accept a data object on its lefthand side. If one wishes to run the composed function on a data object without intermediate storage of the function itself, the following syntax is permissible:

```
(fc(head, n=50) %>% summary)(x)
```

### Examples

#### Example 1

```
log_sqrt_fc_pipe <- fc(log, x=x) %>% fc(sqrt, x=x)
```

This function takes the square root of the log of an input argument `x`.

#### Example 2 

```
get_sepal2_pipe <- fc(function(x, cols) {x[sample(1:nrow(x)), cols]},
                        cols = grep("Sepal", colnames(x))) %>%
                        fc(head, n = 10) %>% summary
```

#### More Information
More details can be found in our [working paper](https://arxiv.org/pdf/1806.11021.pdf).
