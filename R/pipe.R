# idea: x %>% (f1() %>% f2()) should be converted to x %>>% f1() %>>>% f2()
#       f1() %>% f2() should be converted to f1() %>>>% f2()
`%>>%` <- function(lhs, rhs) { # lhs is data, rhs is func
  if (!is.function(rhs)) stop("Error: rhs is not a function.")
  rhs(lhs)
}

`%>>>%` <- function(lhs, rhs) { # lhs, rhs both funcs
  # LHS should either be a function or a call to fapply()
  lhs_string <- paste(deparse(lhs), collapse="\n")
  # RHS should always be a function result of fapply
  left_to_right_arg <- names(formals(rhs)[1])
  rhs_string <- paste(deparse(rhs), collapse="\n")
  eval(parse(text = paste0("fapply(",rhs_string, ", ",
                           left_to_right_arg, " = ", lhs_string,
                           ")")))
}
#' @export
`%>%` <- function(lhs, rhs) {
  require(codetools)
  fun_args <- as.list(match.call())

  # detect if first argument is a data frame
  lhs_fun <- strsplit(as.character(as.expression(fun_args$lhs)), "%>%")[[1]][1]
  lhs_fun <- try(is.function(eval(parse(text=lhs_fun))), silent =TRUE)
  if (class(lhs_fun) == "try-error") lhs_fun <- TRUE
  if (lhs_fun) {
    # lhs is function, assume lhs always pipes into first argument of rhs fun
    #lhs %>>>% eval(parse(text = gsub("%>%", "%>>>%", rhs, fixed=TRUE)))
    new_call <- as.character(as.expression(match.call()))
    new_call <- gsub("%>%", "%>>>%", new_call, fixed=TRUE)
    eval(parse(text=new_call))
  } else {
    # is.function() eval to FALSE
    # lhs is data

    # check if rhs has only one function
    rhs <- as.character(as.expression(fun_args$rhs))
    if (!grepl("%>%", rhs)) {
      lhs %>>% eval(parse(text = paste0("identity() %>>>%", rhs)))
    } else # otherwise
      lhs %>>% eval(parse(text = gsub("%>%", "%>>>%", rhs, fixed=TRUE)))
  }
}

