# idea: x %>% (f1() %>% f2()) should be converted to x %>>% f1() %>>>% f2()
#       f1() %>% f2() should be converted to f1() %>>>% f2()
`%>>%` <- function(lhs, rhs) { # lhs is data, rhs is func
  if (!is.function(rhs)) stop("Error: rhs is not a function.")
  rhs(lhs)
}
`%>>>%` <- function(lhs, rhs) { # lhs, rhs both funcs
  tmpfun <- function() {}
  fun_args <- as.list(match.call())
  lhs <- as.character(as.expression(fun_args[[2]]))
  rhs <- as.character(as.expression(fun_args[[3]]))

  # process leftside
  if (grepl("%>>>%", lhs) | substr(lhs, 1, 10) == "(function(") {
    lhs_string <- eval(parse(text=lhs))
    lhs_string <- paste(deparse(lhs_string), collapse="\n")
  } else {
    body(tmpfun) <-  parse(text = paste0("{",  lhs,  "}"))
    lhs_fun <- findGlobals(tmpfun, merge=FALSE)$fun[2]
    lhs_args <- names(formals(lhs_fun))[1]
    lhs_other_args <- gsub(lhs_fun, "", lhs)
    lhs_other_args <- substr(lhs_other_args, 2, nchar(lhs_other_args)-1)
    if(nchar(lhs_other_args) > 0) lhs_other_args <- paste0(", ", lhs_other_args)
    lhs_string <- paste(lhs_fun, "(", lhs_args, lhs_other_args, ")")
  }


  # fiddle right
  body(tmpfun) <-  parse(text = paste0("{",  rhs,  "}"))
  rhs_fun <- findGlobals(tmpfun, merge=FALSE)$fun[2]
  rhs_args <- gsub(rhs_fun, "", rhs)
  rhs_args <- substr(rhs_args, 2, nchar(rhs_args)-1)
  if (nchar(rhs_args) > 0) rhs_args <- paste0(", ",rhs_args)

  left_to_right_arg <- names(formals(rhs_fun))[1]
  eval(parse(text = paste0("fapply(",rhs_fun, ", ",
                           left_to_right_arg, " = ", lhs_string,
                           rhs_args,")")))
}
`%>%` <- function(lhs, rhs) {
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

