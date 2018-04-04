
is_fc_function <- function(expr) {
  substr(as.character(as.expression(expr)),
          1, 3) == "fc("
}

get_all_symbols <- function(a) {
  ret <- c()
  for (i in 1:length(a)) {
    lp <- language_properties(a[[i]])
    ret <- c(ret, c(lp$functions, lp$variables))
  }
  ret
}

make_anon_func_name <- function(a) {
  symbols <- get_all_symbols(a)
  ret <- "internal_anon_func"
  i <- 1
  while(ret %in% symbols) {
    new_ret_name <- paste0(ret, "_", i)
    if ( !(new_ret_name %in% symbols) ) {
      ret <- new_ret_name
      break
    }
    i <- i + 1
  }
  ret
}

args_to_string <- function(args) {
  ret <- c()
  for(i in seq_len(length(args))) {
    if (length(args[[i]]) == 1 && as.character(args[[i]]) == "") {
      ret <- c(ret, names(args)[i])
    } else {
      if (is.character(args[[i]])) {
        args[[i]] <- deparse(args[[i]])
      } 
      ret <- c(ret, paste(names(args)[i], as.character(args[i]), sep=" = "))
    }
  }
  ret
}

unbound_args <- function(args) {
  ub <- unlist(lapply(args, 
               function(x) length(x) == 1 && 
               as.character(x) == "" && !(!is.symbol(x) && is.na(x))))
  names(args)[ub]
}

make_function <- function(args, body, env) {
  f <- function() {}
  formals(f) <- args
  body(f) <- body
  environment(f) <- env
  f
} 

#' @export
fc <- function(func, ...) {
  # Get the arguments.
  arg_list <- as.list(match.call())
 
  # Create the return function body and environment. 
  ret_fun_body_string <- ""
  ret_fun_env <- new.env()

  # If func is an fc or an anonymous function, then evaluate it. 
  # We'll keep it in the return function's evironment so we can use it.
  if (is_fc_function(arg_list$func) || is_anon_function(arg_list$func)) {
    anon_func_name <- make_anon_func_name(arg_list[3:length(arg_list)])
    ret_fun_env[[anon_func_name]] <- eval(arg_list$func)
    func_name <- anon_func_name
    func_formals <- formals(ret_fun_env[[anon_func_name]])
  } else {
    func_name <- as.character(arg_list[[2]])
    fe <- eval(arg_list[[2]])
    if (!is.function(fe)) {
      stop("The first argument must be a function.")
    }
    func_formals <- formals(fe)
  }
  ff_dots <- which(names(func_formals) == "...")
  if (length(ff_dots) > 0) {
    func_formals <- func_formals[-which(names(func_formals) == "...")]
  }

  ################################
  # Process the "..." arguments.
  ################################
  
  arg_formals <- arg_list[-(1:2)] 

  if (any(names(arg_formals) == "")) {
    stop("All parameter arguments must be named.")
  }

  # The following speeds up the case where one of the ... arguments
  # is anther fc.
  for (i in seq_along(arg_formals)) {
    if (is_fc_function(arg_formals[[i]])) {
      if (length(arg_formals[[i]]) == 1) {
        stop(paste0("Problem with argument", i+1, 
                    ".  You must supply parameters to composed functions."))
      }
      anon_func_name <- make_anon_func_name(
        c(arg_list[3:length(arg_list)], as.list(ret_fun_env)))

      # Stash the function in the return function environement.
      ret_fun_env[[anon_func_name]] <- 
        eval(parse(text=as.character(arg_formals[[i]][1])))
      arg_formals[[i]] <- parse(text=(paste0(anon_func_name, "(", 
        paste(arg_formals[[i]][-1], collapse=", "), ")")))[[1]]
    }
  }

  # pevn - Parameter expression variables names. The variables that show
  # up on the right side of the expressions in ... They must appear
  # as parameters in the returned function.
  dot_names <- lapply(arg_formals, get_variable_names)
  pevn <- unlist(dot_names)
  formals_from_func <- setdiff(unbound_args(func_formals), names(arg_formals))
  ret_fun_body_string <- paste0(func_name, "(", 
    paste(c(formals_from_func, args_to_string(arg_formals)), collapse=", "), 
    ")")

  ret_fun_arg_names <- gsub("$", "=", union(unbound_args(arg_formals), pevn),
                            perl=TRUE)
  ret_fun_arg_names <- union(
    gsub("$", "=", setdiff(unbound_args(func_formals), names(pevn))), 
    ret_fun_arg_names)
  ret_fun_arg <- eval(parse(text=
    paste0("alist(", paste(ret_fun_arg_names, collapse=","), ")")))
  make_function(ret_fun_arg, 
                as.call(c(as.name("{"), parse(text=ret_fun_body_string))),
                ret_fun_env)
}
