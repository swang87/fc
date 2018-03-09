#' @title Generalized Function Composition and Partial Function Evaluation
#'
#' @description 'fapply' is used to modify functions. It can be used to
#' compose function with specified function parameters and it can be used
#' to set parameter values (partial function evaluation).
#'
#' @param func the function to be modified.
#' @param ... the function modifiers (see Details).
#' @param .dots additional arguments, possibly results of function evaluations, where the evaluation should only occur once
#' @return A modified function based on the parameters provided.
#' @details The 'fapply' function works by capturing function modifier
#' expressions in a list, which can be applied to the specified function
#' via the 'do.call' function.
#' The function make use of standard R evaluation only. The 'substitute'
#' function is not used and modifiers expressions must be syntatically valid.
#' @importFrom codetools findGlobals
#' @examples
#'
#' # Partial function evaluation - a function that returns the first three
#' # elements of an object.
#' head3 <- fapply(head, n=3)
#'
#' # Function composition - a function that returns the fifth through the
#' # 10th element of an object using the head and tail functions.
#' head_1_to_10 <- fapply(head, n=10)
#' head_5_to_10 <- fapply(tail, x=head_1_to_10(x))
#' head_5_to_10(iris)
#' @export
fapply <- function(func, ..., .dots = NULL) {
  f <- function() {}
  args <- as.list(match.call())
  fun_args <- names(formals(func))
  spec_args <- c()
  which_args <- 3:length(args)

  # pass additional arguments ... to list
  body_str <- ""

  if (".dots" %in% names(args)) {
    body_str <- paste(body_str,
                      paste0(".dots <- ",
                             as.character(as.expression(args$.dots)),
                             ";"),
                      sep=";")
    which_args <- setdiff(which_args, which(names(args) == ".dots"))
  }
  fun_arg_list <- c()

  if (length(args) > 2)
    for(i in which_args)  {
      argVal <- args[[i]]
      argName <- names(args)[i]
      argChar <- as.character(as.expression(paste(deparse(argVal),
                                                  collapse="")))
      if (is_anon_function(argVal)) { # if anon function
        body_str <- paste0(body_str,
                          paste0(".", argName, " <- ",
                                 argChar),";")
        tmpfunvars <- extractAnonFunVars(argVal)
        fun_arg_list <- c(fun_arg_list, paste0(argName, " = .",
                                               argName,
                                               "(",
paste(tmpfunvars, collapse=","),")"))

      } else {
        fun_arg_list <- c(fun_arg_list, paste(argName, " = ",
                                              argChar))
        tmpfun <- function(){}
        body(tmpfun) <- parse(text=
                                paste0("{", argChar, "}"))
        tmpfunvars <- findGlobals(tmpfun, merge=FALSE)$variables
      }
      if (argName %in% tmpfunvars) {
        spec_args <- c(spec_args, argName)
      }
    }


  def_args <- paste0(names(args)[which_args], "=",
    names(args)[which_args], collapse=", ")
  other_args <- setdiff(names(formals(func)), names(args))

  if (length(other_args) > 0) {
    # add other args
    other_arg_vec <- unlist(vapply(other_args, function(a) {
      if (a != "...")
        paste0(a, "=", a)
      else as.character(NA)
    }, ""))
    other_arg_vec <- other_arg_vec[!is.na(other_arg_vec)]
    fun_arg_list <- c(other_arg_vec, fun_arg_list)
  }
  body_str <- paste0("{",
                     body_str)
  if (is.primitive(func)) {
    formals(f) <- formals(args(func))[union(spec_args, other_args)]
  } else {
    formals(f) <- formals(func)[union(spec_args, other_args)]
  }
  # run function
  if(is_anon_function(args$func)) {
    fun_line <- paste0(".main_func <- ", as.character(as.expression(args$func)), ";",
                       ".main_func(", paste(fun_arg_list,
                                                                                                           collapse=","), ")")
  } else if (is_fapply_function(args$func)) { # first arg fapply
    body_str <- paste0(body_str,
                       paste0(".firstfun <- ",
                              as.character(as.expression(args$func))),";")
    fun_line <- paste0(".firstfun(", paste(fun_arg_list,
                                            collapse=","), ")")
  } else
    fun_line <- paste0(args$func,"(", paste(fun_arg_list,
                                          collapse=","), ")")
  body_str <- paste0(body_str, fun_line, "}")
  body(f) <- parse(text=body_str)
  environment(f) <- parent.frame()
  f
}

is_fapply_function <- function(expr) {
  substr(as.character(as.expression(expr)),
          1, 7) == "fapply("
}
is_anon_function <- function(expr) {
  substr(as.character(as.expression(expr)),
         1, 9) == "function("
}

extractAnonFunVars <- function(expr) {
  argsString <- gsub("function\\((.*)\\).*", "\\1", as.character(as.expression(expr)))
  argsString <- trimws(unlist(strsplit(argsString, ",")))

  # convert arguments to list
  argsList <- regmatches(argsString, regexpr("=", argsString), invert = TRUE)
  argNames <- sapply(argsList, function(x) x[1])
  return(argNames)
}

