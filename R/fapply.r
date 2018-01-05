#' @title Generalized Function Composition and Partial Function Evaluation
#'
#' @description 'fapply' is used to modify functions. It can be used to
#' compose function with specified function parameters and it can be used
#' to set parameter values (partial function evaluation).
#'
#' @param func the function to be modified.
#' @param ... the function modifiers (see Details).
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
fapply <- function(func, ...) {
  f <- function() {}
  args <- as.list(match.call())
  fun_arg_list_str <- "fun_arg_list <- list();"
  arg_offset <- 2
  fun_args <- names(formals(func))
  spec_args <- c()

  # pass additional arguments ... to list
  body_str <- paste("args <- as.list(match.call());",
                    "if (length(args) > 1) ",
                    "  for(i in 2:length(args)) {",
                    "    if (!(names(args)[i] %in% names(fun_arg_list)))",
                    "      fun_arg_list[[names(args)[i]]] <- args[[i]] }; ")

  for(i in seq_len(length(args) - arg_offset))  {
    # If it's a call, see if it's a function declaration.
    if (inherits(args[[i + arg_offset]], "call")) {
      obj <- try(eval(args[[i + arg_offset]]), silent=TRUE)
      # If we can't evaluate then leave it alone.
      if (!inherits(obj, "try-error")) {
        if (is.function(obj)) {
          args[[i + arg_offset]] <- obj
        } else {
          stop(paste0("Don't know how to deal with object of class ",
                      class(obj), "."))
        }
      }
    }
    if (inherits(args[[i + arg_offset]], "name")) {
      # If it's a name, then see what it is.
      obj <- eval(args[[i + arg_offset]])
      if (!is.function(obj)) {
        # It's not a function. We can set the evaluated object to the 
        # argument.
        args[[i + arg_offset]] <- obj
      } else {
        # It's a function. Assume it's the one we apply to the specified
        # parameter before do.call.
        args[[i + arg_offset]] <- parse(text=
          paste0(as.character(args[[i + arg_offset]]), "(", 
            names(args)[i + arg_offset], ")"))
        
      }
      #args[[i + arg_offset]] <- fapply(eval(args[[i + arg_offset]]))
    }
    if (inherits(args[[i + arg_offset]], "function")) {
      fun_arg_list_str <- paste(fun_arg_list_str,
        paste0("fun_arg_list[['",
               names(args)[i + arg_offset], "']] <- (",
               paste(deparse(args[[i + arg_offset]]), collapse=" "), ")(",
               names(args)[i + arg_offset], ")"))
    } else {
      fun_arg_list_str <- paste(fun_arg_list_str,
        paste0("fun_arg_list[['",
               names(args)[i + arg_offset], "']] <- ",
               as.expression(args[[i + arg_offset]])), sep=";")

    } 
    tmpfun <- function(){}
    try_eval <- try(eval(args[[i + arg_offset]]), silent=TRUE)
    if (class(try_eval) == "function") {
      body(tmpfun) <- body(try_eval)
    } else
      body(tmpfun) <- parse(text= paste0("{",
        as.character(
          as.expression(args[[i + arg_offset]])),
        "}"))
    tmpfunvars <- findGlobals(tmpfun, merge=FALSE)$variables
    if (names(args)[i+arg_offset] %in% tmpfunvars) {
      spec_args <- c(spec_args, names(args)[i+arg_offset])
    }
  }


  def_args <- paste0(names(args)[-(1:arg_offset)], "=",
    names(args)[-(1:arg_offset)], collapse=", ")
  other_args <- setdiff(names(formals(func)), names(args))

  if (length(other_args) > 0) {
    # add other args
    other_arg_vec <- unlist(vapply(other_args, function(a) {
      if (a != "...")
        paste0("fun_arg_list[['",
               a, "']] <- ",
               a)
      else as.character(NA)
    }, ""))
    other_arg_vec <- other_arg_vec[!is.na(other_arg_vec)]
    fun_arg_list_str <- paste(fun_arg_list_str, ";",
                              paste(other_arg_vec,
                              collapse=";"))
  }
  body_str <- paste0("{", fun_arg_list_str, ";",
                     body_str)
  formals(f) <- formals(func)[union(spec_args, other_args)]
  fun_line <- paste0( "do.call(", deparse(args$func),", fun_arg_list)")
  body_str <- paste0(body_str, fun_line, "}")
  body(f) <- parse(text=body_str)
  environment(f) <- parent.frame()
  f
}
