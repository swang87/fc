require(codetools)

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
     # better way of doing this?
     if (substr(as.character(as.expression(args[[i+arg_offset]])),
                1, 9) == "function(") {
       fun_arg_list_str <- paste(fun_arg_list_str,
                                 paste0("fun_arg_list[['",
                                        names(args)[i + arg_offset],
                                        "']] <- do.call(",
                                        as.expression(args[[i + arg_offset]]),
                                        ", list(",
                                        names(formals(eval(args[[i+arg_offset]])))[1],
                                        "=",
                                        names(args)[i + arg_offset],
                                        "))"),
                                 sep=";")
     } else
       fun_arg_list_str <- paste(fun_arg_list_str,
                             paste0("fun_arg_list[['",
                                    names(args)[i + arg_offset], "']] <- ",
                                    as.expression(args[[i + arg_offset]])),
                             sep=";")
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
    other_arg_vec <- unlist(sapply(other_args, function(a) {
      if (a != "...")
        paste0("fun_arg_list[['",
               a, "']] <- ",
               a)
    }))
    fun_arg_list_str <- paste(fun_arg_list_str, ";",
                              paste(other_arg_vec,
                              collapse=";"))
  }
  body_str <- paste0("{", fun_arg_list_str, ";",
                     body_str)
  formals(f) <- formals(func)[union(spec_args, other_args)]
  fun_line <- paste0("do.call(", as.character(as.expression(args$func)),", fun_arg_list)")
  body_str <- paste0(body_str, fun_line, "}")
  body(f) <- parse(text=body_str)
  environment(f) <- parent.frame()
  f
}
