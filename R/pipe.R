#' @title Forward-pipe Operator Using Standard Evaluation
#'
#' @description The forward pipe operator behaves similar to the magrittr
#' pipe operator with two exceptions. First, it only supports standard
#' evaluation. If modified parameter values are needed
#' then the fapply function should be used. Second, it composes functions.
#' The return type of this operator is an R function.
#' @param lhs the function that will be applied second to an input.
#' @param rhs the function that will be applied first to an input.
#' @return The composed function lhs(rhs(x)).
#' @rdname pipe
#' @examples
#' # Create a new code block in case the pipe operator is already
#' # defined.
#' {
#'   # Make sure the example uses the correct pipe operator.
#'   `%>%` <- fapply::`%>%`
#'
#'  # Create a function that gets the 9th and 10th objects using the head
#'  # and tail functions.
#'  nine_and_ten <- fapply(head, n=10) %>% fapply(tail, n=2)
#'  nine_and_ten(iris)
#'}
#' @export
`%>%` <- function(lhs, rhs) {
  args <- as.list(match.call())
  lhsChar <- as.character(as.expression(args$lhs))
  rhsChar <- as.character(as.expression(args$rhs))
  rhsChar <- gsub("(.*\\(.*\\))\\(.*\\)", "\\1", rhsChar)
  rhs_first_arg_name <- names(formals(eval(parse(text=rhsChar))))[1]
  fun_line <- paste0("fapply(", rhsChar, ",", rhs_first_arg_name, "= (",
                    lhsChar, ")(", rhs_first_arg_name, "))")
  eval(parse(text=fun_line))
}
