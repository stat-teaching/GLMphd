#' Format code into rmd/qmd documents
#'
#' @param x text to format
#' @param format type of formatting
#'
#' @returns the formatted string
#' @export
#'
code <- function(x, format = c("html", "markdown", "latex")) {
  format <- match.arg(format)
  if (format == "html") {
    sprintf("<code>%s</code>", x)
  } else if (format == "markdown") {
    sprintf("`%s`", x)
  } else {
    sprintf("\\texttt{%s}", x)
  }
}

#' Wrapper for the latex2exp::TeX function
#' @description
#' Wrapper for the latex2exp::TeX function including also a `sprintf` functionality
#'
#' @param text the string to be passed into latex2exp::TeX
#' @param ... other objects passed to `sprintf(fmt, ...)`
#'
#' @returns the formatted expression
#' @export
#'
latex <- function(text, ...) {
  latex2exp::TeX(sprintf(text, ...))
}


#' Show code and results in rmd/qmd documents
#' @description
#' Show code and results in rmd/qmd documents. The expression is evaluated in the parent frame.
#'
#' @param expr code
#' @param between string to include between code and result
#' @param digits number of digits for numbers
#'
#' @returns the string with code and result
#' @export
#'
dcode <- function(expr, between = NULL, digits = NULL) {
  if (is.null(digits)) {
    digits <- options()$digits
  }

  code <- deparse(substitute(expr))
  result <- eval(expr, envir = parent.env(environment()))

  if (is.numeric(result)) {
    result <- round(result, digits)
  }

  if (is.null(between)) {
    sprintf("`%s` %s", code, result)
  } else {
    sprintf("`%s` %s %s", code, between, result)
  }
}

#' @keywords internal
.paste_arg_value <- function(x){
  x <- data.frame(x)
  rr <- vector(mode = "list", length = ncol(x))
  for(i in 1:ncol(x)){
    rr[[i]] <- sprintf("%s = %s", names(x)[i], x[[i]])
  }
  rr <- data.frame(rr)
  apply(rr, 1, function(x) paste(x, collapse = ", "))
}
