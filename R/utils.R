#' With directory
#'
#' Evaluate an expression with a temporary working directory.
#'
#' @param dir directory
#' @param expr expression
#' @return the value of the evaluated expression
#' @export
with_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir)
  force(expr)
}

#'
#' Escape LaTeX special characters.
#'
#' @param x character vector
#' @return escaped character vector
escape_latex <- function(x) {
  # Backslashes are protected first, as the other replacements introduce some
  x <- gsub("\\", "\001", x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  return(x)
}

#'
#' Format a test message (failure, error or warning) for LaTeX.
#' ANSI styles are removed, white spaces (including new lines) are collapsed,
#' long messages are truncated and LaTeX special characters are escaped.
#'
#' @param x character vector
#' @param max_chars maximum number of characters, before escaping
#' @return LaTeX-ready character vector
format_message <- function(x, max_chars = 300) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  too_long <- nchar(x) > max_chars
  x[too_long] <- paste0(substr(x[too_long], 1, max_chars), "...")
  return(escape_latex(x))
}

#'
#' Format the 'More info' column of the risk metrics table for LaTeX.
#' URLs are wrapped into \\url\{\} (so that they can be broken over several lines),
#' other values are escaped.
#'
#' @param x character vector, multiple values separated by a comma
#' @return LaTeX-ready character vector
format_more_info <- function(x) {
  return(vapply(
    x,
    FUN = function(info) {
      if (is.na(info) || info == "") {
        return("")
      }
      parts <- strsplit(info, ", ", fixed = TRUE)[[1]]
      parts <- ifelse(grepl("^https?://", parts), sprintf("\\url{%s}", parts), escape_latex(parts))
      return(paste0(parts, collapse = ", "))
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  ))
}
