#' Check TinyTeX installation
#'
#' @return TRUE if TinyTeX is installed correctly, FALSE otherwise
#' @importFrom tinytex pdflatex
#' @export
check_tinytex_installation <- function() {
  template <- "
\\documentclass{article}
\\begin{document}
Well done. Your installation works well.
\\end{document}"

  tmp_dir <- tempdir()
  output_file <- "check_tinytex.pdf"
  tex_file <- "check_tinytex.tex"

  out <- tryCatch(
    {
      with_dir(tmp_dir, {
        file_conn <- file(tex_file)
        writeLines(template, file_conn)
        close(file_conn)
        tinytex::pdflatex(tex_file, pdf_file = output_file)
      })
    },
    error = function(e) {
      print(e)
    }
  )

  if (is(out, "error")) {
    return(FALSE)
  } else if (is.character(out)) {
    return(out == output_file)
  } else {
    print("Unknown error")
    return(FALSE)
  }
}
