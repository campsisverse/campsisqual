test_that("with_dir evaluates the expression in the given directory and restores the working directory", {
  dir <- withr::local_tempdir()
  wd <- getwd()
  expect_equal(normalizePath(with_dir(dir, getwd())), normalizePath(dir))
  expect_equal(getwd(), wd)

  expect_error(with_dir(dir, stop("boom")), "boom")
  expect_equal(getwd(), wd)
})

test_that("get_os_name returns the OS name", {
  os <- get_os_name()
  expect_type(os, "character")
  expect_length(os, 1)

  short <- get_os_name(short = TRUE)
  expect_false(grepl(" ", short))
  if (Sys.info()[["sysname"]] == "Windows") {
    expect_true(startsWith(os, "Windows"))
    expect_true(startsWith(short, "Win"))
  }
})

test_that("escape_latex escapes LaTeX special characters", {
  expect_equal(escape_latex("a_b & 10% $ #1 {x}"), "a\\_b \\& 10\\% \\$ \\#1 \\{x\\}")
  expect_equal(escape_latex("a~b^c"), "a\\textasciitilde{}b\\textasciicircum{}c")
  expect_equal(escape_latex("a\\b"), "a\\textbackslash{}b")
  expect_equal(escape_latex(c("x", "y_z")), c("x", "y\\_z"))
})

test_that("format_message cleans, truncates and escapes test messages", {
  expect_equal(format_message("\033[31mred\033[39m  text\n on\nlines "), "red text on lines")
  expect_equal(format_message("a_b"), "a\\_b")
  expect_equal(format_message(strrep("x", 10), max_chars = 4), "xxxx...")
  expect_equal(format_message(c("ok", strrep("y", 5)), max_chars = 4), c("ok", "yyyy..."))
  expect_equal(format_message(character(0)), character(0))
})

test_that("format_more_info wraps URLs and escapes the rest", {
  x <- c(
    "",
    NA,
    "https://github.com/campsisverse/campsismod",
    "https://a.com, https://b.com/x_y",
    "John_Doe <john@doe.com>"
  )
  expect_equal(
    format_more_info(x),
    c(
      "",
      "",
      "\\url{https://github.com/campsisverse/campsismod}",
      "\\url{https://a.com}, \\url{https://b.com/x_y}",
      "John\\_Doe <john@doe.com>"
    )
  )
})
