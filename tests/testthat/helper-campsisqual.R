skip_if_no_latex <- function() {
  testthat::skip_if_not(rmarkdown::pandoc_available(), "Pandoc is not available")
  testthat::skip_if_not(tinytex::is_tinytex() || nzchar(Sys.which("pdflatex")), "LaTeX is not available")
}

skip_if_no_engines <- function() {
  testthat::skip_if_not_installed("mrgsolve")
  testthat::skip_if_not_installed("rxode2")
}

# campsisqual can qualify itself: avoid running a qualification within a qualification
skip_if_running_qualification <- function() {
  option <- getOption("campsisqual.options")
  testthat::skip_if(isTRUE(option$RUNNING_QUALIFICATION), "Qualification of the Campsis suite is running")
}

#' Run a small fake test suite, with expectations that pass, fail, warn,
#' error and are skipped.
#'
#' @return a 'testthat_results' object
generate_test_results <- function() {
  dir <- tempfile(pattern = "fake_suite_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines(
    c(
      'test_that("pass", { expect_true(TRUE); expect_equal(1, 1) })',
      'test_that("fail", { expect_true(FALSE) })',
      'test_that("warn", { warning("my warning"); expect_true(TRUE) })',
      'test_that("skip", { skip("skipped") })',
      'test_that("error", { stop("boom") })'
    ),
    file.path(dir, "test-a.R")
  )
  writeLines('test_that("pass b", { expect_true(TRUE) })', file.path(dir, "test-b.R"))
  return(testthat::test_dir(
    dir,
    reporter = "list",
    stop_on_failure = FALSE,
    stop_on_warning = FALSE,
    load_package = "none"
  ))
}

#' Fake results, as returned by 'run_qualification_core', based on the fake test suite.
#'
#' @param package package name the results are attributed to (must be installed)
#' @return a list, see ?run_qualification_core
generate_qualification_results <- function(package = "campsismod") {
  raw <- generate_test_results()
  cleaned <- clean_package_results(raw)
  return(list(
    summarised = bind_package_and_version(cleaned, package),
    all = stats::setNames(list(cleaned), package),
    warnings = stats::setNames(list(collect_package_warnings(raw)), package),
    failures = stats::setNames(list(collect_package_failures(raw)), package)
  ))
}
