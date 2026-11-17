test_that("deprecated methods still work but raise a deprecation warning", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(os <- getOSName(short = TRUE), class = "lifecycle_warning_deprecated")
  expect_equal(os, get_os_name(short = TRUE))

  expect_warning(x <- areEqual(1, 1.001, tolerance = 0.01, id = 1, type = "OUTPUT"), class = "lifecycle_warning_deprecated")
  expect_true(x)

  expect_warning(
    expect_error(runQualification(packages = "dplyr", fullname = "John Doe"), "Invalid packages"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("deprecated getters on qualification summary still work", {
  withr::local_options(lifecycle_verbosity = "warning")
  summary <- new("qualification_summary", plots = list("1" = list(CONC = "plot")), tables = list("1" = "table"))

  expect_warning(plot <- getPlot(summary, 1, "CONC"), class = "lifecycle_warning_deprecated")
  expect_equal(plot, "plot")
  expect_warning(table <- getTable(summary, 1), class = "lifecycle_warning_deprecated")
  expect_equal(table, "table")
})

test_that("deprecated checkTinyTEXInstallation still works", {
  skip_on_cran()
  skip_if_no_latex()
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(ok <- checkTinyTEXInstallation(), class = "lifecycle_warning_deprecated")
  expect_true(ok)
})
