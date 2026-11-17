test_that("clean_package_results sums the expectations of each test", {
  results <- clean_package_results(generate_test_results())
  expect_equal(colnames(results), c("file", "context", "Test", "Passed", "Failed", "Warnings", "Errors", "Skipped"))
  expect_equal(nrow(results), 6)

  row <- function(test) results %>% dplyr::filter(Test == test)
  expect_equal(row("pass")$Passed, 2)
  expect_equal(row("fail")$Failed, 1)
  expect_equal(c(row("warn")$Warnings, row("warn")$Passed), c(1, 1))
  expect_equal(row("skip")$Skipped, 1)
  expect_equal(row("error")$Errors, 1)
  expect_equal(row("pass b")$file, "test-b.R")
})

test_that("summarise_results and summarise_results_by_script aggregate results", {
  results <- clean_package_results(generate_test_results())

  expect_equal(
    as.data.frame(summarise_results(results)),
    data.frame(Passed = 4L, Failed = 1L, Warnings = 1L, Errors = 1, Skipped = 1)
  )

  byScript <- summarise_results_by_script(results)
  expect_equal(
    as.data.frame(byScript),
    data.frame(
      `Test script` = c("test-a.R", "test-b.R"),
      Passed = c(3L, 1L),
      Failed = c(1L, 0L),
      Warnings = c(1L, 0L),
      Errors = c(1, 0),
      Skipped = c(1, 0),
      check.names = FALSE
    )
  )
})

test_that("qualification_passed is FALSE as soon as a test fails or errors", {
  results <- clean_package_results(generate_test_results())
  expect_false(qualification_passed(results))
  expect_false(qualification_passed(results %>% dplyr::filter(Test != "error")))
  expect_false(qualification_passed(results %>% dplyr::filter(Test != "fail")))
  # Warnings and skipped tests do not make the qualification fail
  expect_true(qualification_passed(results %>% dplyr::filter(!Test %in% c("fail", "error"))))
})

test_that("collect_package_warnings counts the warnings", {
  warnings <- collect_package_warnings(generate_test_results())
  expect_equal(warnings, tibble::tibble(warning = "my warning", occurrences = 1L))
  expect_error(collect_package_warnings(list()), "x must be of type 'testthat_results'")
})

test_that("collect_package_failures lists the failed expectations and errors", {
  failures <- collect_package_failures(generate_test_results())
  expect_equal(colnames(failures), c("file", "test", "type", "message"))
  expect_equal(failures$file, c("test-a.R", "test-a.R"))
  expect_equal(failures$test, c("fail", "error"))
  expect_equal(failures$type, c("Failure", "Error"))
  expect_match(failures$message[1], "FALSE")
  expect_match(failures$message[2], "boom")

  empty <- collect_package_failures(structure(list(), class = "testthat_results"))
  expect_equal(nrow(empty), 0)
  expect_equal(colnames(empty), c("file", "test", "type", "message"))

  expect_error(collect_package_failures(list()), "x must be of type 'testthat_results'")
})

test_that("bind_package_and_version adds the package name and version", {
  results <- clean_package_results(generate_test_results())
  summarised <- bind_package_and_version(results, "campsismod")
  expect_equal(colnames(summarised), c("Package", "Version", "Passed", "Failed", "Warnings", "Errors", "Skipped"))
  expect_equal(summarised$Package, "campsismod")
  expect_equal(summarised$Version, as.character(packageVersion("campsismod")))
})

test_that("print_summary_kable prints a LaTeX table", {
  results <- generate_qualification_results()
  expect_output(print_summary_kable(results$summarised), "campsismod")
  expect_output(print_summary_kable(results$summarised), "tabu")
})

test_that("get_initials deduces the initials from the full name", {
  expect_equal(get_initials("John Doe"), "JD")
  expect_equal(get_initials("  Jean Pierre   Dupont "), "JPD")
  expect_equal(get_initials("Plato"), "P")
})

test_that("get_report_filename follows the naming convention", {
  date <- as.Date("2026-11-17")
  expect_equal(
    get_report_filename(c("campsismod", "campsis"), initials = "JD", date = date),
    sprintf("IQ_OQ_Campsis_Suite-%s_%s_JD_20261117", packageVersion("campsis"), get_os_name(short = TRUE))
  )
})

test_that("run_qualification checks its arguments", {
  expect_error(run_qualification(packages = c("campsis", "dplyr"), fullname = "John Doe"), "Invalid packages")
  expect_error(run_qualification_core(packages = character(0)), "No packages to qualify")
  expect_error(run_qualification_core(packages = "notapackage"), "notapackage")
})

test_that("render_report produces the IQ/OQ report", {
  skip_on_cran()
  skip_if_no_latex()
  outputDir <- withr::local_tempdir()
  results <- generate_qualification_results("campsismod")

  report <- render_report(results, packages = "campsismod", fullname = "John Doe", output_dir = outputDir)
  expect_true(file.exists(report))
  expect_equal(basename(report), paste0(get_report_filename("campsismod", initials = "JD"), ".pdf"))
  expect_equal(list.files(outputDir), basename(report)) # Intermediate files are not kept

  # Initials can be provided
  report <- render_report(results, packages = "campsismod", fullname = "John Doe", initials = "JOD", output_dir = outputDir)
  expect_match(basename(report), "_JOD_")

  # Results without failures and warnings (e.g. produced by an older version of campsisqual)
  results$failures <- NULL
  results$warnings <- NULL
  unlink(report)
  report <- render_report(results, packages = "campsismod", fullname = "John Doe", output_dir = outputDir)
  expect_true(file.exists(report))
})

test_that("render_report warns if the report cannot be rendered", {
  skip_if_no_latex()
  outputDir <- withr::local_tempdir()
  expect_warning(
    report <- render_report(list(), packages = "campsismod", fullname = "John Doe", output_dir = outputDir),
    "Qualification report could not be rendered"
  )
  expect_null(report)
})

test_that("run_qualification qualifies campsismod and produces the IQ/OQ report", {
  skip_on_cran()
  skip_if_running_qualification()
  skip_if_no_engines()
  skip_if_no_latex()
  skip_if_not(
    dir.exists(file.path(find.package("campsismod"), "tests", "testthat")),
    "campsismod was installed without its tests"
  )
  outputDir <- withr::local_tempdir()
  optionsBefore <- options()[c("campsis.options", "campsisqual.options")]

  output <- capture.output(
    qualOK <- run_qualification(packages = "campsismod", fullname = "John Doe", output_dir = outputDir, cpu = 1L)
  )
  expect_type(qualOK, "logical")
  expect_length(qualOK, 1)

  # Report is produced in the output directory
  files <- list.files(outputDir)
  expect_equal(files, paste0(get_report_filename("campsismod", initials = "JD"), ".pdf"))
  expect_gt(file.size(file.path(outputDir, files)), 0)

  # Options set for the qualification are restored
  expect_equal(options()[c("campsis.options", "campsisqual.options")], optionsBefore)
})
