test_that("risk_metric_result converts a metric into Yes/No", {
  expect_equal(risk_metric_result(TRUE), "Yes")
  expect_equal(risk_metric_result(c(TRUE, FALSE)), "No")
  expect_equal(risk_metric_result(logical(0)), "No")
  expect_equal(risk_metric_result("abc"), "Yes")
  expect_equal(risk_metric_result(character(0)), "No")
  expect_equal(risk_metric_result(2L), "Yes")
  expect_equal(risk_metric_result(0L), "No")
  expect_output(expect_equal(risk_metric_result(1.5), "No"), "Issue with class numeric")
})

test_that("risk_metric_to_string collapses character metrics", {
  expect_equal(risk_metric_to_string(c("a", "b")), "a, b")
  expect_equal(risk_metric_to_string(TRUE), "")
})

test_that("get_package_branch returns the main branch of the package", {
  expect_equal(get_package_branch("campsis"), "main")
  expect_equal(get_package_branch("campsisqual"), "master")
})

test_that("summarise_riskmetric is not implemented for unknown metrics", {
  expect_error(summarise_riskmetric(structure(1, class = "unknown_metric"), "campsis"), "No summarise method")
})

test_that("summarise_riskmetric overrides some metrics for Campsis packages", {
  metric <- structure(FALSE, class = c("logical", "pkg_metric_has_vignettes"))
  expect_equal(summarise_riskmetric(metric, "campsis")$Result, "Yes")
  expect_equal(summarise_riskmetric(metric, "campsisnca")$Result, "No")

  metric <- structure(FALSE, class = c("logical", "pkg_metric_online_code_coverage"))
  expect_equal(summarise_riskmetric(metric, "campsismod")$`More info`, "https://app.codecov.io/gh/Calvagone/campsismod")
  expect_equal(summarise_riskmetric(metric, "campsisqual")$Result, "No")

  metric <- structure(TRUE, class = c("logical", "pkg_metric_has_news"))
  expect_equal(
    summarise_riskmetric(metric, "campsisqual")$`More info`,
    "https://github.com/Calvagone/campsisqual/blob/master/NEWS.md"
  )
})

test_that("assess_package returns a human-readable table with the risk metrics", {
  assessment <- assess_package("campsismod", successful_tests = TRUE)
  expect_s3_class(assessment, "tbl_df")
  expect_equal(colnames(assessment), c("Criteria", "Result", "More info", "Category"))
  expect_equal(nrow(assessment), 10)
  expect_equal(sort(unique(assessment$Category)), c("Documentation", "Maintenance", "Testing", "Transparency"))
  expect_false(is.unsorted(assessment$Category))
  expect_true(all(assessment$Result %in% c("Yes", "No")))
  expect_equal(assessment$Result[assessment$Criteria == "Were tests successful?"], "Yes")

  assessment <- assess_package("campsismod", successful_tests = FALSE)
  expect_equal(assessment$Result[assessment$Criteria == "Were tests successful?"], "No")

  expect_error(assess_package("notapackage", successful_tests = TRUE))
})
