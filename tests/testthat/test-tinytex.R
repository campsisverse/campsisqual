test_that("check_tinytex_installation compiles a minimal LaTeX document", {
  skip_on_cran()
  skip_if_no_latex()
  expect_true(check_tinytex_installation())
})
