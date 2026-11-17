test_that("read.nonmem reads all tables from a NONMEM output file", {
  tables <- read.nonmem(test_path("resources", "output.tab"))
  expect_length(tables, 2)

  expect_equal(colnames(tables[[1]]), c("ID", "TIME", "CP", "EVID"))
  expect_equal(tables[[1]]$ID, c(1, 1, 2, 2))
  expect_equal(tables[[1]]$CP, c(0, 12.345, 0, 25))
  expect_equal(tables[[1]]$EVID, c(1, 0, 1, 0))

  expect_equal(nrow(tables[[2]]), 1)
  expect_equal(tables[[2]]$CP, 1e-3)
})

test_that("read.nonmem raises an error if no table is found", {
  file <- withr::local_tempfile(fileext = ".tab")
  writeLines(c(" ID TIME CP", " 1 0 0"), file)
  expect_error(read.nonmem(file), "No NONMEM table was recognised")
})

test_that("read.nonmemtable reads a single table", {
  table <- read.nonmemtable(c("TABLE NO.     1", " ID   TIME", "  1.0E+00  2.0E+00", "  3.0E+00  4.0E+00"))
  expect_equal(table, tibble::tibble(ID = c(1, 3), TIME = c(2, 4)))
})
