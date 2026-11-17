model <- campsismod::model_suite$pk$`1cpt_fo`
dataset <- campsis::Dataset(3) %>%
  campsismod::add(campsis::Bolus(time = 0, amount = 1000)) %>%
  campsismod::add(campsis::Observations(times = seq(0, 24, by = 4)))

# Reference predictions are simulated with rxode2 (they play the role of NONMEM predictions)
simulate_reference <- function() {
  return(campsis::simulate(model, dataset = dataset, dest = "rxode2", seed = 1, outvars = "CONC"))
}

perturb <- function(ipred, id, factor = 1.1) {
  return(ipred %>% dplyr::mutate(CONC = ifelse(ID == id & TIME > 0, CONC * factor, CONC)))
}

test_that("are_equal compares vectors given the relative tolerance", {
  expect_equal(are_equal(c(1, 0, 100), c(1.005, 0, 100), tolerance = 0.01, id = 1, type = "OUTPUT"), c(T, T, T))
  expect_equal(are_equal(c(1, 0, 2), c(1.02, 1e-3, 2), tolerance = 0.01, id = 1, type = "OUTPUT"), c(F, F, T))
  expect_error(are_equal(c(1, 2), 1, tolerance = 0.01, id = 5, type = "TIME"), "same length \\(ID=5\\)")
})

test_that("check_dest only accepts rxode2 or mrgsolve", {
  expect_no_error(check_dest("rxode2"))
  expect_no_error(check_dest("mrgsolve"))
  expect_error(check_dest("NONMEM"), "Dest must be rxode2 or mrgsolve")
  expect_error(check_dest(c("rxode2", "mrgsolve")), "Dest must be rxode2 or mrgsolve")
})

test_that("add_simulation_id_column adds consecutive IDs and keeps the original ones", {
  data <- data.frame(ID = c(20, 20, 5), TIME = c(0, 1, 0))
  expect_equal(
    add_simulation_id_column(data),
    data.frame(ID = c(1L, 2L, 2L), ORIGINAL_ID = c(5, 20, 20), TIME = c(0, 0, 1))
  )

  # Custom identifier column, existing ID column is dropped
  data <- data.frame(ID = c(1, 2, 3), SUBJ = c("B", "A", "B"), TIME = c(0, 0, 1))
  expect_equal(
    add_simulation_id_column(data, id = "SUBJ"),
    data.frame(ID = c(1L, 2L, 2L), ORIGINAL_ID = c("A", "B", "B"), TIME = c(0, 0, 1))
  )
})

test_that("append_original_id appends ORIGINAL_ID when available in dataset", {
  x <- tibble::tibble(ID = c(1, 2), TIME = 0, CONC = 1)
  dataset <- data.frame(ID = c(1, 1, 2), ORIGINAL_ID = c(10, 10, 20))
  expect_equal(
    append_original_id(x, dataset),
    tibble::tibble(ID = c(1, 2), ORIGINAL_ID = c(10, 20), TIME = 0, CONC = 1)
  )
  expect_equal(append_original_id(x, data.frame(ID = c(1, 2))), x)
  expect_equal(append_original_id(x, dataset = campsis::Dataset(2)), x)
})

test_that("fix_rxode_bug removes time 0 with rxode2 when model has a lag time and no observation at 0", {
  lag_model <- model %>% campsismod::add(campsismod::LagTime(compartment = 1, rhs = "1"))
  dataset_no_zero <- campsis::Dataset(1) %>%
    campsismod::add(campsis::Bolus(time = 0, amount = 1000)) %>%
    campsismod::add(campsis::Observations(times = c(1, 2)))
  output <- tibble::tibble(ID = 1, TIME = c(0, 1, 2))

  expect_equal(fix_rxode_bug(output, lag_model, dataset_no_zero, dest = "rxode2")$TIME, c(1, 2))
  expect_equal(fix_rxode_bug(output, lag_model, dataset_no_zero, dest = "mrgsolve"), output)
  expect_equal(fix_rxode_bug(output, model, dataset_no_zero, dest = "rxode2"), output)
  expect_equal(fix_rxode_bug(output, lag_model, dataset, dest = "rxode2"), output)
  expect_equal(fix_rxode_bug(output, lag_model, as.data.frame(output), dest = "rxode2"), output)
})

test_that("qualify passes when Campsis reproduces the reference predictions", {
  skip_if_no_engines()
  ipred <- simulate_reference()

  for (dest in c("rxode2", "mrgsolve")) {
    expect_output(
      summary <- qualify(model, dataset, ipred = ipred, variables = "CONC", dest = dest, seed = 1),
      "QUALIFICATION SUCCESSFUL"
    )
    expect_s4_class(summary, "qualification_summary")
    expect_true(passed(summary))
    expect_equal(summary@ids, 1:3)
    expect_equal(summary@original_ids, c("1", "2", "3"))
    expect_equal(summary@variables, "CONC")
    expect_equal(summary@dest, dest)
    expect_equal(summary@ipred_source, "NONMEM")
    expect_equal(summary@tolerance, 1e-2)
    expect_equal(summary@summary, data.frame(ID = 1:3, CONC = "PASS"))

    expect_s3_class(get_plot(summary, 1, "CONC"), "ggplot")
    table <- get_table(summary, 2)
    expect_equal(colnames(table), c("ID", "Time NONMEM", paste("Time", dest), "Variable", "Pass", "NONMEM", dest))
    expect_equal(nrow(table), 7)
    expect_true(all(table$Pass == "OK"))
  }
})

test_that("qualify fails when predictions differ", {
  skip_if_no_engines()
  ipred <- simulate_reference() %>% perturb(id = 2)

  expect_output(
    summary <- qualify(model, dataset, ipred = ipred, variables = "CONC", dest = "mrgsolve", seed = 1),
    "QUALIFICATION FAILED"
  )
  expect_false(passed(summary))
  expect_equal(summary@summary$CONC, c("PASS", "FAIL", "PASS"))
  expect_true(all(get_table(summary, 1)$Pass == "OK"))
  expect_equal(get_table(summary, 2)$Pass, c("OK", rep("NOK", 6))) # Time 0 is not perturbed

  # Within a larger tolerance, qualification passes
  expect_output(
    summary <- qualify(model, dataset, ipred = ipred, variables = "CONC", dest = "mrgsolve", seed = 1, tolerance = 0.2),
    "QUALIFICATION SUCCESSFUL"
  )
})

test_that("qualify works with a table (data frame) and keeps the original IDs", {
  skip_if_no_engines()
  ipred <- simulate_reference() %>%
    dplyr::mutate(ID = ID * 100)
  table <- dataset %>%
    campsismod::export(dest = "mrgsolve", model = model, seed = 1) %>%
    dplyr::mutate(ID = ID * 100)

  expect_output(
    summary <- qualify(model, table, ipred = ipred, variables = "CONC", dest = "rxode2", ipred_source = "Monolix"),
    "QUALIFICATION SUCCESSFUL"
  )
  expect_equal(summary@ids, 1:3)
  expect_equal(summary@original_ids, c("100", "200", "300"))
  expect_equal(summary@ipred_source, "Monolix")
  expect_true("Time Monolix" %in% colnames(get_table(summary, 1)))

  # IDs not in the reference are ignored
  expect_output(
    summary <- qualify(model, table, ipred = ipred %>% dplyr::filter(ID != 200), variables = "CONC", dest = "rxode2"),
    "QUALIFICATION SUCCESSFUL"
  )
  expect_equal(summary@original_ids, c("100", "300"))

  expect_error(qualify(model, table, ipred = ipred, variables = "CONC", idref = "abc"), "idref must be")
  expect_error(qualify(model, table, ipred = ipred, variables = "CONC", dest = "NONMEM"), "Dest must be")
})

test_that("compare detects time mismatches and NA's", {
  skip_if_no_engines()
  ipred <- simulate_reference()

  summary <- compare(ipred, ipred, variables = "CONC", tolerance = 1e-2, dest = "rxode2")
  expect_true(passed(summary))
  expect_equal(summary@original_ids, c("", "", ""))

  expect_error(
    compare(ipred %>% dplyr::mutate(TIME = TIME + 1), ipred, variables = "CONC", tolerance = 1e-2),
    "Times are not identical"
  )
  expect_error(
    compare(ipred %>% dplyr::mutate(CONC = NA), ipred, variables = "CONC", tolerance = 1e-2),
    "NA's detected"
  )
  expect_error(compare(ipred, ipred, variables = "CONC", tolerance = 1e-2, dest = "NONMEM"), "Dest must be")
})

test_that("qualification summary can be exported to PDF", {
  skip_on_cran()
  skip_if_no_engines()
  skip_if_no_latex()
  ipred <- simulate_reference() %>% perturb(id = 2)
  expect_output(summary <- qualify(model, dataset, ipred = ipred, variables = "CONC", dest = "rxode2"))

  file <- withr::local_tempfile(fileext = ".pdf")
  suppressMessages(capture.output(
    write(summary, file = file, original_model = "RUN001.ctl", notes = "Some notes")
  ))
  expect_true(file.exists(file))
  expect_gt(file.size(file), 0)
})
