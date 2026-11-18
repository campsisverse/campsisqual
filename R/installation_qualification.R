#'
#' Clean package results.
#'
#' @param raw raw results
#' @return cleaned results
#' @importFrom dplyr as_tibble group_by rename summarise
clean_package_results <- function(raw) {
  ret_value <- raw %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Test = "test") %>%
    dplyr::group_by(.data$file, .data$context, .data$Test) %>%
    dplyr::summarise(
      Passed = sum(.data$passed),
      Failed = sum(.data$failed),
      Warnings = sum(.data$warning),
      Errors = sum(as.numeric(.data$error)),
      Skipped = sum(as.numeric(.data$skipped)),
      .groups = "drop"
    )
  return(ret_value)
}

#'
#' Summarise results.
#'
#' @param results results
#' @return summarised results
#' @importFrom dplyr summarise_if
summarise_results <- function(results) {
  return(results %>% dplyr::summarise_if(is.numeric, sum))
}

#'
#' Summarise results by test script.
#'
#' @param results cleaned package results, see `clean_package_results()`
#' @return a tibble with one row per test script and the following columns:
#'  'Test script', 'Passed', 'Failed', 'Warnings', 'Errors' and 'Skipped'
#' @importFrom dplyr across group_by rename summarise
summarise_results_by_script <- function(results) {
  return(
    results %>%
      dplyr::group_by(.data$file) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(c("Passed", "Failed", "Warnings", "Errors", "Skipped")), sum),
        .groups = "drop"
      ) %>%
      dplyr::rename(`Test script` = "file")
  )
}

#'
#' Print summary kable (LaTeX).
#'
#' @param x data frame
#' @return printed kable
#' @importFrom kableExtra kbl kable_styling column_spec
print_summary_kable <- function(x) {
  return(
    x %>%
      kableExtra::kbl(format = "latex", booktabs = T, linesep = "") %>%
      kableExtra::kable_styling(full_width = T, latex_options = c("striped", "hold_position")) %>%
      kableExtra::column_spec(1, width = "2.6cm") %>%
      kableExtra::column_spec(2, width = "1.5cm") %>%
      print()
  )
}

#'
#' Passed method.
#'
#' @param results results
#' @return TRUE or FALSE
#' @importFrom dplyr summarise_if
qualification_passed <- function(results) {
  row <- results %>% dplyr::summarise_if(is.numeric, sum)
  ret_value <- TRUE
  if (row$Failed > 0) {
    ret_value <- FALSE
  }
  # Too strict, so disabled
  # if (row$Warnings > 0) {
  #   ret_value <- FALSE
  # }
  if (row$Errors > 0) {
    ret_value <- FALSE
  }
  return(ret_value)
}

bind_package_and_version <- function(x, package) {
  return(cbind(
    "Package" = package,
    "Version" = getNamespaceVersion(package) %>% as.character(),
    x %>% summarise_results()
  ))
}

#' Get OS name.
#'
#' @param short if TRUE, returns a short version of the OS name (e.g., "Win11" for Windows 11)
#' @return the OS name
#' @export
get_os_name <- function(short = FALSE) {
  # 1. Get basic info
  sys <- Sys.info()
  sysname <- sys[["sysname"]]
  release <- sys[["release"]]

  # 2. Refined Windows detection
  if (sysname == "Windows") {
    # Extract build number
    build <- as.numeric(gsub(".*?([0-9]+).*", "\\1", sys[["version"]]))

    if (!is.na(build) && build >= 22000) {
      # For Win 11, we manually set the name and ignore the "10" in release
      os <- paste("Windows 11", sys[["machine"]])
    } else {
      # For Win 10 and older, use the standard release string
      os <- paste("Windows", release, sys[["machine"]])
    }
  } else {
    # Fallback for macOS/Linux
    os <- paste(sysname, release)
  }

  # 3. Handle the "short" formatting
  if (short) {
    os <- gsub("Windows", "Win", x = os)
    os <- gsub(" ", "", x = os)
  }

  return(os)
}

#'
#' Collect package warnings
#'
#' @param x package test results
#' @return a tibble with 2 columns: 'warning', 'occurrences'
#' @importFrom dplyr group_by summarise
#' @importFrom tibble tibble
#' @importFrom purrr map_chr discard
collect_package_warnings <- function(x) {
  assertthat::assert_that(is(x, "testthat_results"), msg = "x must be of type 'testthat_results'")
  warningMessages <- character(0)
  for (i in seq_along(x)) {
    testResult <- x[[i]]
    warnings <- testResult$results %>%
      purrr::map_chr(.f = function(y) {
        if (is(y, "expectation_warning")) {
          return(y$message)
        } else {
          return(as.character(NA))
        }
      }) %>%
      purrr::discard(~ is.na(.x))
    warningMessages <- warningMessages %>%
      append(warnings)
  }

  ret_value <- tibble::tibble(warning = warningMessages) %>%
    dplyr::group_by(.data$warning) %>%
    dplyr::summarise(occurrences = dplyr::n())

  return(ret_value)
}

#'
#' Collect package failures (failed expectations and errors).
#'
#' @param x package test results
#' @return a tibble with 4 columns: 'file', 'test', 'type' ('Failure' or 'Error') and 'message'
#' @importFrom tibble tibble
#' @importFrom purrr keep list_rbind map map_chr
collect_package_failures <- function(x) {
  assertthat::assert_that(is(x, "testthat_results"), msg = "x must be of type 'testthat_results'")
  empty <- tibble::tibble(file = character(0), test = character(0), type = character(0), message = character(0))

  failures <- x %>%
    purrr::map(.f = function(testResult) {
      expectations <- testResult$results %>%
        purrr::keep(~ is(.x, "expectation_failure") || is(.x, "expectation_error"))
      return(tibble::tibble(
        file = rep(testResult$file, length(expectations)),
        test = rep(testResult$test, length(expectations)),
        type = expectations %>% purrr::map_chr(~ ifelse(is(.x, "expectation_error"), "Error", "Failure")),
        message = expectations %>% purrr::map_chr(~ as.character(.x$message))
      ))
    })

  return(purrr::list_rbind(c(list(empty), failures)))
}

#'
#' Collect skipped tests.
#'
#' @param x package test results
#' @return a tibble with 3 columns: 'file', 'test' and 'reason'
#' @importFrom tibble tibble
#' @importFrom purrr keep list_rbind map map_chr
collect_package_skipped <- function(x) {
  assertthat::assert_that(is(x, "testthat_results"), msg = "x must be of type 'testthat_results'")
  empty <- tibble::tibble(file = character(0), test = character(0), reason = character(0))

  skipped <- x %>%
    purrr::map(.f = function(testResult) {
      expectations <- testResult$results %>%
        purrr::keep(~ is(.x, "expectation_skip"))
      return(tibble::tibble(
        file = rep(testResult$file, length(expectations)),
        test = rep(testResult$test, length(expectations)),
        reason = expectations %>% purrr::map_chr(~ sub("^Reason: ", "", as.character(.x$message)))
      ))
    })

  return(purrr::list_rbind(c(list(empty), skipped)))
}

#'
#' Run qualification.
#'
#' Run the tests of the given packages from the Campsis suite and produce an
#' IQ/OQ report in PDF (rendered with LaTeX, see `render_report()`).
#'
#' @param packages packages to qualify
#' @param fullname fullname of the person running the qualification
#' @param initials username initials to be used in the filename. If not provided, initials are deduced from the fullname.
#' @param output_dir output directory where the qualification report is written
#' @param cpu number of workers to be used, default is 6 workers
#' @param skip_vdiffr skip Vdiffr tests, default is TRUE
#' @param skip_python skip Python tests (e.g. Pharmpy in campsistrans), default is TRUE
#' @return TRUE if the qualification passes, FALSE otherwise
#' @importFrom tictoc tic toc
#' @export
run_qualification <- function(
  packages,
  fullname,
  initials = NULL,
  output_dir = getwd(),
  cpu = 6L,
  skip_vdiffr = TRUE,
  skip_python = TRUE
) {
  if (
    !all(
      packages %in% c("campsismod", "campsis", "campsisnca", "campsismisc", "campsisqual", "campsistrans")
    )
  ) {
    stop("Invalid packages. Only packages from the Campsis suite can be qualified.")
  }

  if (length(find.package("mrgsolve", quiet = TRUE)) == 0) {
    stop("mrgsolve not installed")
  }
  if (length(find.package("rxode2", quiet = TRUE)) == 0) {
    stop("rxode2 not installed")
  }

  # Check tinyTEX installation
  if (!check_tinytex_installation()) {
    stop("TinyTeX is not properly installed. Please install it using tinytex::install_tinytex()")
  }

  is_windows <- tolower(get_os_name()) %>% startsWith("win")
  if (is_windows) {
    default_path <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
    if (file.exists(default_path)) {
      Sys.setenv("RSTUDIO_PANDOC" = default_path)
    }
  }

  tictoc::tic()
  results <- run_qualification_core(
    packages = packages,
    cpu = cpu,
    skip_vdiffr = skip_vdiffr,
    skip_python = skip_python
  )
  report <- render_report(
    results = results,
    packages = packages,
    fullname = fullname,
    initials = initials,
    output_dir = output_dir
  )
  tictoc::toc()

  if (!is.null(report)) {
    cat(sprintf("Qualification report: %s\n", report))
  }

  # Check if qualification passed
  qual_ok <- results$summarised %>% qualification_passed()
  print(ifelse(qual_ok, "QUALIFICATION SUCCESSFUL", "QUALIFICATION FAILED"))

  return(qual_ok)
}

#'
#' Run qualification core.
#'
#' @param packages packages to qualify
#' @param cpu number of workers to be used
#' @param skip_vdiffr skip Vdiffr tests, default is TRUE
#' @param skip_python skip Python tests (e.g. Pharmpy in campsistrans), default is TRUE
#' @return a list with the following items:
#'  - `summarised`: data frame with one row per package (columns 'Package', 'Version',
#'  'Passed', 'Failed', 'Warnings', 'Errors' and 'Skipped')
#'  - `all`: named list (one item per package) with the results of each test
#'  (columns 'file', 'context', 'Test', 'Passed', 'Failed', 'Warnings', 'Errors' and 'Skipped')
#'  - `warnings`: named list (one item per package) with the collected warnings, see `collect_package_warnings()`
#'  - `failures`: named list (one item per package) with the failed expectations and errors, see `collect_package_failures()`
#'  - `skipped`: named list (one item per package) with the skipped tests, see `collect_package_skipped()`
#' @importFrom purrr map map_df
#' @importFrom testthat test_package
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom pbmcapply progressBar
#' @export
run_qualification_core <- function(
  packages,
  cpu = 6L,
  skip_vdiffr = TRUE,
  skip_python = TRUE
) {
  packages_no <- length(packages)
  if (packages_no == 0) {
    stop("No packages to qualify")
  }

  # Check package existence and check that it was installed with tests
  for (package in packages) {
    # Check package exists (error is raised if it does not exist)
    pkg_path <- find.package(package)

    # Check that testthat repo exists
    test_repo <- file.path(pkg_path, "tests/testthat")
    if (!dir.exists(test_repo)) {
      stop(paste0("Package ", package, " does not have a testthat repository"))
    }
  }

  # 1 CPU per package, don't create workers if not needed
  if (packages_no <= cpu) {
    cpu <- packages_no
  }

  # Start cluster configuration
  if (cpu > 1) {
    cl <- parallel::makeCluster(cpu)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doSNOW::registerDoSNOW(cl)
    `%op%` <- foreach::`%dopar%`
  } else {
    `%op%` <- foreach::`%do%`
  }

  # Options and environment variables are set within the loop. When tests run
  # sequentially (current session), they must be restored afterwards.
  option_names <- c("campsisqual.options", "campsistrans.options", "campsis.options")
  old_options <- lapply(stats::setNames(nm = option_names), getOption)
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA)
  on.exit(
    {
      options(old_options)
      if (is.na(old_not_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv("NOT_CRAN" = old_not_cran)
    },
    add = TRUE
  )

  # Progress bar
  pb <- pbmcapply::progressBar(max = packages_no, style = "ETA")
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # foreach iterator, declared to avoid the 'no visible binding' note in R CMD check
  i <- NULL

  test_results <- foreach::foreach(i = seq_along(packages), .combine = append, .options.snow = opts) %op%
    {
      package <- packages[i]
      Sys.setenv("NOT_CRAN" = TRUE)
      # Tells campsisqual tests that a qualification is running (avoids recursive qualification)
      options(campsisqual.options = list(RUNNING_QUALIFICATION = TRUE))
      options(campsistrans.options = list(SKIP_PHARMPY_TESTS = skip_python))
      options(campsis.options = list(SKIP_LONG_TESTS = FALSE, SKIP_VDIFFR_TESTS = skip_vdiffr))
      ret_value <- list()
      ret_value[[package]] <- testthat::test_package(
        package,
        reporter = c("list"),
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
      )
      return(ret_value)
    }
  close(pb)

  # Clean test results
  test_results_cleaned <- test_results %>%
    purrr::map(~ clean_package_results(.x))

  # Collect warnings
  warnings <- test_results %>%
    purrr::map(~ collect_package_warnings(.x))

  # Collect failures and errors
  failures <- test_results %>%
    purrr::map(~ collect_package_failures(.x))

  # Collect skipped tests
  skipped <- test_results %>%
    purrr::map(~ collect_package_skipped(.x))

  summarisedResults <- packages %>%
    purrr::map_df(.f = function(package) {
      return(test_results_cleaned[[package]] %>% bind_package_and_version(package))
    })

  return(list(summarised = summarisedResults, all = test_results_cleaned, warnings = warnings, failures = failures, skipped = skipped))
}

#'
#' Get initials from full name.
#'
#' @param fullname full name, e.g. 'John Doe'
#' @return initials, e.g. 'JD'
get_initials <- function(fullname) {
  parts <- strsplit(trimws(fullname), "\\s+")[[1]]
  return(paste0(substring(parts, 1, 1), collapse = ""))
}

#'
#' Get the report filename (without extension).
#'
#' @param packages packages that were qualified
#' @param initials initials of the person running the qualification
#' @param date date of the qualification
#' @return report filename, character
get_report_filename <- function(packages, initials, date = Sys.Date()) {
  return(sprintf(
    "IQ_OQ_%s-%s_%s_%s_%s",
    "Campsis_Suite",
    as.character(getNamespaceVersion("campsis")),
    get_os_name(short = TRUE),
    initials,
    format(date, "%Y%m%d")
  ))
}

#'
#' Render the IQ/OQ qualification report with rmarkdown (PDF via LaTeX).
#'
#' @param results results, as returned by `run_qualification_core()`
#' @param packages packages that were qualified
#' @param fullname fullname of the person running the qualification
#' @param initials username initials to be used in the filename. If not provided, initials are deduced from the fullname.
#' @param output_dir output directory
#' @return the path to the PDF report, or NULL if the report could not be rendered
#' @importFrom rmarkdown render
render_report <- function(results, packages, fullname, initials = NULL, output_dir = getwd()) {
  if (is.null(initials)) {
    initials <- get_initials(fullname)
  }
  report_filename <- get_report_filename(packages = packages, initials = initials)
  title <- "Campsis suite qualification report"

  # Export Rmd template to a temporary working directory
  work_dir <- tempfile(pattern = "campsisqual_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  rmd_file <- file.path(work_dir, "qualification_report.Rmd")
  writeLines(text = campsisqual::suite_qualification_template, con = rmd_file)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  report <- tryCatch(
    rmarkdown::render(
      input = rmd_file,
      params = list(
        set_title = title,
        results = results,
        packages = packages,
        fullname = fullname
      ),
      output_file = report_filename,
      output_dir = output_dir,
      intermediates_dir = work_dir,
      # Template can use the internal functions of campsisqual
      envir = new.env(parent = asNamespace("campsisqual")),
      quiet = TRUE
    ),
    error = function(cond) {
      warning(sprintf("Qualification report could not be rendered: %s", conditionMessage(cond)), call. = FALSE)
      return(NULL)
    }
  )

  return(report)
}
