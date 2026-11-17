#' Are vectors equal given the tolerance.
#'
#' @param x first vector
#' @param xref second vector
#' @param tolerance relative tolerance
#' @param id subject ID (for troubleshooting)
#' @param type vector type (for troubleshooting)
#' @return logical vector
#' @importFrom assertthat assert_that
#' @export
are_equal <- function(x, xref, tolerance, id, type) {
  assertthat::assert_that(
    length(x) == length(xref),
    msg = sprintf("x and xref (%s) do not have the same length (ID=%s)", type, as.character(id))
  )
  relative_change <- (x - xref) / xref
  return(abs(relative_change) < tolerance | (x == 0 & xref == 0))
}

#' Compare NONMEM results with CAMPSIS results, according to the given tolerance.
#'
#' @param ipred individual predictions to be compared with (=reference results)
#' @param campsis Campsis results, data frame
#' @param variables variables to be compared
#' @param tolerance comparison tolerance
#' @param dest destination engine that was used in Campsis
#' @param ipred_source source of individual predictions, default is "NONMEM" (informative field)
#' @return logical vector
#' @importFrom dplyr distinct filter mutate pull rename rename_at select
#' @importFrom ggplot2 aes geom_line geom_point ggplot ggtitle scale_colour_discrete ylab
#' @importFrom tibble add_column as_tibble
#' @importFrom campsis obs_only
#' @export
compare <- function(ipred, campsis, variables, tolerance, dest = "rxode2", ipred_source = "NONMEM") {
  # Check destination engine
  check_dest(dest)

  # Filtering on observations
  ref_results <- ipred %>% campsis::obs_only()
  campsis_results <- as.data.frame(campsis) %>% campsis::obs_only()

  # Access ORIGINAL_ID column from Campsis results
  # Originally column was accessed from NONMEM results
  # But this is not a good idea because TAB file is formatted (e.g. 1.0510E+04)
  # and therefore the ORIGINAL_ID can't be a big integer or a string
  ids <- unique(campsis_results$ID)
  if ("ORIGINAL_ID" %in% colnames(campsis_results)) {
    original_ids <- campsis_results %>%
      dplyr::select(ID, ORIGINAL_ID) %>%
      dplyr::distinct() %>%
      dplyr::pull(ORIGINAL_ID)
    if (original_ids %>% length() != ids %>% length()) {
      stop("Incorrect column ORIGINAL_ID")
    }
  } else {
    original_ids <- character(ids %>% length())
  }

  # Instantiate a new qualification summary object
  qualification_summary <- new("qualification_summary")
  qualification_summary@ids <- as.integer(ids)
  qualification_summary@original_ids <- as.character(original_ids)
  qualification_summary@variables <- variables
  qualification_summary@ipred_source <- ipred_source
  qualification_summary@dest <- dest
  qualification_summary@tolerance <- tolerance

  vars_of_interest <- variables

  # Init qualification dataframe
  qualification <- data.frame(ID = ids)
  for (variable in variables) {
    qualification[, variable] <- rep(NA, length(ids))
  }

  for (id in ids) {
    index <- which(id == ids)
    original_id <- original_ids[index]

    ref_subj <- ref_results %>%
      dplyr::filter(ID == id) %>%
      dplyr::mutate(Simulation = ipred_source) %>%
      dplyr::select(c("ID", "TIME", "Simulation", dplyr::all_of(vars_of_interest)))

    campsis_subj <- campsis_results %>%
      dplyr::filter(ID == id) %>%
      dplyr::mutate(Simulation = dest) %>%
      dplyr::select(c("ID", "TIME", "Simulation", dplyr::all_of(vars_of_interest)))

    if (!all(are_equal(ref_subj$TIME, campsis_subj$TIME, tolerance = tolerance, id = id, type = "TIME"))) {
      stop(paste0("Times are not identical between NONMEM and CAMPSIS for subject ", id))
    }

    subj <- dplyr::bind_rows(campsis_subj, ref_subj)
    subj <- subj %>%
      tidyr::gather(key = "Variable", value = "value", dplyr::all_of(vars_of_interest), -ID, -TIME, -Simulation)
    subj$Pass <- FALSE

    # Qualification results summary
    for (output in vars_of_interest) {
      ref_output <- subj %>%
        dplyr::filter(Variable == output & Simulation == ipred_source) %>%
        dplyr::pull(value)

      campsis_output <- subj %>%
        dplyr::filter(Variable == output & Simulation == dest) %>%
        dplyr::pull(value)

      same_output <- are_equal(ref_output, campsis_output, tolerance = tolerance, id = id, type = "OUTPUT")
      if (any(is.na(same_output))) {
        stop(paste0("NA's detected in original ID ", original_id))
      }
      if (all(same_output)) {
        qualification[which(qualification$ID == id), output] <- "PASS"
      } else {
        qualification[which(qualification$ID == id), output] <- "FAIL"
      }

      subj <- subj %>%
        dplyr::mutate(Pass = ifelse(Variable == output & Simulation == ipred_source & same_output, TRUE, Pass))
      subj <- subj %>%
        dplyr::mutate(Pass = ifelse(Variable == output & Simulation == dest & same_output, TRUE, Pass))
    }
    subj$Pass <- ifelse(subj$Pass, "OK", "NOK")

    # All pass
    all_pass <- all(qualification[which(qualification$ID == id), vars_of_interest] == "PASS")
    all_pass <- ifelse(all_pass, "Pass", "Fail")

    # Saving plots
    plots <- list()
    for (output in vars_of_interest) {
      data <- subj %>%
        dplyr::filter(Variable == output) %>%
        dplyr::mutate(Simulation = factor(Simulation, levels = c(ipred_source, dest)))
      p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = TIME, y = value, group = Simulation)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~Simulation) +
        ggplot2::geom_point(mapping = ggplot2::aes(color = Pass), size = 4) +
        ggplot2::ylab(output) +
        ggplot2::scale_colour_manual(values = c("NOK" = "#FF0000", "OK" = "#008080"), drop = FALSE)
      plots[[output]] <- p
    }
    qualification_summary@plots[[as.character(id)]] <- plots

    # Saving table output
    time_campsis <- paste0("Time ", dest)
    time_ref <- paste0("Time ", ipred_source)

    tryCatch(
      {
        subj_ref <- subj %>%
          dplyr::filter(Simulation == ipred_source) %>%
          tidyr::spread(Simulation, value) %>%
          dplyr::rename_at(.vars = "TIME", .funs = ~time_ref)

        subj_campsis <- subj %>%
          dplyr::filter(Simulation == dest) %>%
          tidyr::spread(Simulation, value) %>%
          dplyr::rename_at(.vars = "TIME", .funs = ~time_campsis)
      },
      error = function(cond) {
        warning(paste0("Problem with original ID ", original_id))
      }
    )

    subj_ref <- tibble::as_tibble(subj_ref)
    subj_ref <- subj_ref %>%
      tibble::add_column(TIME_CAMPSIS = subj_campsis[, time_campsis], .after = time_ref) %>%
      dplyr::rename_at(.vars = "TIME_CAMPSIS", .funs = ~time_campsis)
    subj_ref[, dest] <- subj_campsis[, dest]
    qualification_summary@tables[[as.character(id)]] <- subj_ref
  }

  # Saving summary
  qualification_summary@summary <- qualification

  return(qualification_summary)
}
