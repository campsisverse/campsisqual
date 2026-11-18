#' Check the destination engine.
#'
#' @param dest destination engine, character
#'
check_dest <- function(dest) {
  if (!(dest %>% length() == 1 && dest %in% c("rxode2", "mrgsolve"))) {
    stop("Dest must be rxode2 or mrgsolve")
  }
}

#' Qualify function.
#'
#' @param model Campsis model to be qualified
#' @param dataset Campsis dataset or data frame to be qualified
#' @param ipred individual predictions to be compared with (=reference results)
#' @param variables variables to compare
#' @param tolerance relative tolerance accepted when comparing 2 values
#' @param dest destination engine to use
#' @param seed simulation/table export seed, default value is 1
#' @param settings simulation settings, please note NOCB is enabled by default
#' @param idref ID reference, "dataset" or "ipred" (other IDs will be ignored)
#' @param ipred_source source of individual predictions, default is "NONMEM" (informative field)
#' @return qualification summary
#' @importFrom dplyr arrange filter
#' @importFrom campsismod export
#' @importFrom campsis NOCB Settings simulate
#' @export
qualify <- function(
  model,
  dataset,
  ipred,
  variables,
  tolerance = 1e-2,
  dest = "rxode2",
  seed = 1,
  settings = campsis::Settings(campsis::NOCB(TRUE)),
  idref = "ipred",
  ipred_source = "NONMEM"
) {
  # Check destination engine
  check_dest(dest)

  # Dataset to table
  is_campsis_dataset <- is(dataset, "dataset")
  if (is_campsis_dataset) {
    settings_nm <- settings
    settings_nm@nocb@enable <- TRUE # NOCB always TRUE for NONMEM
    table <- dataset %>%
      campsismod::export(dest = "mrgsolve", model = model, seed = seed, settings = settings_nm)
  } else {
    table <- dataset
  }

  # Ignore IDs mechanism
  dataset_ids <- table$ID %>% unique()
  ipred_ids <- ipred$ID %>% unique()
  if (idref == "dataset") {
    ref_ids <- dataset_ids
  } else if (idref == "ipred") {
    ref_ids <- ipred_ids
  } else {
    stop("idref must be 'dataset' or 'ipred'")
  }
  table <- table %>%
    dplyr::filter(.data$ID %in% ref_ids) %>%
    dplyr::arrange(.data$ID, .data$TIME) %>%
    add_simulation_id_column()
  ipred <- ipred %>%
    dplyr::filter(.data$ID %in% ref_ids) %>%
    dplyr::arrange(.data$ID, .data$TIME) %>%
    add_simulation_id_column()

  # Check destination engine
  check_dest(dest)

  # Simulate with rxode2 or mrgsolve
  # If dataset is a Campsis dataset, it will be used as is (e.g. Declare added automatically with mrgsolve, etc.)
  # If dataset is a table, we give the table with the ID (from 1 to subjects) and ORIGINAL_ID columns
  campsis <- campsis::simulate(
    model,
    dataset = if (is_campsis_dataset) {
      dataset
    } else {
      table
    },
    dest = dest,
    seed = seed,
    settings = settings,
    outvars = variables
  )

  # Append ORIGINAL_ID
  campsis <- append_original_id(campsis, table)

  # Compare results
  summary <- compare(
    ipred,
    campsis,
    variables = variables,
    tolerance = tolerance,
    dest = dest,
    ipred_source = ipred_source
  )

  # Show if qualification passed or failed
  cat(ifelse(summary %>% passed(), "QUALIFICATION SUCCESSFUL", "QUALIFICATION FAILED"))

  return(summary)
}

#' Append original ID to simulation output if it exists in the dataset.
#'
#' @param x Campsis output
#' @param dataset Campsis dataset or data frame
#' @param dataset engine table OR Campsis dataset
#' @return updated output
#' @importFrom dplyr distinct left_join relocate select
append_original_id <- function(x, dataset) {
  if (is(dataset, "data.frame")) {
    if ("ORIGINAL_ID" %in% colnames(dataset)) {
      id_pairs <- dataset %>% dplyr::select("ID", "ORIGINAL_ID") %>% dplyr::distinct()
      x <- x %>% dplyr::left_join(id_pairs, by = "ID") %>% dplyr::relocate("ID", "ORIGINAL_ID")
    }
  }
  return(x)
}

#' Add simulation ID column (id's starting at 1 and consecutive). Original column ID
#' will be replaced by the new simulation ID column, after being renamed into
#' ORIGINAL_ID column.
#'
#' @param dataset NONMEM dataset
#' @param id current identifier column, default is 'ID'
#' @return updated data frame
#' @importFrom dplyr arrange group_by group_indices rename_at select
add_simulation_id_column <- function(dataset, id = "ID") {
  if ("ID" %in% colnames(dataset) && id != "ID") {
    dataset <- dataset %>% dplyr::select(-"ID")
  }
  # Current ID is renamed into ORIGINAL_ID
  if (!("ORIGINAL_ID" %in% colnames(dataset))) {
    dataset <- dataset %>%
      dplyr::rename_at(.vars = id, .funs = function(x) {
        "ORIGINAL_ID"
      })
  }
  # Arrange rows by ORIGINAL_ID
  dataset <- dataset %>%
    dplyr::arrange(.data$ORIGINAL_ID)

  # Add simulation ID column
  dataset <- dataset %>%
    tibble::add_column(
      ID = dataset %>% dplyr::group_by(.data$ORIGINAL_ID) %>% dplyr::group_indices(),
      .before = "ORIGINAL_ID"
    )
  # Arrange rows by ID
  dataset <- dataset %>% dplyr::arrange(.data$ID)
  return(dataset)
}
