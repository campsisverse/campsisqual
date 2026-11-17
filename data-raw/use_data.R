template <- file.path("data-raw", "model_qualification_template.Rmd")
model_qualification_template <- readChar(template, file.info(template)$size)
usethis::use_data(model_qualification_template, overwrite = TRUE)

template <- file.path("data-raw", "suite_qualification_template.Rmd")
suite_qualification_template <- paste0(readLines(template), collapse = "\n")
usethis::use_data(suite_qualification_template, overwrite = TRUE)
