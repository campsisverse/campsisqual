
# campsisqual <img src='man/figures/logo.png' align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/campsisverse/campsisqual/workflows/R-CMD-check/badge.svg)](https://github.com/campsisverse/campsisqual/actions)
<!-- badges: end -->

A library dedicated to:

- the qualification of the Campsis suite (installation and operational
  qualification, IQ/OQ), which runs the tests of each package and issues
  a PDF report,
- the qualification of Campsis models after import, by comparing the
  Campsis-simulated predictions with the original predictions of the
  modelling tool (e.g. NONMEM).

## Installation

Install the latest release from GitHub:

``` r
remotes::install_github("Calvagone/campsisqual")
```

Install TinyTeX (reports are rendered in PDF with LaTeX):

``` r
tinytex::install_tinytex()
```

## Qualify the Campsis suite

The packages to qualify must be installed with their tests. For
instance:

``` r
install.packages(
  c("campsismod", "campsis", "campsisnca", "campsismisc"),
  INSTALL_opts = "--install-tests"
)
remotes::install_github("Calvagone/campsisqual", INSTALL_opts = "--install-tests")
```

Then run the qualification:

``` r
library(campsisqual)

run_qualification(
  packages = c(
    "campsismod",
    "campsis",
    "campsisnca",
    "campsismisc",
    "campsisqual"
  ),
  fullname = "<YOUR_NAME>",
  output_dir = "<OUTPUT_DIRECTORY>"
)
```

`run_qualification()` returns `TRUE` if the qualification is successful
(i.e. no test failed and no error occurred) and writes the IQ/OQ report
(e.g. `IQ_OQ_Campsis_Suite-<VERSION>_<OS>_<INITIALS>_<DATE>.pdf`) into
the output directory. This report contains:

- the computer information (operating system, R version, simulation
  engines, etc.),
- a summary table of the qualification (number of tests passed, failed,
  with warnings, errors or skipped per package),
- for each package: an overview of the package, based on risk metrics
  (documentation, maintenance, testing, transparency), and the test
  results by test script.

## Qualify a Campsis model

Compare the predictions of an imported Campsis model with the original
predictions (e.g. NONMEM):

``` r
library(campsis)
library(campsisqual)

model <- read.campsis("<PATH_TO_CAMPSIS_MODEL>")
dataset <- read.csv("<PATH_TO_NONMEM_DATASET>")
ipred <- read.nonmem("<PATH_TO_NONMEM_OUTPUT_TABLE>")[[1]]

summary <- qualify(
  model = model,
  dataset = dataset,
  ipred = ipred,
  variables = "CP",
  dest = "rxode2"
)
passed(summary)

# Export the qualification summary to PDF
write(summary, file = "model_qualification.pdf")
```
