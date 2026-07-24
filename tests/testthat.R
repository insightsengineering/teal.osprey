pkg_name <- "teal.osprey"
library(pkg_name, character.only = TRUE)
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_check(pkg_name)
}
