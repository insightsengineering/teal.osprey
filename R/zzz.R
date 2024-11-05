.onLoad <- function(libname, pkgname) {
  # Fixes R CMD check note on "All declared Imports should be used."
  # teal.data is necessary to access S3 method names.teal_data
  teal.data::teal_data

  teal.logger::register_logger(namespace = "teal.osprey")
  teal.logger::register_handlers("teal.osprey")
}
