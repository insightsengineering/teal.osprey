.onLoad <- function(libname, pkgname) { # nolint
  teal.logger::register_logger(namespace = "teal.osprey")
  teal.logger::register_handlers("teal.osprey")
}
