#' @importFrom utils getFromNamespace
get_fun <- function (pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
get_rcode_header <- get_fun("teal.devel", "get_rcode_header")

has_source_attribute <- function(x) {
    if (is.data.frame(x)) x <- list(x)
    all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}

#' @importFrom teal.devel white_small_well
whiteSmallWell <- white_small_well # nolint