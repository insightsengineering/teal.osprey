#' @importFrom utils getFromNamespace
get_rcode_header <- getFromNamespace(get_rcode_header, "teal.devel")

has_source_attribute <- function(x) {
    if (is.data.frame(x)) x <- list(x)
    all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}

#' @importFrom teal.devel white_small_well
whiteSmallWell <- white_small_well
