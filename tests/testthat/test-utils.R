testthat::test_that("as_numeric_from_comma_sep_str converts comma sep string into numeric vector", {
  testthat::expect_equal(as_numeric_from_comma_sep_str("5.65"), 5.65)
  testthat::expect_equal(as_numeric_from_comma_sep_str("5,-6, 0"), c(5, -6, 0))
})

testthat::test_that(
  "as_numeric_from_comma_sep_str throws warning if any entries are not numeric and converts those to NAs",
  {
    warning_msg <- "NAs introduced by coercion"
    testthat::expect_warning(result <- as_numeric_from_comma_sep_str("hello"), warning_msg)
    testthat::expect_equal(result, as.numeric(NA))
    testthat::expect_warning(result <- as_numeric_from_comma_sep_str("5,6,,hello,3"), warning_msg)
    testthat::expect_equal(result, c(5, 6, NA, NA, 3))
    testthat::expect_warning(result <- as_numeric_from_comma_sep_str("6f, 0.14,  f;"), warning_msg)
    testthat::expect_equal(result, c(NA, 0.14, NA))
  }
)

testthat::test_that("as_numeric_from_comma_sep_str returns NULL if input is NULL or whitespace", {
  testthat::expect_null(as_numeric_from_comma_sep_str(NULL))
  testthat::expect_null(as_numeric_from_comma_sep_str("  "))
  testthat::expect_null(as_numeric_from_comma_sep_str(""))
})

testthat::test_that("as_numeric_from_comma_sep_str ignores empty values at end of string", {
  testthat::expect_equal(as_numeric_from_comma_sep_str("5.65,"), 5.65)
  testthat::expect_equal(as_numeric_from_comma_sep_str("5.65, "), 5.65)
})
