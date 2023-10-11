test_that("as_numeric_from_comma_sep_str converts comma sep string into numeric vector", {
  expect_equal(as_numeric_from_comma_sep_str("5.65"), 5.65)
  expect_equal(as_numeric_from_comma_sep_str("5,-6, 0"), c(5, -6, 0))
})

test_that("as_numeric_from_comma_sep_str throws warning if any entries are not numeric and converts those to NAs", {
  warning_msg <- "NAs introduced by coercion"
  expect_warning(result <- as_numeric_from_comma_sep_str("hello"), warning_msg)
  expect_equal(result, as.numeric(NA))
  expect_warning(result <- as_numeric_from_comma_sep_str("5,6,,hello,3"), warning_msg)
  expect_equal(result, c(5, 6, NA, NA, 3))
  expect_warning(result <- as_numeric_from_comma_sep_str("6f, 0.14,  f;"), warning_msg)
  expect_equal(result, c(NA, 0.14, NA))
})

test_that("as_numeric_from_comma_sep_str returns NULL if input is NULL or whitespace", {
  expect_null(as_numeric_from_comma_sep_str(NULL))
  expect_null(as_numeric_from_comma_sep_str("  "))
  expect_null(as_numeric_from_comma_sep_str(""))
})

test_that("as_numeric_from_comma_sep_str ignores empty values at end of string", {
  expect_equal(as_numeric_from_comma_sep_str("5.65,"), 5.65)
  expect_equal(as_numeric_from_comma_sep_str("5.65, "), 5.65)
})
