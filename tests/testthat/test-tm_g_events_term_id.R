testthat::test_that("tm_g_events_term_id builds a teal module with picks encodings", {
  testthat::skip_if_not_installed("teal.picks")

  mod <- tm_g_events_term_id(
    label = "Common AE",
    dataname = "ADAE",
    parent_dataname = "ADSL",
    term_var = teal.picks::picks(
      teal.picks::datasets("ADAE"),
      teal.picks::variables(
        choices = "AEDECOD",
        selected = "AEDECOD"
      )
    ),
    arm_var = teal.picks::picks(
      teal.picks::datasets("ADSL"),
      teal.picks::variables(
        choices = "ACTARMCD",
        selected = "ACTARMCD"
      )
    )
  )
  testthat::expect_s3_class(mod, "teal_module")
  testthat::expect_identical(mod$server, srv_g_events_term_id)
  testthat::expect_equal(mod$datanames, c("ADAE", "ADSL"))
})

testthat::test_that("tm_g_events_term_id coerces multiple variable selection", {
  testthat::skip_if_not_installed("teal.picks")

  term_var <- teal.picks::picks(
    teal.picks::datasets("ADAE"),
    teal.picks::variables(
      choices = c("AEDECOD", "AETERM"),
      selected = c("AEDECOD", "AETERM"),
      multiple = TRUE
    )
  )
  arm_var <- teal.picks::picks(
    teal.picks::datasets("ADSL"),
    teal.picks::variables(
      choices = "ACTARMCD",
      selected = "ACTARMCD"
    )
  )

  mod <- testthat::expect_warning(
    tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = term_var,
      arm_var = arm_var
    ),
    "accepts only a single variable selection"
  )
  testthat::expect_false(teal.picks::is_pick_multiple(mod$ui_args$term_var$variables))
})
