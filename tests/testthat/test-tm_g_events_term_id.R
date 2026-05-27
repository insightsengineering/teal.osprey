testthat::test_that("tm_g_events_term_id dispatches on term_var class", {
  testthat::skip_if_not_installed("teal.picks")

  mod_default <- tm_g_events_term_id(
    label = "Common AE",
    dataname = "ADAE",
    term_var = teal.transform::choices_selected(
      selected = "AEDECOD",
      choices = "AEDECOD"
    ),
    arm_var = teal.transform::choices_selected(
      selected = "ACTARMCD",
      choices = "ACTARMCD"
    )
  )
  testthat::expect_s3_class(mod_default, "teal_module")
  testthat::expect_identical(mod_default$server, srv_g_events_term_id)

  mod_picks <- tm_g_events_term_id(
    label = "Common AE",
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
  testthat::expect_s3_class(mod_picks, "teal_module")
  testthat::expect_identical(mod_picks$server, srv_g_events_term_id_picks)
  testthat::expect_equal(mod_picks$datanames, c("ADAE", "ADSL"))
})

testthat::test_that("tm_g_events_term_id.picks rejects multiple variable selection", {
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

  testthat::expect_error(
    tm_g_events_term_id(
      label = "Common AE",
      term_var = term_var,
      arm_var = arm_var
    ),
    "`term_var` must use variables\\(\\.\\.\\., multiple = FALSE\\)"
  )
})
