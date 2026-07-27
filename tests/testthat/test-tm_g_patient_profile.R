patient_id_cs <- teal.transform::choices_selected(
  selected = "USUBJID",
  choices = c("USUBJID")
)

sl_start_date_cs <- teal.transform::choices_selected(
  selected = "TRTSDTM",
  choices = c("TRTSDTM", "RANDDT")
)

patient_id_picks <- teal.picks::variables(
  choices = dplyr::where(is.character),
  selected = 1L
)

sl_start_date_picks <- teal.picks::variables(
  choices = dplyr::where(function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))),
  selected = 1L
)

testthat::describe("tm_g_patient_profile input validation", {
  it("plot arguments input validation", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = patient_id_cs,
            sl_dataname = "ADSL",
            sl_start_date = sl_start_date_cs,
            plot_height = c(1200, 5000, 400)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_height' failed"
    )

    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = patient_id_cs,
            sl_dataname = "ADSL",
            sl_start_date = sl_start_date_cs,
            plot_width = c(1200, 5000, 400)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })

  it("Forcing conversion from multiple picks to single", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = teal.picks::variables(
              choices = dplyr::where(is.character),
              selected = 1L,
              multiple = TRUE
            ),
            sl_dataname = "ADSL",
            sl_start_date = sl_start_date_cs
          ),
          classes = "picks_delayed"
        )
      },
      "metadata does not match the requirement for patient_id"
    )

    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = patient_id_cs,
            sl_dataname = "ADSL",
            sl_start_date = teal.picks::variables(
              choices = dplyr::where(function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))),
              selected = 1L,
              multiple = TRUE
            )
          ),
          classes = "picks_delayed"
        )
      },
      "metadata does not match the requirement for sl_start_date"
    )
  })
})

testthat::describe("tm_g_patient_profile module creation", {
  it("creates a teal module using choices_selected", {
    mod <- tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_cs,
      sl_dataname = "ADSL",
      sl_start_date = sl_start_date_cs,
      plot_height = c(1200, 400, 5000)
    ) |>
      suppressWarnings(classes = "picks_delayed")
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using teal.picks variables", {
    mod <- tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_picks,
      sl_dataname = "ADSL",
      sl_start_date = sl_start_date_picks,
      plot_height = c(1200, 400, 5000)
    ) |>
      suppressWarnings(classes = "picks_delayed")
    testthat::expect_s3_class(mod, "teal_module")
  })
})
