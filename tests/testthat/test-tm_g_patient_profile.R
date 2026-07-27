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

ex_var_cs <- teal.transform::choices_selected(
  selected = "PARCAT2",
  choices = "PARCAT2"
)

ex_var_picks <- variables(
  selected = "PARCAT2",
  choices = "PARCAT2"
)

describe("tm_g_patient_profile input validation", {
  it("plot arguments input validation", {
    expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = patient_id_cs,
            sl_dataname = "ADSL",
            sl_start_date = sl_start_date_cs
          ),
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "Assertion on 'plot_height' failed"
    )

    expect_error(
      {
        suppressWarnings(
          tm_g_patient_profile(
            label = "Patient Profile Plot",
            patient_id = patient_id_cs,
            sl_dataname = "ADSL",
            sl_start_date = sl_start_date_cs
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })

  it("Forcing conversion from multiple picks to single", {
    expect_error(
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
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "metadata does not match the requirement for patient_id"
    )

    expect_error(
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
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "metadata does not match the requirement for sl_start_date"
    )
  })
})

describe("tm_g_patient_profile module creation", {
  it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_cs,
      sl_dataname = "ADSL",
      sl_start_date = sl_start_date_cs
    ), classes = c("picks_delayed", "lifecycle_warning_deprecated"))
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using teal.picks variables", {
    mod <- suppressWarnings(tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_picks,
      sl_dataname = "ADSL",
      sl_start_date = sl_start_date_picks
    ), classes = "picks_delayed")
    expect_s3_class(mod, "teal_module")
  })


  data <- within(teal_data(), {
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL
    ADAE <- rADAE %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADCM <- rADCM %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    # The step below is to pre-process ADCM to legacy standard
    ADCM <- ADCM %>%
      select(-starts_with("ATC")) %>%
      unique()
    ADRS <- rADRS %>% mutate(ADT = as.Date(ADTM))
    ADEX <- rADEX %>% mutate(ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADLB <- rADLB %>% mutate(ADT = as.Date(ADTM), LBSTRESN = as.numeric(LBSTRESC))
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  it("works using choices_selected", {
    mod <- suppressWarnings(
      tm_g_patient_profile(
        label = "Patient Profile Plot",
        patient_id = patient_id_cs,
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ex_var = ex_var_cs,
        sl_start_date = sl_start_date_cs
      ),
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )

    # expect_warning(
    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInput(select_ADaM = c("ADEX"))
        # browser()
        expect_no_error(session$returned())
      }
    )
    #   regexp = "cartesian join"
    # )
  })

  it("works using teal.picks variables", {
    mod <- suppressWarnings(tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_picks,
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      sl_start_date = sl_start_date_picks,
      ex_var = ex_var_picks
    ), classes = "picks_delayed")

    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInput(select_ADaM = "ADEX")
        expect_no_error(session$returned())
      }
    )
  })
})
