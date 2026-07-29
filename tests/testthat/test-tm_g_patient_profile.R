patient_id_cs <- teal.transform::choices_selected(
  choices = "USUBJID",
  selected = "USUBJID"
)

sl_start_date_cs <- teal.transform::choices_selected(
  choices = c("TRTSDTM", "RANDDT"),
  selected = "TRTSDTM"
)

patient_id_picks <- teal.picks::variables(
  choices = "USUBJID",
  selected = "USUBJID"
)

sl_start_date_picks <- teal.picks::variables(
  choices = c("TRTSDTM", "RANDDT"),
  selected = "TRTSDTM"
)

ex_var_cs <- teal.transform::choices_selected(
  choices = "PARCAT2",
  selected = "PARCAT2"
)

ex_var_picks <- variables(
  choices = "PARCAT2",
  selected = "PARCAT2"
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
            sl_start_date = sl_start_date_cs,
            plot_height = c(1200, 5000, 400)
          ),
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "Assertion on 'plot_height' failed"
    )

    expect_error(
      expect_warning(
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
        "is deprecated"
      ),
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
  it("fails without datanames", {
    expect_error(
      suppressWarnings(
        tm_g_patient_profile(
          label = "Patient Profile Plot",
          patient_id = patient_id_cs,
          sl_dataname = "ADSL",
          sl_start_date = sl_start_date_cs,
          plot_height = c(1200, 400, 5000)
        ),
        classes = c("picks_delayed", "lifecycle_warning_deprecated")
      ),
      "Please specify some datanames."
    )
  })

  it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(
      tm_g_patient_profile(
        label = "Patient Profile Plot",
        patient_id = patient_id_cs,
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ex_var = ex_var_cs,
        sl_start_date = sl_start_date_cs,
        plot_height = c(1200, 400, 5000)
      ),
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using teal.picks variables", {
    mod <- suppressWarnings(
      tm_g_patient_profile(
        label = "Patient Profile Plot",
        patient_id = patient_id_picks,
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ex_var = ex_var_picks,
        sl_start_date = sl_start_date_picks,
        plot_height = c(1200, 400, 5000)
      ),
      classes = "picks_delayed"
    )
    expect_s3_class(mod, "teal_module")
  })

  data <- within(teal_data(), {
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL
    ADAE <- mutate(rADAE, ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADCM <- mutate(rADCM, ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    # The step below is to pre-process ADCM to legacy standard
    ADCM <- ADCM %>%
      select(-starts_with("ATC")) %>%
      unique()
    ADRS <- mutate(rADRS, ADT = as.Date(ADTM))
    ADEX <- mutate(rADEX, ASTDT = as.Date(ASTDTM), AENDT = as.Date(AENDTM))
    ADLB <- mutate(rADLB, ADT = as.Date(ADTM), LBSTRESN = as.numeric(LBSTRESC))
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
        sl_start_date = sl_start_date_cs,
        plot_height = c(1200, 400, 5000)
      ),
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )

    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          session$setInputs(select_ADaM = "ADEX", x_limit = "-28, 750")
          expect_no_error(session$returned())
        }
      ),
      regexp = "contains multiple values"
    )
  })

  it("works using teal.picks variables", {
    mod <- suppressWarnings(tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = patient_id_picks,
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      sl_start_date = sl_start_date_picks,
      ex_var = ex_var_picks,
      plot_height = c(1200, 400, 5000)
    ), classes = "picks_delayed")

    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          session$setInputs(select_ADaM = "ADEX", x_limit = "-28, 750")
          expect_no_error(session$returned())
        }
      ),
      regexp = "contains multiple values"
    )
  })
})
