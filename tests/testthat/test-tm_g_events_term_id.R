describe("tm_g_events_term_id module creation", {
  it("using choices_selected", {
    mod <- tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = choices_selected(
        selected = "AEDECOD",
        choices = c(
          "AEDECOD", "AETERM",
          "AEHLT", "AELLT", "AEBODSYS"
        )
      ),
      arm_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("ACTARM", "ACTARMCD")
      )
    )

    expect_s3_class(mod, "teal_module")
    expect_identical(mod$server, srv_g_events_term_id)
    expect_equal(mod$datanames, c("ADAE", "ADSL"))
  })

  it("using picks", {

    mod <- tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = picks(
        datasets("ADAE"),
        variables(
          choices = "AEDECOD",
          selected = "AEDECOD"
        )
      ),
      arm_var = picks(
        datasets("ADSL"),
        variables(
          choices = "ACTARMCD",
          selected = "ACTARMCD"
        )
      )
    )
    expect_s3_class(mod, "teal_module")
    expect_identical(mod$server, srv_g_events_term_id)
    expect_equal(mod$datanames, c("ADAE", "ADSL"))
  })

  data <- within(teal_data(), {
    ADSL <- rADSL
    ADAE <- rADAE
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  it("using choices_selected works", {
    mod <- tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = choices_selected(
        selected = "AEDECOD",
        choices = c(
          "AEDECOD", "AETERM",
          "AEHLT", "AELLT", "AEBODSYS"
        )
      ),
      arm_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("ACTARM", "ACTARMCD")
      )
    )
    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          arm_ref = "ARM A",
          arm_trt = "ARM B",
          ci = "wald", conf_level = 0.95,
          raterange = c(.1, 1),
          diffrange = c(-.5, .5),
          reverse = FALSE,
          fontsize = 5
        )
        expect_no_error(session$returned())
      }
    )
  })

  it("using picks works", {
    mod <- tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = picks(
        datasets("ADAE"),
        variables(
          choices = "AEDECOD",
          selected = "AEDECOD"
        )
      ),
      arm_var = picks(
        datasets("ADSL"),
        variables(
          choices = "ACTARMCD",
          selected = "ACTARMCD"
        )
      )
    )

    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          arm_ref = "ARM A",
          arm_trt = "ARM B",
          ci = "wald", conf_level = 0.95,
          raterange = c(.1, 1),
          diffrange = c(-.5, .5),
          reverse = FALSE,
          fontsize = 5
        )
        expect_no_error(session$returned())
      }
    )
  })
})

test_that("tm_g_events_term_id coerces multiple variable selection", {
  term_var <- picks(
    datasets("ADAE"),
    variables(
      choices = c("AEDECOD", "AETERM"),
      selected = c("AEDECOD", "AETERM"),
      multiple = TRUE
    )
  )
  arm_var <- picks(
    datasets("ADSL"),
    variables(
      choices = "ACTARMCD",
      selected = "ACTARMCD"
    )
  )

  mod <- expect_warning(
    tm_g_events_term_id(
      label = "Common AE",
      dataname = "ADAE",
      parent_dataname = "ADSL",
      term_var = term_var,
      arm_var = arm_var
    ),
    "accepts only a single variable selection"
  )
  expect_false(is_pick_multiple(mod$ui_args$term_var$variables))
})
