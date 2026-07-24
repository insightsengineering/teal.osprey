arm_var_cs <- teal.transform::choices_selected(
  choices = c("ACTARM", "ACTARMCD"),
  selected = "ACTARMCD"
)

group_var_cs <- teal.transform::choices_selected(
  selected = c("SEX", "REGION1", "RACE"),
  choices = c("SEX", "REGION1", "RACE")
)

describe("tm_g_ae_sub input validation", {
  it("plot arguments input validation", {
    expect_error(
      {
        suppressWarnings(
          tm_g_ae_sub(
            label = "subgroups Plot",
            dataname = "ADAE",
            arm_var = arm_var_cs,
            group_var = group_var_cs,
            plot_height = c(0, 1)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_height' failed"
    )

    expect_error(
      {
        suppressWarnings(tm_g_ae_sub(
          label = "subgroups Plot",
          dataname = "ADAE",
          arm_var = arm_var_cs,
          group_var = group_var_cs,
          plot_width = "a"
        ), classes = "picks_delayed")
      },
      "Assertion on 'plot_width' failed"
    )
  })
})

describe("tm_g_ae_sub module creation", {
  it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(tm_g_ae_sub(
      label = "subgroups Plot",
      dataname = "ADAE",
      arm_var = arm_var_cs,
      group_var = group_var_cs
    ), classes = "picks_delayed")
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using picks", {
    mod <- suppressWarnings(tm_g_ae_sub(
      label = "subgroups Plot",
      dataname = "ADAE",
      arm_var = variables(
        choices = c("ACTARM", "ACTARMCD"),
        selected = "ACTARMCD"
      ),
      group_var = variables(
        choices = c("SEX", "REGION1", "RACE"),
        selected = c("SEX", "REGION1", "RACE"),
        multiple = TRUE
      ), classes = "picks_delayed"
    ))
    expect_s3_class(mod, "teal_module")
  })

  data <- within(teal_data(), {
    ADSL <- rADSL
    ADAE <- rADAE
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  it("using choices_selected works", {
    mod <- suppressWarnings(tm_g_ae_sub(
      label = "subgroups Plot",
      dataname = "ADAE",
      arm_var = arm_var_cs,
      group_var = group_var_cs
    ), classes = "picks_delayed")
    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          arm_ref = "ARM A",
          arm_trt = "ARM B",
          ci = "wald", conf_level = 0.95,
          fontsize = 3,
          arm_n = FALSE
        )
        expect_no_error(session$returned())
      }
    )
  })

  it("using picks works", {
    mod <- suppressWarnings(tm_g_ae_sub(
      label = "subgroups Plot",
      dataname = "ADAE",
      arm_var = variables(
        choices = c("ACTARM", "ACTARMCD"),
        selected = "ACTARMCD"
      ),
      group_var = variables(
        choices = c("SEX", "REGION1", "RACE"),
        selected = c("SEX", "REGION1", "RACE"),
        multiple = TRUE
      )
    ), classes = "picks_delayed")

    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          arm_ref = "ARM A",
          arm_trt = "ARM B",
          ci = "wald", conf_level = 0.95,
          fontsize = 3,
          arm_n = FALSE
        )
        expect_no_error(session$returned())
      }
    )
  })
})
