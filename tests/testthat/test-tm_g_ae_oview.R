arm_var_cs <- teal.transform::choices_selected(
  selected = "ACTARM",
  choices = c("ACTARM", "ACTARMCD")
)

flag_var_cs <- teal.transform::choices_selected(
  selected = "TMPFL_SER",
  choices = c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5")
)

arm_var_picks <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "ACTARM"
), classes = "picks_delayed")

flag_var_picks <- variables(
  choices = c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5"),
  selected = "TMPFL_SER"
)

describe("tm_g_ae_oview argument verification", {
  it("fails when arm_var is neither picks or choices selected", {
    expect_error(
      tm_g_ae_oview(
        label = "AE Overview",
        dataname = "ADAE",
        arm_var = list(),
        flag_var_anl = flag_var_cs
      ),
      class = "error"
    )
  })

  it("fails when flag_var_anl is neiter picks or choices_selected", {
    expect_error(
      tm_g_ae_oview(
        label = "AE Overview",
        dataname = "ADAE",
        arm_var = arm_var_cs,
        flag_var_anl = list()
      ),
      class = "error"
    )
  })
})

describe("tm_g_ae_oview module creation", {
  skip_if_not_installed("dplyr")
  data <- within(teal_data(), {
    library(dplyr)
    ADSL <- rADSL
    ADAE <- rADAE
    .add_event_flags <- function(dat) {
      dat <- dat %>%
        mutate(
          TMPFL_SER = AESER == "Y",
          TMPFL_REL = AEREL == "Y",
          TMPFL_GR5 = AETOXGR == "5",
          AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
          AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")
        )
      labels <- c(
        "Serious AE", "Related AE", "Grade 5 AE",
        "AE related to A: Drug X", "AE related to B: Placebo"
      )
      cols <- c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5", "AEREL1", "AEREL2")
      for (i in seq_along(labels)) {
        attr(dat[[cols[i]]], "label") <- labels[i]
      }
      dat
    }
    ADAE <- .add_event_flags(ADAE)
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = arm_var_cs,
      flag_var_anl = flag_var_cs,
      plot_height = c(600, 200, 2000)
    ), classes = "picks_delayed")
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using picks", {
    mod <- suppressWarnings(tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = arm_var_picks,
      flag_var_anl = flag_var_picks,
      plot_height = c(600, 200, 2000)
    ), classes = "picks_delayed")
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = arm_var_cs,
      flag_var_anl = flag_var_cs
    ), classes = "picks_delayed")
    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          fontsize = 5, conf_level = 0.95, diff_ci_method = "wald",
          arm_ref = "A: Drug X", arm_trt = "B: Placebo"
        )
        expect_no_error(session$returned())
      }
    )
  })

  it("creates a teal module using picks", {
    mod <- suppressWarnings(tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = arm_var_picks,
      flag_var_anl = flag_var_picks,
      plot_height = c(600, 200, 2000)
    ), classes = "picks_delayed")

    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          fontsize = 5, conf_level = 0.95, diff_ci_method = "wald",
          arm_ref = "A: Drug X", arm_trt = "B: Placebo"
        )
        expect_no_error(session$returned())
      }
    )
  })
})
