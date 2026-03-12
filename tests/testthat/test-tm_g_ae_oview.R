arm_var_cs <- teal.transform::choices_selected(
  selected = "ACTARM",
  choices = c("ACTARM", "ACTARMCD")
)

flag_var_cs <- teal.transform::choices_selected(
  selected = "TMPFL_SER",
  choices = c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5")
)

arm_var_picks <- teal.picks::picks(
  teal.picks::datasets("ADAE"),
  teal.picks::variables(
    choices = teal.picks::is_categorical(min.len = 2),
    selected = 1L
  )
)

flag_var_picks <- teal.picks::picks(
  teal.picks::datasets("ADAE"),
  teal.picks::variables(
    choices = c("TMPFL_SER", "TMPFL_REL", "TMPFL_GR5"),
    selected = "TMPFL_SER"
  )
)

testthat::describe("tm_g_ae_oview argument verification", {
  testthat::it("fails when arm_var is picks but flag_var_anl is choices_selected", {
    testthat::expect_error(
      tm_g_ae_oview(
        label = "AE Overview",
        arm_var = arm_var_picks,
        flag_var_anl = flag_var_cs
      ),
      class = "error"
    )
  })

  testthat::it("fails when arm_var is choices_selected but flag_var_anl is picks", {
    testthat::expect_error(
      tm_g_ae_oview(
        label = "AE Overview",
        dataname = "ADAE",
        arm_var = arm_var_cs,
        flag_var_anl = flag_var_picks
      ),
      class = "error"
    )
  })
})

testthat::describe("tm_g_ae_oview module creation", {
  testthat::it("creates a teal module using choices_selected (default method)", {
    mod <- tm_g_ae_oview(
      label = "AE Overview",
      dataname = "ADAE",
      arm_var = arm_var_cs,
      flag_var_anl = flag_var_cs,
      plot_height = c(600, 200, 2000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks (.picks method)", {
    mod <- tm_g_ae_oview(
      label = "AE Overview",
      arm_var = arm_var_picks,
      flag_var_anl = flag_var_picks,
      plot_height = c(600, 200, 2000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
