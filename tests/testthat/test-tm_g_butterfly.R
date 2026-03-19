right_var_cs <- teal.transform::choices_selected(
  selected = "SEX",
  choices = c("SEX", "ARM", "RACE")
)

left_var_cs <- teal.transform::choices_selected(
  selected = "RACE",
  choices = c("SEX", "ARM", "RACE")
)

category_var_cs <- teal.transform::choices_selected(
  selected = "AEBODSYS",
  choices = c("AEDECOD", "AEBODSYS")
)

color_by_var_cs <- teal.transform::choices_selected(
  selected = "AETOXGR",
  choices = c("AETOXGR", "None")
)

count_by_var_cs <- teal.transform::choices_selected(
  selected = "# of patients",
  choices = c("# of patients", "# of AEs")
)

right_var_picks <- teal.picks::variables(
  choices = dplyr::where(is.factor),
  selected = 1L
)

left_var_picks <- teal.picks::variables(
  choices = dplyr::where(is.factor),
  selected = 1L
)

testthat::describe("tm_g_butterfly argument verification", {
  testthat::it("fails when right_var is picks but left_var is choices_selected", {
    testthat::expect_error(
      tm_g_butterfly(
        label = "Butterfly Plot",
        dataname = "ADAE",
        right_var = right_var_picks,
        left_var = left_var_cs,
        category_var = teal.picks::variables(
          choices = teal.picks::is_categorical(min.len = 2),
          selected = 1L
        ),
        color_by_var = teal.picks::variables(
          choices = teal.picks::is_categorical(min.len = 2),
          selected = 1L
        ),
        count_by_var = teal.picks::values(
          selected = "# of patients",
          choices = c("# of patients", "# of AEs")
        ),
        plot_height = c(600, 200, 2000)
      ),
      regexp = "Assertion on 'picks' failed"
    )
  })

  testthat::it("fails when right_var is choices_selected but left_var is picks", {
    testthat::expect_error(
      tm_g_butterfly(
        label = "Butterfly Plot",
        dataname = "ADAE",
        right_var = right_var_cs,
        left_var = left_var_picks,
        category_var = category_var_cs,
        color_by_var = color_by_var_cs,
        count_by_var = count_by_var_cs,
        plot_height = c(600, 200, 2000)
      ),
      regexp = "Assertion on 'left_var' failed:"
    )
  })
})

testthat::describe("tm_g_butterfly module creation", {
  testthat::it("creates a teal module using choices_selected (default method)", {
    mod <- tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = right_var_cs,
      left_var = left_var_cs,
      category_var = category_var_cs,
      color_by_var = color_by_var_cs,
      count_by_var = count_by_var_cs,
      plot_height = c(600, 200, 2000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks (.pick method)", {
    mod <- tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = right_var_picks,
      left_var = left_var_picks,
      category_var = teal.picks::variables(
        choices = teal.picks::is_categorical(min.len = 2),
        selected = 1L
      ),
      color_by_var = teal.picks::variables(
        choices = teal.picks::is_categorical(min.len = 2),
        selected = 1L
      ),
      count_by_var = teal.picks::values(
        selected = "# of patients",
        choices = c("# of patients", "# of AEs")
      ),
      sort_by_var = teal.picks::values(
        selected = "count",
        choices = c("count", "alphabetical")
      ),
      plot_height = c(600, 200, 2000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
