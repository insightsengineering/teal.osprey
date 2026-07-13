id_var_cs <- teal.transform::choices_selected(
  choices = c("USUBJID", "SUBJID"),
  selected = "USUBJID"
)

visit_var_cs <- teal.transform::choices_selected(
  choices = "AVISIT",
  selected = "AVISIT"
)

ongo_var_cs <- teal.transform::choices_selected(
  choices = "ongo_status",
  selected = "ongo_status"
)

anno_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "COUNTRY", "USUBJID"),
  selected = c("SEX", "COUNTRY")
)

heat_var_cs <- teal.transform::choices_selected(
  choices = "AETOXGR",
  selected = "AETOXGR"
)

conmed_var_cs <- teal.transform::choices_selected(
  choices = "CMDECOD",
  selected = "CMDECOD"
)

id_var_picks <- teal.picks::variables(
  choices = teal.picks::is_categorical(min.len = 2),
  selected = 1L
)

visit_var_picks <- teal.picks::variables(
  choices = teal.picks::is_categorical(min.len = 2),
  selected = 1L
)

ongo_var_picks <- teal.picks::variables(
  choices = dplyr::where(is.logical),
  selected = 1L
)

anno_var_picks <- teal.picks::variables(
  choices = teal.picks::is_categorical(min.len = 2),
  selected = 1L
)

heat_var_picks <- teal.picks::variables(
  choices = teal.picks::is_categorical(min.len = 2),
  selected = 1L
)

conmed_var_picks <- teal.picks::variables(
  choices = teal.picks::is_categorical(min.len = 2),
  selected = 1L
)

testthat::describe("tm_g_heat_bygrade argument verification", {
  testthat::it("plot arguments input validation", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_heat_bygrade(
            label = "Heatmap by grade",
            sl_dataname = "ADSL",
            ex_dataname = "ADEX",
            ae_dataname = "ADAE",
            id_var = id_var_cs,
            visit_var = visit_var_cs,
            ongo_var = ongo_var_cs,
            anno_var = anno_var_cs,
            heat_var = heat_var_cs,
            plot_height = c(600, 2000, 200)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_height' failed"
    )

    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_heat_bygrade(
            label = "Heatmap by grade",
            sl_dataname = "ADSL",
            ex_dataname = "ADEX",
            ae_dataname = "ADAE",
            id_var = id_var_cs,
            visit_var = visit_var_cs,
            ongo_var = ongo_var_cs,
            anno_var = anno_var_cs,
            heat_var = heat_var_cs,
            plot_width = c(600, 2000, 200)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })
})

testthat::describe("tm_g_heat_bygrade module creation", {
  testthat::it("creates a teal module using choices_selected", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        id_var = id_var_cs,
        visit_var = visit_var_cs,
        ongo_var = ongo_var_cs,
        anno_var = anno_var_cs,
        heat_var = heat_var_cs,
        plot_height = c(600, 200, 2000)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using choices_selected with conmed", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        cm_dataname = "ADCM",
        id_var = id_var_cs,
        visit_var = visit_var_cs,
        ongo_var = ongo_var_cs,
        anno_var = anno_var_cs,
        heat_var = heat_var_cs,
        conmed_var = conmed_var_cs,
        plot_height = c(600, 200, 2000)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        id_var = id_var_picks,
        visit_var = visit_var_picks,
        ongo_var = ongo_var_picks,
        anno_var = anno_var_picks,
        heat_var = heat_var_picks,
        plot_height = c(600L, 200L, 2000L)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks with conmed (.pick method)", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        cm_dataname = "ADCM",
        id_var = id_var_picks,
        visit_var = visit_var_picks,
        ongo_var = ongo_var_picks,
        anno_var = anno_var_picks,
        heat_var = heat_var_picks,
        conmed_var = conmed_var_picks,
        plot_height = c(600L, 200L, 2000L)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
