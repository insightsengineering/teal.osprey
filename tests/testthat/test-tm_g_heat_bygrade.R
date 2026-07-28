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

id_var_picks <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "USUBJID"
), classes = "picks_delayed")

visit_var_picks <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "AVISIT"
), classes = "picks_delayed")

ongo_var_picks <- suppressWarnings(variables(
  choices = dplyr::where(is.logical),
  selected = "ongo_status"
), classes = "picks_delayed")

anno_var_picks_single <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "SEX"
), classes = "picks_delayed")

anno_var_picks_multiple <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = c("SEX", "COUNTRY"),
  multiple = TRUE
), classes = "picks_delayed")

heat_var_picks <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "AETOXGR"
), classes = "picks_delayed")

conmed_var_picks <- suppressWarnings(variables(
  choices = is_categorical(min.len = 2),
  selected = "CMDECOD"
), classes = "picks_delayed")

data <- teal_data() %>%
  within({
    library(dplyr)
    library(nestcolor)
    ADSL <- teal.data::rADSL %>% slice(1:30)
    ADEX <- teal.data::rADEX %>%
      filter(USUBJID %in% ADSL$USUBJID) %>%
      filter(PARCAT1 == "INDIVIDUAL") %>%
      mutate(ongo_status = (EOSSTT == "ONGOING"))
    ADAE <- teal.data::rADAE %>% filter(USUBJID %in% ADSL$USUBJID)
    ADCM <- teal.data::rADCM %>% filter(USUBJID %in% ADSL$USUBJID)
  })
join_keys(data) <- default_cdisc_join_keys[names(data)]

describe("tm_g_heat_bygrade argument verification", {
  it("plot arguments input validation", {
    expect_error(
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
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "Assertion on 'plot_height' failed"
    )

    expect_error(
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
          classes = c("picks_delayed", "lifecycle_warning_deprecated")
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })
})

describe("tm_g_heat_bygrade module creation", {
  it("creates a teal module using choices_selected", {
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
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using choices_selected with conmed", {
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
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using picks", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        id_var = id_var_picks,
        visit_var = visit_var_picks,
        ongo_var = ongo_var_picks,
        anno_var = anno_var_picks_multiple,
        heat_var = heat_var_picks,
        plot_height = c(600L, 200L, 2000L)
      ),
      classes = "picks_delayed"
    )
    expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using picks with conmed", {
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
        anno_var = anno_var_picks_multiple,
        heat_var = heat_var_picks,
        conmed_var = conmed_var_picks,
        plot_height = c(600L, 200L, 2000L)
      ),
      classes = "picks_delayed"
    )
    expect_s3_class(mod, "teal_module")
  })

  it("works with choices_selected", {
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
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )
    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          expect_no_error(session$returned())
        }
      ),
      regexp = "cartesian join"
    )
  })

  it("works with choices_selected with conmed", {
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
        conmed_var = conmed_var_cs
      ),
      classes = c("picks_delayed", "lifecycle_warning_deprecated")
    )

    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          expect_no_error(session$returned())
        }
      ),
      regexp = "cartesian join"
    )
  })

  it("works with picks", {
    mod <- suppressWarnings(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        id_var = id_var_picks,
        visit_var = visit_var_picks,
        ongo_var = ongo_var_picks,
        anno_var = anno_var_picks_multiple,
        heat_var = heat_var_picks
      ),
      classes = "picks_delayed"
    )

    # cartesian product warning: unclear what class is it
    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          expect_no_error(session$returned())
        }
      ),
      regexp = "cartesian join"
    )
  })

  it("works with picks with conmed", {
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
        anno_var = anno_var_picks_multiple,
        heat_var = heat_var_picks,
        conmed_var = conmed_var_picks
      ),
      classes = "picks_delayed"
    )

    expect_warning(
      testServer(
        mod$server,
        args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
        expr = {
          expect_no_error(session$returned())
        }
      ),
      regexp = "cartesian join"
    )
  })

  it("Throws warning when converting anno_var to multiple", {
    mod <- expect_warning(
      tm_g_heat_bygrade(
        label = "Heatmap by grade",
        sl_dataname = "ADSL",
        ex_dataname = "ADEX",
        ae_dataname = "ADAE",
        cm_dataname = "ADCM",
        id_var = id_var_picks,
        visit_var = visit_var_picks,
        ongo_var = ongo_var_picks,
        anno_var = anno_var_picks_single,
        heat_var = heat_var_picks,
        conmed_var = conmed_var_picks
      ),
      "accepts only a multiple variable selection"
    )
    expect_s3_class(mod, "teal_module")
    expect_true(is_pick_multiple(mod$server_args$anno_var$variables))
  })
})
