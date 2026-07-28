create_tm_g_events_term_id_data <- function() { # nolint: object_length_linter.
  data <- within(teal.data::teal_data(), {
    ADSL <- teal.data::rADSL
    ADAE <- teal.data::rADAE
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[names(data)]
  data
}

app_driver_tm_g_events_term_id <- function() {
  data <- create_tm_g_events_term_id_data()
  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_events_term_id(
        label = "Common AE",
        term_var = variables(
          choices = c("AEDECOD", "AETERM", "AEHLT"),
          selected = "AEDECOD"
        ),
        arm_var = variables(
          choices = c("ACTARMCD", "ACTARM"),
          selected = "ACTARMCD"
        ),
        dataname = "ADAE",
        parent_dataname = "ADSL"
      )
    )
  )
}

test_that("e2e - tm_g_events_term_id initializes and renders a plot", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_events_term_id()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  app_driver$expect_no_shiny_error()
  app_driver$expect_no_validation_error()
  expect_match(
    app_driver$get_active_module_plot_output("out"),
    "data:image/png;base64,"
  )
  expect_false(
    isTRUE(app_driver$get_active_module_input("arm_ref") == app_driver$get_active_module_input("arm_trt"))
  )
})

test_that(
  "e2e - tm_g_events_term_id starts with expected label and encoding selections.",
  {
    skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_events_term_id()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()

    expect_equal(
      app_driver$get_text("a.nav-link.active"),
      "Common AE"
    )
    expect_equal(get_teal_picks_slot(app_driver, "term_var", "datasets"), "ADAE")
    expect_equal(
      .teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "term_var", "variables")),
      "AEDECOD"
    )
    expect_equal(get_teal_picks_slot(app_driver, "arm_var", "datasets"), "ADSL")
    expect_equal(
      .teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "arm_var", "variables")),
      "ACTARMCD"
    )
  }
)

test_that(
  "e2e - tm_g_events_term_id: changing term_var changes the plot and does not throw validation errors.",
  {
    skip_if_not_installed("shinytest2")
    skip_if_too_deep(5)
    app_driver <- app_driver_tm_g_events_term_id()
    withr::defer(app_driver$stop())
    app_driver$wait_for_idle()
    plot_before <- app_driver$get_active_module_plot_output("out")
    set_teal_picks_slot(app_driver, "term_var", "variables", "AETERM")
    expect_equal(
      .teal_picks_strip_ds_prefix_vec(get_teal_picks_slot(app_driver, "term_var", "variables")),
      "AETERM"
    )
    expect_false(identical(plot_before, app_driver$get_active_module_plot_output("out")))
    app_driver$expect_no_validation_error()
  }
)

test_that("e2e - tm_g_events_term_id: deselection of term_var throws validation error.", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_events_term_id()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()
  set_teal_picks_slot(app_driver, "term_var", "variables", character(0L))
  app_driver$expect_validation_error()
})

test_that("e2e - tm_g_events_term_id sort updates title", {
  skip_if_not_installed("shinytest2")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_events_term_id()
  withr::defer(app_driver$stop())
  app_driver$wait_for_idle()

  app_driver$set_active_module_input("sort", "riskdiff")
  expect_identical(
    app_driver$get_active_module_input("title"),
    "Common AE Table Sorted by Risk Difference"
  )
  app_driver$expect_no_validation_error()
})
