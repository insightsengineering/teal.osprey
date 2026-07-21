bar_paramcd_cs <- teal.transform::choices_selected(
  choices = "SLDINV",
  selected = "SLDINV"
)

bar_var_cs <- teal.transform::choices_selected(
  choices = c("PCHG", "AVAL"),
  selected = "PCHG"
)

bar_color_var_cs <- teal.transform::choices_selected(
  choices = c("ARMCD", "SEX"),
  selected = "ARMCD"
)

sort_var_cs <- teal.transform::choices_selected(
  choices = c("ARMCD", "SEX"),
  selected = NULL
)

add_label_var_sl_cs <- teal.transform::choices_selected(
  choices = c("SEX", "EOSDY"),
  selected = NULL
)

add_label_paramcd_rs_cs <- teal.transform::choices_selected(
  choices = c("BESRSPI", "OBJRSPI"),
  selected = NULL
)

anno_txt_var_sl_cs <- teal.transform::choices_selected(
  choices = c("SEX", "ARMCD", "BMK1", "BMK2"),
  selected = c("SEX", "ARMCD")
)

anno_txt_paramcd_rs_cs <- teal.transform::choices_selected(
  choices = c("BESRSPI", "OBJRSPI"),
  selected = c("BESRSPI", "OBJRSPI")
)

facet_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "ARMCD", "STRATA1", "STRATA2"),
  selected = NULL
)

bar_paramcd_picks <- teal.picks::values(
  choices = "SLDINV",
  selected = "SLDINV",
  multiple = FALSE
)

bar_var_picks <- teal.picks::variables(
  choices = c("PCHG", "AVAL"),
  selected = "PCHG"
)

bar_color_var_picks <- teal.picks::variables(
  choices = c("ARMCD", "SEX"),
  selected = "ARMCD"
)

sort_var_picks <- teal.picks::variables(
  choices = c("ARMCD", "SEX"),
  selected = NULL
)

add_label_var_sl_picks <- teal.picks::variables(
  choices = c("SEX", "EOSDY"),
  selected = NULL
)

add_label_paramcd_rs_picks <- teal.picks::values(
  choices = c("BESRSPI", "OBJRSPI"),
  selected = NULL,
  multiple = FALSE
)

anno_txt_var_sl_picks <- teal.picks::variables(
  choices = c("SEX", "ARMCD", "BMK1", "BMK2"),
  selected = NULL,
  multiple = TRUE
)

anno_txt_paramcd_rs_picks <- teal.picks::values(
  choices = c("BESRSPI", "OBJRSPI"),
  selected = NULL
)

facet_var_picks <- teal.picks::variables(
  choices = c("SEX", "ARMCD", "STRATA1", "STRATA2"),
  selected = NULL
)

testthat::describe("tm_g_waterfall argument verification", {
  testthat::it("plot arguments input validation", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_waterfall(
            label = "Waterfall",
            dataname_tr = "ADTR",
            dataname_rs = "ADRS",
            bar_paramcd = bar_paramcd_cs,
            bar_var = bar_var_cs,
            bar_color_var = bar_color_var_cs,
            bar_color_opt = NULL,
            sort_var = sort_var_cs,
            add_label_var_sl = add_label_var_sl_cs,
            add_label_paramcd_rs = add_label_paramcd_rs_cs,
            anno_txt_var_sl = anno_txt_var_sl_cs,
            anno_txt_paramcd_rs = anno_txt_paramcd_rs_cs,
            facet_var = facet_var_cs,
            href_line = "-30, 20",
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
          tm_g_waterfall(
            label = "Waterfall",
            dataname_tr = "ADTR",
            dataname_rs = "ADRS",
            bar_paramcd = bar_paramcd_cs,
            bar_var = bar_var_cs,
            bar_color_var = bar_color_var_cs,
            bar_color_opt = NULL,
            sort_var = sort_var_cs,
            add_label_var_sl = add_label_var_sl_cs,
            add_label_paramcd_rs = add_label_paramcd_rs_cs,
            anno_txt_var_sl = anno_txt_var_sl_cs,
            anno_txt_paramcd_rs = anno_txt_paramcd_rs_cs,
            facet_var = facet_var_cs,
            href_line = "-30, 20",
            plot_width = c(600, 2000, 200)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })

  testthat::it("Forcing Conversion from multiple picks to single", {
    testthat::expect_warning(
      {
        suppressWarnings(
          tm_g_waterfall(
            label = "Waterfall",
            dataname_tr = "ADTR",
            dataname_rs = "ADRS",
            bar_paramcd = bar_paramcd_cs,
            bar_var = teal.picks::variables(
              choices = c("PCHG", "AVAL"),
              selected = "PCHG",
              multiple = TRUE
            ),
            bar_color_var = bar_color_var_cs,
            bar_color_opt = NULL,
            sort_var = sort_var_cs,
            add_label_var_sl = add_label_var_sl_cs,
            add_label_paramcd_rs = add_label_paramcd_rs_cs,
            anno_txt_var_sl = anno_txt_var_sl_cs,
            anno_txt_paramcd_rs = anno_txt_paramcd_rs_cs,
            facet_var = facet_var_cs,
            href_line = "-30, 20"
          ),
          classes = "picks_delayed"
        )
      },
      "`bar_var` accepts only a single variable selection"
    )

    testthat::expect_warning(
      {
        suppressWarnings(
          tm_g_waterfall(
            label = "Waterfall",
            dataname_tr = "ADTR",
            dataname_rs = "ADRS",
            bar_paramcd = teal.picks::values(
              choices = "SLDINV",
              selected = "SLDINV",
              multiple = TRUE
            ),
            bar_var = bar_var_picks,
            bar_color_var = bar_color_var_picks,
            bar_color_opt = NULL,
            sort_var = sort_var_picks,
            add_label_var_sl = add_label_var_sl_picks,
            add_label_paramcd_rs = add_label_paramcd_rs_picks,
            anno_txt_var_sl = anno_txt_var_sl_picks,
            anno_txt_paramcd_rs = anno_txt_paramcd_rs_picks,
            facet_var = facet_var_picks,
            href_line = "-30, 20"
          ),
          classes = "picks_delayed"
        )
      },
      "`bar_paramcd` accepts only a single variable selection"
    )
  })
})

testthat::describe("tm_g_waterfall module creation", {
  testthat::it("creates a teal module using choices_selected (default method)", {
    mod <- tm_g_waterfall(
      label = "Waterfall",
      dataname_tr = "ADTR",
      dataname_rs = "ADRS",
      bar_paramcd = bar_paramcd_cs,
      bar_var = bar_var_cs,
      bar_color_var = bar_color_var_cs,
      bar_color_opt = NULL,
      sort_var = sort_var_cs,
      add_label_var_sl = add_label_var_sl_cs,
      add_label_paramcd_rs = add_label_paramcd_rs_cs,
      anno_txt_var_sl = anno_txt_var_sl_cs,
      anno_txt_paramcd_rs = anno_txt_paramcd_rs_cs,
      facet_var = facet_var_cs,
      href_line = "-30, 20",
      plot_height = c(1200, 400, 5000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks (.pick method)", {
    mod <- tm_g_waterfall(
      label = "Waterfall",
      dataname_tr = "ADTR",
      dataname_rs = "ADRS",
      bar_paramcd = bar_paramcd_picks,
      bar_var = bar_var_picks,
      bar_color_var = bar_color_var_picks,
      bar_color_opt = NULL,
      sort_var = sort_var_picks,
      add_label_var_sl = add_label_var_sl_picks,
      add_label_paramcd_rs = add_label_paramcd_rs_picks,
      anno_txt_var_sl = anno_txt_var_sl_picks,
      anno_txt_paramcd_rs = anno_txt_paramcd_rs_picks,
      facet_var = facet_var_picks,
      href_line = "-30, 20",
      plot_height = c(1200, 400, 5000)
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
