paramcd_cs <- teal.transform::choices_selected(
  choices = "SLDINV",
  selected = "SLDINV"
)

x_var_cs <- teal.transform::choices_selected(
  choices = "ADY",
  selected = "ADY"
)

y_var_cs <- teal.transform::choices_selected(
  choices = c("PCHG", "CHG", "AVAL"),
  selected = "PCHG"
)

marker_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "RACE", "USUBJID"),
  selected = "SEX"
)

line_colorby_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "USUBJID", "RACE"),
  selected = "SEX"
)

xfacet_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "ARM"),
  selected = "SEX",
)

yfacet_var_cs <- teal.transform::choices_selected(
  choices = c("SEX", "ARM"),
  selected = "ARM",
)

paramcd_picks <- teal.picks::variables(
  choices = "PARAMCD",
  selected = "PARAMCD"
)

x_var_picks <- teal.picks::variables(
  choices = c("ADY", "AGE"),
  selected = "ADY"
)

y_var_picks <- teal.picks::variables(
  choices = c("PCHG", "CHG", "AVAL"),
  selected = "PCHG"
)

marker_var_picks <- teal.picks::variables(
  choices = c("SEX", "RACE", "USUBJID"),
  selected = "SEX"
)

line_colorby_var_picks <- teal.picks::variables(
  choices = c("SEX", "USUBJID", "RACE"),
  selected = "SEX"
)

xfacet_var_picks <- teal.picks::variables(
  choices = c("SEX", "ARM"),
  selected = "SEX"
)

yfacet_var_picks <- teal.picks::variables(
  choices = c("SEX", "ARM"),
  selected = "ARM"
)

testthat::describe("tm_g_spiderplot argument verification", {
  testthat::it("plot arguments input validation", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_spiderplot(
            label = "Spider Plot",
            dataname = "ADTR",
            paramcd = paramcd_cs,
            x_var = x_var_cs,
            y_var = y_var_cs,
            marker_var = marker_var_cs,
            line_colorby_var = line_colorby_var_cs,
            xfacet_var = xfacet_var_cs,
            yfacet_var = yfacet_var_cs,
            vref_line = "10, 37",
            href_line = "-20, 0",
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
          tm_g_spiderplot(
            label = "Spider Plot",
            dataname = "ADTR",
            paramcd = paramcd_cs,
            x_var = x_var_cs,
            y_var = y_var_cs,
            marker_var = marker_var_cs,
            line_colorby_var = line_colorby_var_cs,
            xfacet_var = xfacet_var_cs,
            yfacet_var = yfacet_var_cs,
            vref_line = "10, 37",
            href_line = "-20, 0",
            plot_width = c(600, 2000, 200)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })

  testthat::it("Forcing Conversion from multiple picks to single", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_spiderplot(
            label = "Spider Plot",
            dataname = "ADTR",
            paramcd = paramcd_cs,
            x_var = teal.picks::variables(
              choices = c("ADY", "AGE"),
              selected = "ADY",
              multiple = TRUE
            ),
            y_var = y_var_cs,
            marker_var = marker_var_cs,
            line_colorby_var = line_colorby_var_cs,
            xfacet_var = xfacet_var_cs,
            yfacet_var = yfacet_var_cs,
            vref_line = "10, 37",
            href_line = "-20, 0"
          ),
          classes = "picks_delayed"
        )
      },
      "metadata does not match the requirement for x_var"
    )

    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_spiderplot(
            label = "Spider Plot",
            dataname = "ADTR",
            paramcd = teal.picks::variables(
              choices = "PARAMCD",
              selected = "PARAMCD",
              multiple = TRUE
            ),
            x_var = x_var_picks,
            y_var = y_var_picks,
            marker_var = marker_var_picks,
            line_colorby_var = line_colorby_var_picks,
            xfacet_var = xfacet_var_picks,
            yfacet_var = yfacet_var_picks,
            vref_line = "10, 37",
            href_line = "-20, 0"
          ),
          classes = "picks_delayed"
        )
      },
      "metadata does not match the requirement for paramcd"
    )
  })
})

testthat::describe("tm_g_spiderplot module creation", {
  testthat::it("creates a teal module using choices_selected (default method)", {
    mod <- suppressWarnings(
      tm_g_spiderplot(
        label = "Spider Plot",
        dataname = "ADTR",
        paramcd = paramcd_cs,
        x_var = x_var_cs,
        y_var = y_var_cs,
        marker_var = marker_var_cs,
        line_colorby_var = line_colorby_var_cs,
        xfacet_var = xfacet_var_cs,
        yfacet_var = yfacet_var_cs,
        vref_line = "10, 37",
        href_line = "-20, 0",
        plot_height = c(600, 200, 2000)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  testthat::it("creates a teal module using picks (.pick method)", {
    mod <- suppressWarnings(
      tm_g_spiderplot(
        label = "Spider Plot",
        dataname = "ADTR",
        paramcd = paramcd_picks,
        x_var = x_var_picks,
        y_var = y_var_picks,
        marker_var = marker_var_picks,
        line_colorby_var = line_colorby_var_picks,
        xfacet_var = xfacet_var_picks,
        yfacet_var = yfacet_var_picks,
        vref_line = "10, 37",
        href_line = "-20, 0",
        plot_height = c(600, 200, 2000)
      ),
      classes = "picks_delayed"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
