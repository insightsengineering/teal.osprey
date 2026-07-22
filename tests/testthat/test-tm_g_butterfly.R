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
  selected = "SEX"
) |>
  suppressWarnings(classes = "picks_delayed")

left_var_picks <- teal.picks::variables(
  choices = dplyr::where(is.factor),
  selected = "RACE"
) |>
  suppressWarnings(classes = "picks_delayed")

testthat::describe("tm_g_butterfly input validation", {
  it("plot arguments input validation", {
    testthat::expect_error(
      {
        suppressWarnings(
          tm_g_butterfly(
            label = "Butterfly Plot",
            dataname = "ADAE",
            right_var = right_var_cs,
            left_var = left_var_cs,
            category_var = category_var_cs,
            color_by_var = color_by_var_cs,
            count_by_var = count_by_var_cs,
            plot_height = c(600, 2000, 200)
          ),
          classes = "picks_delayed"
        )
      },
      "Assertion on 'plot_height' failed"
    )

    testthat::expect_error(
      {
        suppressWarnings(tm_g_butterfly(
          label = "Butterfly Plot",
          dataname = "ADAE",
          right_var = right_var_cs,
          left_var = left_var_cs,
          category_var = category_var_cs,
          color_by_var = color_by_var_cs,
          count_by_var = count_by_var_cs,
          plot_width = c(600, 2000, 200)
        ), classes = "picks_delayed")
      },
      "Assertion on 'plot_width' failed"
    )
  })

  it("Forcing Conversion from multiple picks to single", {
    testthat::expect_error(
      {
        suppressWarnings(tm_g_butterfly(
          label = "Butterfly Plot",
          dataname = "ADAE",
          right_var = right_var_cs,
          left_var = teal.picks::variables(
            choices = dplyr::where(is.factor),
            selected = 1L,
            multiple = TRUE
          ),
          category_var = category_var_cs,
          color_by_var = color_by_var_cs,
          count_by_var = count_by_var_cs
        ), classes = "picks_delayed")
      },
      "metadata does not match the requirement for left_var"
    )

    testthat::expect_error(
      {
        suppressWarnings(tm_g_butterfly(
          label = "Butterfly Plot",
          dataname = "ADAE",
          right_var = teal.picks::variables(
            choices = dplyr::where(is.factor),
            selected = 1L,
            multiple = TRUE
          ),
          left_var = left_var_picks,
          category_var = category_var_cs,
          color_by_var = color_by_var_cs,
          count_by_var = count_by_var_cs
        ), classes = "picks_delayed")
      },
      "metadata does not match the requirement for right_var"
    )
  })
})

testthat::describe("tm_g_butterfly module creation", {
  it("creates a teal module using choices_selected", {
    mod <- tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = right_var_cs,
      left_var = left_var_cs,
      category_var = category_var_cs,
      color_by_var = color_by_var_cs,
      count_by_var = count_by_var_cs,
      plot_height = c(600, 200, 2000)
    ) |>
      suppressWarnings(classes = "picks_delayed")
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("creates a teal module using picks", {
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
    ) |>
      suppressWarnings(classes = "picks_delayed")
    testthat::expect_s3_class(mod, "teal_module")
  })


  data <- teal_data() %>%
    eval_code("set.seed(23) # @linksto ADSL") %>%
    within({
      library(nestcolor)
      library(dplyr)
      ADSL <- rADSL
      ADAE <- rADAE
      ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
      ADAE <- mutate(
        ADAE,
        flag1 = ifelse(AETOXGR == 1, 1, 0),
        flag2 = ifelse(AETOXGR == 2, 1, 0),
        flag3 = ifelse(AETOXGR == 3, 1, 0),
        flag1_filt = rep("Y", n())
      )
    })

  join_keys(data) <- default_cdisc_join_keys[names(data)]

  it("works with choices_selected", {
    mod <- tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = right_var_cs,
      left_var = left_var_cs,
      category_var = category_var_cs,
      color_by_var = color_by_var_cs,
      count_by_var = count_by_var_cs
    ) |>
      suppressWarnings(classes = "picks_delayed")

    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          right_val = "F", left_val = "ASIAN", legend_on = TRUE, count_by_var = "# of patients"
        )
        expect_no_error(session$returned())
      }
    )

  })

  it("works with picks", {
    mod <- tm_g_butterfly(
      label = "Butterfly Plot",
      dataname = "ADAE",
      right_var = right_var_picks,
      left_var = left_var_picks,
      category_var = teal.picks::variables(
        choices = teal.picks::is_categorical(min.len = 2),
        selected = "AEBODSYS"
      ),
      color_by_var = teal.picks::variables(
        choices = teal.picks::is_categorical(min.len = 2),
        selected = "AETOXGR"
      ),
      count_by_var = teal.picks::values(
        selected = "# of patients",
        choices = c("# of patients", "# of AEs")
      ),
      sort_by_var = teal.picks::values(
        selected = "count",
        choices = c("count", "alphabetical")
      )
    ) |>
      suppressWarnings(classes = "picks_delayed")
    testServer(
      mod$server,
      args = c(list(id = "test_id", data = shiny::reactive(data)), mod$server_args),
      expr = {
        session$setInputs(
          right_val = "F", left_val = "ASIAN", legend_on = TRUE, count_by_var = "# of patients"
        )
        expect_no_error(session$returned())
      }
    )
  })
})
