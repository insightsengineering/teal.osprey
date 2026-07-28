bar_var_picks <- teal.picks::variables(choices = "TRTDURD", selected = "TRTDURD")
bar_color_var_picks <- teal.picks::variables(choices = "EOSSTT", selected = "EOSSTT")
marker_pos_var_picks <- teal.picks::variables(choices = "ADY", selected = "ADY")
marker_shape_var_picks <- teal.picks::variables(choices = "AVALC", selected = "AVALC")
marker_color_var_picks <- teal.picks::variables(choices = "AVALC", selected = "AVALC")
anno_txt_var_picks <- teal.picks::variables(
  choices = c("ACTARM", "SEX"),
  selected = "ACTARM",
  multiple = TRUE
)
dataname <- "ADRS"
marker_shape_opt <- c(CR = 16)
marker_color_opt <- c(CR = "green")

bar_var_cs <- teal.transform::choices_selected(choices = "TRTDURD", selected = "TRTDURD")
bar_color_var_cs <- teal.transform::choices_selected(choices = "EOSSTT", selected = "EOSSTT")
marker_pos_var_cs <- teal.transform::choices_selected(choices = "ADY", selected = "ADY")
marker_shape_var_cs <- teal.transform::choices_selected(choices = "AVALC", selected = "AVALC")
marker_color_var_cs <- teal.transform::choices_selected(choices = "AVALC", selected = "AVALC")
anno_txt_var_cs <- teal.transform::choices_selected(
  choices = c("ACTARM", "SEX"),
  selected = "ACTARM"
)

describe("tm_swimlane input validation", {
  it("plot arguments input validation", {
    expect_error(
      {
        mod <- tm_g_swimlane(
          label = "Test - Swimlane - Plot Height",
          dataname = dataname,
          bar_var = bar_var_picks,
          bar_color_var = bar_color_var_picks,
          marker_pos_var = marker_pos_var_picks,
          marker_shape_var = marker_shape_var_picks,
          marker_shape_opt = marker_shape_opt,
          marker_color_var = marker_color_var_picks,
          marker_color_opt = marker_color_opt,
          anno_txt_var = anno_txt_var_picks,
          plot_height = c(600, 2000, 200)
        )
      },
      "Assertion on 'plot_height' failed"
    )

    expect_error(
      {
        mod <- tm_g_swimlane(
          label = "Test - Swimlane - Plot Width",
          dataname = dataname,
          bar_var = bar_var_picks,
          bar_color_var = bar_color_var_picks,
          marker_pos_var = marker_pos_var_picks,
          marker_shape_var = marker_shape_var_picks,
          marker_shape_opt = marker_shape_opt,
          marker_color_var = marker_color_var_picks,
          marker_color_opt = marker_color_opt,
          anno_txt_var = anno_txt_var_picks,
          plot_width = c(600, 2000, 200)
        )
      },
      "Assertion on 'plot_width' failed"
    )
  })

  it("fails if bar_var is not the expected class", {
    expect_error(
      {
        mod <- tm_g_swimlane(
          label = "Test - Swimlane - Plot Height",
          dataname = dataname,
          bar_var = list(),
          bar_color_var = bar_color_var_picks,
          marker_pos_var = marker_pos_var_picks,
          marker_shape_var = marker_shape_var_picks,
          marker_shape_opt = marker_shape_opt,
          marker_color_var = marker_color_var_picks,
          marker_color_opt = marker_color_opt,
          anno_txt_var = anno_txt_var_picks
        )
      },
      "Assertion on 'bar_var' failed"
    )
  })
})

describe("tm_g_swimlane module creation", {
  it("is correctly created using teal.picks", {
    mod <- tm_g_swimlane(
      label = "Test - Swimlane",
      dataname = dataname,
      bar_var = bar_var_picks,
      bar_color_var = bar_color_var_picks,
      marker_pos_var = marker_pos_var_picks,
      marker_shape_var = marker_shape_var_picks,
      marker_shape_opt = marker_shape_opt,
      marker_color_var = marker_color_var_picks,
      marker_color_opt = marker_color_opt,
      anno_txt_var = anno_txt_var_picks
    )
    expect_s3_class(mod, "teal_module")
    expect_identical(mod$server, srv_g_swimlane)
  })

  it("accepts (optionally) NULL argument as module argument", {
    mod <- tm_g_swimlane(
      label = "Test - Swimlane",
      dataname = dataname,
      bar_var = bar_var_picks,
      bar_color_var = bar_color_var_picks,
      marker_pos_var = NULL,
      marker_shape_var = marker_shape_var_picks,
      marker_shape_opt = marker_shape_opt,
      marker_color_var = marker_color_var_picks,
      marker_color_opt = marker_color_opt,
      anno_txt_var = anno_txt_var_picks
    )
    expect_s3_class(mod, "teal_module")
    expect_identical(mod$server, srv_g_swimlane)
  })

  it("is correctly created using choices_selected", {
    suppressWarnings({
      mod <- tm_g_swimlane(
        label = "Test - Swimlane",
        dataname = dataname,
        bar_var = bar_var_cs,
        bar_color_var = bar_color_var_cs,
        marker_pos_var = marker_pos_var_cs,
        marker_shape_var = marker_shape_var_cs,
        marker_shape_opt = marker_shape_opt,
        marker_color_var = marker_color_var_cs,
        marker_color_opt = marker_color_opt,
        anno_txt_var = anno_txt_var_cs
      )
    })

    expect_s3_class(mod, "teal_module")
    expect_identical(mod$server, srv_g_swimlane)
  })
})

testthat::describe("test for server module function", {
  data <- within(teal_data(), {
    library(nestcolor)
    library(dplyr)
    ADSL <- rADSL %>%
      mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1) %>%
      filter(STRATA1 == "A" & ARMCD == "ARM A")
    ADRS <- rADRS %>%
      filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
      mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
      rbind(filter(rADRS, PARAMCD == "OVRINV" & AVALC != "NE")) %>%
      arrange(USUBJID)
  })

  join_keys(data) <- default_cdisc_join_keys[names(data)]


  it("works using teal.picks", {
    mod <- tm_g_swimlane(
      label = "Test - Swimlane",
      dataname = dataname,
      bar_var = bar_var_picks,
      bar_color_var = bar_color_var_picks,
      marker_pos_var = marker_pos_var_picks,
      marker_shape_var = marker_shape_var_picks,
      marker_shape_opt = marker_shape_opt,
      marker_color_var = marker_color_var_picks,
      marker_color_opt = marker_color_opt,
      anno_txt_var = anno_txt_var_picks
    )
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

  it("works with (optionally) NULL argument as module argument", {
    mod <- tm_g_swimlane(
      label = "Test - Swimlane",
      dataname = dataname,
      bar_var = bar_var_picks,
      bar_color_var = bar_color_var_picks,
      marker_pos_var = NULL,
      marker_shape_var = marker_shape_var_picks,
      marker_shape_opt = marker_shape_opt,
      marker_color_var = marker_color_var_picks,
      marker_color_opt = marker_color_opt,
      anno_txt_var = anno_txt_var_picks
    )
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

  it("works using choices_selected", {
    suppressWarnings({
      mod <- tm_g_swimlane(
        label = "Test - Swimlane",
        dataname = dataname,
        bar_var = bar_var_cs,
        bar_color_var = bar_color_var_cs,
        marker_pos_var = marker_pos_var_cs,
        marker_shape_var = marker_shape_var_cs,
        marker_shape_opt = marker_shape_opt,
        marker_color_var = marker_color_var_cs,
        marker_color_opt = marker_color_opt,
        anno_txt_var = anno_txt_var_cs
      )
    })

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
