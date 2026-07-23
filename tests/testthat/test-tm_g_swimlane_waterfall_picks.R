testthat::test_that("tm_g_swimlane builds a teal module with picks encodings", {
  testthat::skip_if_not_installed("teal.picks")

  mod <- tm_g_swimlane(
    label = "Swimlane",
    dataname = "ADRS",
    bar_var = teal.picks::picks(
      teal.picks::datasets("ADSL"),
      teal.picks::variables(choices = "TRTDURD", selected = "TRTDURD")
    ),
    bar_color_var = teal.picks::picks(
      teal.picks::datasets("ADSL"),
      teal.picks::variables(choices = "EOSSTT", selected = "EOSSTT")
    ),
    marker_pos_var = teal.picks::picks(
      teal.picks::datasets("ADRS"),
      teal.picks::variables(choices = "ADY", selected = "ADY")
    ),
    marker_shape_var = teal.picks::picks(
      teal.picks::datasets("ADRS"),
      teal.picks::variables(choices = "AVALC", selected = "AVALC")
    ),
    marker_shape_opt = c(CR = 16),
    marker_color_var = teal.picks::picks(
      teal.picks::datasets("ADRS"),
      teal.picks::variables(choices = "AVALC", selected = "AVALC")
    ),
    marker_color_opt = c(CR = "green"),
    anno_txt_var = teal.picks::picks(
      teal.picks::datasets("ADSL"),
      teal.picks::variables(
        choices = c("ACTARM", "SEX"),
        selected = "ACTARM",
        multiple = TRUE
      )
    )
  )
  testthat::expect_s3_class(mod, "teal_module")
  testthat::expect_identical(mod$server, srv_g_swimlane)
})
