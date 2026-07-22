test_that("tm_g_swimlane builds a teal module with picks encodings", {
  mod <- tm_g_swimlane(
    label = "Swimlane",
    dataname = "ADRS",
    bar_var = picks(
      datasets("ADSL"),
      variables(choices = "TRTDURD", selected = "TRTDURD")
    ),
    bar_color_var = picks(
      datasets("ADSL"),
      variables(choices = "EOSSTT", selected = "EOSSTT")
    ),
    marker_pos_var = picks(
      datasets("ADRS"),
      variables(choices = "ADY", selected = "ADY")
    ),
    marker_shape_var = picks(
      datasets("ADRS"),
      variables(choices = "AVALC", selected = "AVALC")
    ),
    marker_shape_opt = c(CR = 16),
    marker_color_var = picks(
      datasets("ADRS"),
      variables(choices = "AVALC", selected = "AVALC")
    ),
    marker_color_opt = c(CR = "green"),
    anno_txt_var = picks(
      datasets("ADSL"),
      variables(
        choices = c("ACTARM", "SEX"),
        selected = "ACTARM",
        multiple = TRUE
      )
    )
  )
  expect_s3_class(mod, "teal_module")
  expect_identical(mod$server, srv_g_swimlane)
})

test_that("tm_g_waterfall builds a teal module with picks encodings", {
  mod <- tm_g_waterfall(
    label = "Waterfall",
    bar_paramcd = picks(
      datasets("ADTR"),
      variables("PARAMCD", "PARAMCD"),
      values(choices = "SLDINV", selected = "SLDINV")
    ),
    bar_var = picks(
      datasets("ADTR"),
      variables(choices = c("PCHG", "AVAL"), selected = "PCHG")
    ),
    bar_color_var = picks(
      datasets("ADSL"),
      variables(choices = c("ARMCD", "SEX"), selected = "ARMCD")
    )
  )
  expect_s3_class(mod, "teal_module")
  expect_identical(mod$server, srv_g_waterfall)
  expect_equal(mod$datanames, c("ADSL", "ADTR", "ADRS"))
})
