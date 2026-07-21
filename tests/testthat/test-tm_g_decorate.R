testthat::describe("srv_g_decorate creation", {
  it("srv_g_decorate returns", {
    testServer(
      srv_g_decorate,
      args = list(id = NULL, plot_height = c(1, 1, 1), plot_width = c(1, 1, 1)),
      expr = {
        session$setInputs(fontsize = c(1, 1, 1))
        expect_s3_class(plot_r, "reactive")
        returned <- session$returned
        expect_type(returned, "list")
        expect_s3_class(returned$font_size, "reactive")
        expect_equal(returned$font_size(), c(1, 1, 1))
        expect_type(returned$pws, "list")
      }
    )
  })
})
