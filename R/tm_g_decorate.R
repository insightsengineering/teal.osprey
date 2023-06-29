#' Helper UI function to decorate plot output UI
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is used in \code{\link{tm_g_ae_oview}} and \code{\link{tm_g_events_term_id}}.
#'
#' @param id (\code{character}) id of this module. set to `NULL` if you want to make it identical
#' to the module who called it.
#' @param titles (\code{character}) default titles
#' @param footnotes (\code{character}) default footnotes
#' @inheritParams argument_convention
#' @export
ui_g_decorate <- function(id,
                          titles = "Titles",
                          footnotes = "footnotes",
                          fontsize = c(5, 4, 11)) {
  ns <- NS(id)
  tagList(
    teal.widgets::optionalSliderInputValMinMax(
      ns("fontsize"),
      "Font Size",
      value_min_max = fontsize,
      step = 0.1
    ),
    textInput(ns("title"), "Title", value = titles),
    textAreaInput(ns("foot"), "Footnote", value = footnotes, resize = "none")
  )
}

#' Helper server function to decorate plot output
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This is used in \code{\link{tm_g_ae_oview}} and \code{\link{tm_g_events_term_id}}.
#'
#' @inheritParams shared_params
#' @param id (\code{character}) id of the module
#' @param plot_id (\code{character}) id for plot output
#' @param plt (\code{reactive}) a reactive object of graph object
#'
#' @export
srv_g_decorate <- function(id,
                           plot_id = "out",
                           plt = reactive(NULL),
                           plot_height,
                           plot_width) {
  moduleServer(id, function(input, output, session) {
    plot_g <- reactive({
      g <- tern::decorate_grob(
        plt(),
        titles = input$title,
        footnotes = input$foot,
        gp_titles = grid::gpar(
          fontsize = input$fontsize * ggplot2::.pt,
          col = "black",
          fontface = "bold"
        ),
        gp_footnotes = grid::gpar(fontsize = input$fontsize * ggplot2::.pt, col = "black")
      )
    })

    plot_r <- function() {
      grid::grid.newpage()
      grid::grid.draw(plot_g())
      plot_g()
    }

    class(plot_r) <- c(class(plot_r), "reactive")

    pws <- teal.widgets::plot_with_settings_srv(
      id = plot_id,
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    return(
      list(
        font_size = reactive(input$fontsize),
        pws = pws
      )
    )
  })
}

#' Helper function to plot decorated output UI
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param id (\code{character}) id of this element
#'
#' @export
plot_decorate_output <- function(id) {
  ns <- NS(id)
  teal.widgets::plot_with_settings_ui(id = ns("out"))
}
