#' Helper UI function to decorate plot output UI
#'
#' This is used in \code{\link{tm_g_ae_oview}} and \code{\link{tm_g_events_term_id}}.
#'
#' @param id (\code{string}) id of this module. set to `NULL` if you want to make it identical
#' to the module who called it.
#' @param plot_height vector with three \code{integer} elements defining selected,
#' min and max plot height, default is \code{c(600, 200, 2000)}
#' @param titles (\code{string}) default titles
#' @param footnotes (\code{string}) default footnotes
#' @param fontsize a numeric vector with 3 values, selected font size and font size range,
#' default is \code{c(5, 3, 7)}
#' @importFrom teal optionalSliderInputValMinMax
#' @importFrom shiny textInput textAreaInput NS
#' @importFrom teal.devel plot_height_input
#' @export
ui_g_decorate <- function(id,
                          plot_height = c(600, 200, 2000),
                          titles = "Titles",
                          footnotes = "footnotes",
                          fontsize = c(5, 4, 11)) {
    ns <- NS(id)
    tagList(
      optionalSliderInputValMinMax(
        ns("fontsize"),
        "Font Size",
        value_min_max = fontsize,
        step = 0.1
      ),
      textInput(ns("title"), "Title", value = titles),
      textAreaInput(ns("foot"), "Footnote", value = footnotes, resize = "none"),
      plot_height_input(id = ns("plot_height"),
                        value = plot_height)
    )
  }

#' Helper server function to decorate plot output
#'
#' This is used in \code{\link{tm_g_ae_oview}} and \code{\link{tm_g_events_term_id}}.
#'
#' @param input the session's \code{input} object
#' @param output the session's \code{output} object
#' @param session session object is an environment that can be used to access information
#' and functionality relating to the session
#' @param plot_id (\code{string}) id for plot output
#' @param plt a reactive object of graph object
#'
#' @importFrom shiny renderUI req plotOutput renderPlot reactive
#' @importFrom grid grid.draw gpar
#' @importFrom tern decorate_grob
#' @importFrom ggplot2 .pt
#' @export
srv_g_decorate <- function(input,
                           output,
                           session,
                           plot_id = "plot",
                           plt = reactive(NULL)) {
    output$out <- renderUI({
      req(input$plot_height)
      plotOutput(session$ns(plot_id), height = input$plot_height)
    })
    output[[plot_id]] <- renderPlot({
      grid.draw(
        decorate_grob(
          plt(),
          titles = input$title,
          footnotes = input$foot,
          gp_titles = gpar(
            fontsize = input$fontsize * .pt,
            col = "black",
            fontface = "bold"
          ),
          gp_footnotes = gpar(fontsize = input$fontsize * .pt, col = "black")
        )
      )
    })
    return(reactive(input$fontsize))
  }

#' Helper function to plot decorated output ui
#'
#' @param id (\code{string}) id of this element
#' @export
plot_decorate_output <- function(id) {
  ns <- NS(id)
  uiOutput(ns("out"))
}