#' decorate plot output ui
#' @param id id of this module. set to `NULL` if you want to make it identical
#' to the module who called it.
#' @param plot_height plot height control value min max.
#' @param titles default title
#' @param footnotes default footnotes
#' @param fontsize font size of title/footnotes
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

#' server side function of decorate module
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param plot_id plot output id
#' @param plt reactive object of graph object
#' @importFrom shiny renderUI req plotOutput renderPlot reactive
#' @importFrom grid grid.draw gpar
#' @importFrom tern decorate_grob
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

#' plot decorated output ui
#' @param id id of this element
#' @export
plot_decorate_output <- function(id) {
  ns <- NS(id)
  uiOutput(ns("out"))
}