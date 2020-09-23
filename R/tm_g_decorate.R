#' Helper UI function to decorate plot output UI
#'
#' This is used in \code{\link{tm_g_ae_oview}} and \code{\link{tm_g_events_term_id}}.
#'
#' @param id (\code{string}) id of this module. set to `NULL` if you want to make it identical
#' to the module who called it.
#' @param titles (\code{string}) default titles
#' @param footnotes (\code{string}) default footnotes
#' @param fontsize a numeric vector with 3 values, selected font size and font size range,
#' default is \code{c(5, 3, 7)}
#' @export
ui_g_decorate <- function(id,
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
      textAreaInput(ns("foot"), "Footnote", value = footnotes, resize = "none")
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
#' @param height vector with three \code{integer} elements defining selected,
#' min and max plot height
#'
#' @importFrom grid grid.draw gpar
#' @importFrom tern decorate_grob
#' @importFrom ggplot2 .pt
#' @export
srv_g_decorate <- function(input,
                           output,
                           session,
                           plot_id = "out",
                           plt = reactive(NULL),
                           height) {

    plot_r <- reactive({
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

    callModule(plot_with_settings_srv,
               id = plot_id,
               plot_r = plot_r,
               height = height)

    return(reactive(input$fontsize))
  }

#' Helper function to plot decorated output ui
#'
#' @param id (\code{string}) id of this element
#' @param plot_height vector with three \code{integer} elements defining selected,
#' min and max plot height, default is \code{c(600, 200, 2000)}
#' @export
plot_decorate_output <- function(id, plot_height = c(600, 200, 2000)) {
  ns <- NS(id)
  plot_with_settings_ui(id = ns("out"), height = plot_height)
}
