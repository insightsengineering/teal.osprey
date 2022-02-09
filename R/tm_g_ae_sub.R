#' teal module for the AE by subgroups
#'
#' Display the AE by subgroups plot as a teal module
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams argument_convention
#' @param group_var (`choices_selected`) subgroups variables. See [teal::choices_selected()] for details.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @inherit argument_convention return
#'
#' @export
#'
#' @examples
#' # Example using stream (ADaM) dataset
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_ae_sub(
#'       label = "AE by Subgroup",
#'       dataname = "ADAE",
#'       arm_var = choices_selected(
#'         selected = "ACTARMCD",
#'         choices = c("ACTARM", "ACTARMCD")
#'       ),
#'       group_var = choices_selected(
#'         selected = c("SEX", "REGION1", "RACE"),
#'         choices = c("SEX", "REGION1", "RACE")
#'       ),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_ae_sub <- function(label,
                        dataname,
                        arm_var,
                        group_var,
                        plot_height = c(600L, 200L, 2000L),
                        plot_width = NULL,
                        fontsize = c(5, 3, 7)) {
  logger::log_info("Initializing tm_g_ae_sub")
  checkmate::assert_class(arm_var, classes = "choices_selected")
  checkmate::assert_class(group_var, classes = "choices_selected")
  checkmate::assert(
    checkmate::check_number(fontsize, finite = TRUE),
    checkmate::assert(
      combine = "and",
      .var.name = "fontsize",
      checkmate::check_numeric(fontsize, len = 3, any.missing = FALSE, finite = TRUE),
      checkmate::check_numeric(fontsize[1], lower = fontsize[2], upper = fontsize[3])
    )
  )
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  module(
    label = label,
    server = srv_g_ae_sub,
    server_args = list(
      label = label,
      dataname = dataname,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_ae_sub,
    ui_args = list(
      arm_var = arm_var,
      group_var = group_var,
      fontsize = fontsize
    ),
    filters = dataname
  )
}

ui_g_ae_sub <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  standard_layout(
    output = white_small_well(
      plot_decorate_output(id = ns(NULL))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code("ADAE")),
      optionalSelectInput(
        ns("arm_var"),
        "Arm Variable",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_trt"),
        "Treatment",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      selectInput(
        ns("arm_ref"),
        "Control",
        choices = args$arm_var$choices,
        selected = args$arm_var$selected
      ),
      checkboxInput(
        ns("arm_n"),
        "Show N in each arm",
        value = args$arm_n
      ),
      optionalSelectInput(
        ns("groups"),
        "Group Variable",
        choices = args$group_var$choices,
        selected = args$group_var$selected,
        multiple = TRUE
      ),
      panel_item(
        "Change group labels",
        uiOutput(ns("grouplabel_output"))
      ),
      panel_item(
        "Additional plot settings",
        optionalSelectInput(
          ns("ci"),
          "CI method",
          choices = ci_choices,
          selected = ci_choices[1]
        ),
        optionalSliderInput(
          ns("conf_level"),
          "Significant Level",
          min = 0.5,
          max = 1,
          value = 0.95
        ),
        ui_g_decorate(
          ns(NULL),
          fontsize = args$fontsize,
          titles = "AE Table with Subgroups",
          footnotes = ""
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_g_ae_sub <- function(id,
                         datasets,
                         dataname,
                         label,
                         plot_height,
                         plot_width) {
  moduleServer(id, function(input, output, session) {
    init_chunks()
    font_size <- srv_g_decorate(
      id = NULL,
      plt = plt,
      plot_height = plot_height,
      plot_width = plot_width
    )
    observeEvent(input$arm_var, {
      req(!is.null(input$arm_var))
      arm_var <- input$arm_var
      ADAE_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
      ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint

      choices <- unique(ADAE_FILTERED[[arm_var]])

      validate(need(
        length(choices) > 0, "Please include multiple treatment"
      ))
      if (length(choices) == 1) {
        ref_index <- 1
      } else {
        ref_index <- 2
      }

      updateSelectInput(
        session,
        "arm_trt",
        selected = choices[1],
        choices = choices
      )
      updateSelectInput(
        session,
        "arm_ref",
        selected = choices[ref_index],
        choices = choices
      )
    })

    observeEvent(list(input$ci, input$conf_level, input$arm_trt, input$arm_ref), {
      diff_ci_method <- input$ci
      conf_level <- input$conf_level
      trt <- input$arm_trt
      ref <- input$arm_ref
      updateTextAreaInput(
        session,
        "foot",
        value = sprintf(
          "Note: %d%% CI is calculated using %s\nTRT: %s; CONT: %s",
          round(conf_level * 100),
          name_ci(diff_ci_method),
          trt,
          ref
        )
      )
    })

    observeEvent(input$groups, {
      ADAE_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
      ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint
      output$grouplabel_output <- renderUI({
        grps <- input$groups
        lo <- lapply(seq_along(grps), function(index) {
          grp <- grps[index]
          choices <- levels(ADAE_FILTERED[[grp]])
          sel <- optionalSelectInput(
            session$ns(sprintf("groups__%s", index)),
            grp,
            choices,
            multiple = TRUE,
            selected = choices
          )
          textname <- sprintf("text_%s_out", index)
          txt <- uiOutput(session$ns(textname))
          observeEvent(
            eventExpr = input[[sprintf("groups__%s", index)]],
            handlerExpr = {
              output[[textname]] <- renderUI({
                if (!is.null(input[[sprintf("groups__%s", index)]])) {
                  l <- input[[sprintf("groups__%s", index)]]
                  l2 <- lapply(seq_along(l), function(i) {
                    nm <- sprintf("groups__%s__level__%s", index, i)
                    label <- sprintf("Label for %s, Level %s", grp, l[i])
                    textInput(session$ns(nm), label, l[i])
                  })
                  tagList(textInput(
                    session$ns(
                      sprintf("groups__%s__level__%s", index, "all")
                    ),
                    sprintf("Label for %s", grp), grp
                  ), l2)
                }
              })
            }
          )
          tagList(sel, txt)
        })
        ret <- tagList(lo)
        ret
      })
    })

    plt <- reactive({
      validate(need(input$arm_var, "Please select an arm variable."))
      ADAE_FILTERED <- datasets$get_data("ADAE", filtered = TRUE) # nolint
      ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE) # nolint

      validate(need(
        is.factor(ADSL_FILTERED[[input$arm_var]]),
        "Selected arm variable needs to be a factor."
      ))
      validate(
        need(
          all(c(input$arm_trt, input$arm_ref) %in% levels(ADSL_FILTERED[[input$arm_var]])),
          "Updating treatment and control selections."
        )
      )
      validate(
        need(
          all(c(input$arm_trt, input$arm_ref) %in% unique(ADAE_FILTERED[[input$arm_var]])),
          "The dataset does not contain subjects with AE events from both the control and treatment arms."
        ),
        need(
          all(input$groups %in% names(ADAE_FILTERED)) &
            all(input$groups %in% names(ADSL_FILTERED)),
          "Check all selected subgroups are columns in ADAE and ADSL."
        ),
        need(
          input$arm_trt != input$arm_ref,
          "Treatment and Reference can not be identical."
        )
      )

      chunks_reset(envir = environment())

      chunks_push(bquote({
        id <- ADAE_FILTERED$USUBJID
        arm <- as.factor(ADAE_FILTERED[[.(input$arm_var)]])
        arm_sl <- as.character(ADSL_FILTERED[[.(input$arm_var)]])
        grps <- .(input$groups)
        subgroups <- ADAE_FILTERED[grps]
        subgroups_sl <- ADSL_FILTERED[grps]
        trt <- .(input$arm_trt)
        ref <- .(input$arm_ref)
      }))
      chunks_push_new_line()

      chunks_safe_eval()

      group_labels <- lapply(seq_along(input$groups), function(x) {
        items <- input[[sprintf("groups__%s", x)]]
        if (length(items) > 0) {
          l <- lapply(seq_along(items), function(y) {
            input[[sprintf("groups__%s__level__%s", x, y)]]
          })
          names(l) <- items
          l[["Total"]] <- input[[sprintf("groups__%s__level__%s", x, "all")]]
          l
        }
      })

      if (length(unlist(group_labels)) == 0) {
        chunks_push(bquote({
          group_labels <- NULL
        }))
      } else {
        chunks_push(bquote({
          group_labels <- .(group_labels)
          names(group_labels) <- .(input$groups)
        }))
      }

      chunks_push_new_line()
      chunks_safe_eval()
      chunks_push(bquote({
        osprey::g_ae_sub(
          id = id,
          arm = arm,
          arm_sl = arm_sl,
          trt = trt,
          ref = ref,
          subgroups = subgroups,
          subgroups_sl = subgroups_sl,
          subgroups_levels = group_labels,
          conf_level = .(input$conf_level),
          diff_ci_method = .(input$ci),
          fontsize = .(font_size()),
          arm_n = .(input$arm_n),
          draw = TRUE
        )
      }))

      chunks_safe_eval()
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = paste("R code for", label),
      datanames = unique(c(
        dataname,
        vapply(X = dataname, FUN.VALUE = character(1), function(x) {
          if (inherits(datasets, "CDISCFilteredData")) datasets$get_parentname(x)
        })
      ))
    )
  })
}
