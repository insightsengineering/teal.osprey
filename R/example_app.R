# nolint start
# Example App Using Random ADaM Dataset
# - to use, copy into a new R scrip file and uncomment scripts
#
# .libPaths(c(normalizePath("./libs"), .libPaths()))
#
# library(teal.modules.clinical)
# library(teal.osprey)
# library(dplyr)
#
# options(teal_logging = FALSE)
#
#
# ASL <- rADSL
# ATE <- rADTTE
# AAE <- rADAE
# ATR <- rADTR
# ARS <- rADRS
# ARS_swim <- rADRS
#
#
# # Data processing
# #@start_preprocessing
# AAE <- AAE %>% mutate(RELFL = ifelse(AEREL == "Y", "Y", "N"),
#                       CTC35FL = ifelse(AETOXGR %in% c("3", "4", "5"), "Y", "N"),
#                       SERFL = ifelse(AESER == "Y", "Y", "N"),
#                       RELSERFL = ifelse(AEREL == "Y" & AESER == "Y", "Y", "N"))
#
# ARS_swim <- ARS_swim %>% filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
#   mutate(AVALC = DCSREAS,
#          ADY   = EOSDY) %>%
#   rbind(ARS %>% filter(PARAMCD == "OVRINV" & AVALC != "NE")) %>%
#   arrange(USUBJID)
#
# ATR <- ATR %>% mutate(PCHG = ifelse(is.na(PCHG), 0, PCHG),
#                       CHG  = ifelse(is.na(CHG), 0, CHG),
#                       AVAL = ifelse(is.na(AVAL), BASE, AVAL),
#                       AVALC = ifelse(is.na(AVALC), as.character(BASE), AVALC))
#
# #@end_preprocessing
#
# #Attributes must come after data preprcessing
# attr(ASL, "source") <- "rADSL"
# attr(ARS, "source") <- "rADRS"
# attr(ATE, "source") <- "rADTTE"
# attr(AAE, "source") <- "rADAE"
# attr(ATR, "source") <- "rADTR"
# attr(ARS_swim, "source") <- "rADRS"
#
# ## Define Reusable Configuartions for teal.osprey modules----
# arm_var <- "ACTARM"
# arm_var_choices <- c("ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1","SEX", "STRATA1", "STRATA2", "BMK2")
#
# strata_var <- "AGEGR1"
# strata_var_choices <- c("AGEGR1", "SEX", "STRATA1", "STRATA2", "RACE", "BMK2")
#
# aeclass_var <- "AEBODSYS"
# aeclass_var_choices <- c("AEBODSYS", "AESOC", "AEHLGT", "AEHLT")
#
# aeterm_var <- "AEDECOD"
# aeterm_var_choices <- c("AEDECOD", "AETERM")
#
# paramcd_tte <- "OS"
# paramcd_choices_tte <- unique(ATE$PARAMCD)
#
# paramcd_tr <- "SLDINV"
# paramcd_choices_tr <- unique(ATR$PARAMCD)
#
# paramcd_rsp <- "BESRSPI"
# paramcd_choices_rsp <- setdiff(unique(ARS$PARAMCD), "OVRINV")
#
#
#
# ## Configure teal ----
# # Not working teal function
# # chunks <- teal::parse_code_chunks(file = "./app.R")
#
# ## Create front page for app ----
# srv_front_page <- function(input, output, session, datasets) {
#   observeEvent(input$show_data_generation_rcode, {
#     showModal(modalDialog(
#       title = "R Code Used to Generate the random ADaM datasets - ADSL, ADAE, ADTTE, ADRS, and ADTR ",
#       tags$pre(paste(readLines("https://raw.github.roche.com/Rpackages/osprey/master/inst/generate_random_data.R"), collapse = "\n")),
#       size = "l"
#     ))
#   })
#
#   observeEvent(input$show_teal_setup_code, {
#     showModal(modalDialog(
#       title = "R Code Used to Setup the Current Teal Shiny App",
#       tags$pre(paste(readLines("app.R"), collapse = "\n")),
#       size = "l"
#     ))
#   })
# }
#
#
# ui_front_page <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tags$p("The", tags$code("ADSL"), ",", tags$code("ADAE"), ",", tags$code("ADTTE"), ",", tags$code("ADRS"), ", and ", tags$code("ADTR"),
#            "data in this example app has been created using random number generators."),
#     tags$p("", style = "height: 15px;"),
#     actionButton(ns("show_data_generation_rcode"), "Show Data Generation R Code", icon = icon("glyphicon-align-justify")),
#     tags$p("", style = "height: 20px;"),
#     tags$p(paste("These apps are relatively easily setup for a study.",
#                  "That is, the teal framework is optimized to setup one",
#                  "Shiny App per analysis purpose. For example, the code to setup",
#                  "the current teal app can be requested with the following button:")),
#     tags$p("", style = "height: 15px;"),
#     actionButton(ns("show_teal_setup_code"), "Show Teal Shiny App Setup R-Code", icon = icon("glyphicon-align-justify"))
#   )
# }
#
#
# ## Setup App
# ## Need to add ADSL to validation
# x <- teal::init(
#   data = list(ASL = ASL, ARS = ARS, ARS_swim = ARS_swim, ATE = ATE, AAE = AAE, ATR = ATR),
#   modules = root_modules(
#     module(
#       label = "App Information",
#       server = srv_front_page,
#       ui = ui_front_page,
#       filters = NULL
#     ),
#     tm_data_table("Data Table"),
#     tm_variable_browser("Variable Browser"),
#     tm_t_summarize_variables(
#       label = "Demographics",
#       dataname = "ASL",
#       arm_var = arm_var,
#       arm_var_choices = arm_var_choices,
#       summarize_vars =  c("AGE", "SEX", "STRATA1", "BMK1"),
#       summarize_vars_choices = names(ASL)
#     ),
#     modules(
#       "Adverse Events",
#       tm_t_ae_oview(
#         label = "Adverse Event Overview Table",
#         dataname = "AAE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         total_col = TRUE,
#         code_data_processing = chunks$preprocessing
#       ),
#       tm_t_ae(
#         label = "Adverse Events Table",
#         dataname = "AAE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         filter_var = NULL,
#         filter_var_choices = c("RELFL", "CTC35FL", "SERFL", "RELSERFL"),
#         class_var = aeclass_var,
#         class_var_choices = aeclass_var_choices,
#         term_var = aeterm_var,
#         term_var_choices = aeterm_var_choices,
#         total_col = TRUE,
#         pre_output = helpText("Summary table takes some time to generate for large AE datasets, please be patient"),
#         code_data_processing = chunks$preprocessing
#       ),
#       tm_t_ae_ctc(
#         label = "Adverse Events Table By Highest NCI CTCAE Grade",
#         dataname = "AAE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         filter_var = NULL,
#         filter_var_choices = c("RELFL", "CTC35FL", "SERFL", "RELSERFL"),
#         class_var = aeclass_var,
#         class_var_choices = aeclass_var_choices,
#         term_var = aeterm_var,
#         term_var_choices = aeterm_var_choices,
#         total_col = TRUE,
#         pre_output = helpText("Summary table takes some time to generate for large AE datasets, please be patient"),
#         code_data_processing = chunks$preprocessing
#       ),
#       tm_g_butterfly(
#         label = "Butterfly Plot",
#         dataname = "AAE",
#         filter_var = NULL,
#         filter_var_choices = c("RELFL", "CTC35FL", "SERFL", "RELSERFL"),
#         right_var = arm_var,
#         right_var_choices = arm_var_choices,
#         left_var = arm_var,
#         left_var_choices = arm_var_choices,
#         category_var = aeclass_var,
#         category_var_choices = c(aeclass_var_choices, aeterm_var_choices),
#         color_by_var = "AETOXGR",
#         color_by_var_choices = c("AETOXGR", "None"),
#         count_by_var = "# of patients",
#         count_by_var_choices = c("# of patients", "# of AEs"),
#         facet_var = NULL,
#         facet_var_choices = arm_var_choices,
#         sort_by_var = "count",
#         sort_by_var_choices = c("count", "alphabetical"),
#         legend_on = TRUE,
#         plot_height = c(600, 200, 2000),
#         pre_output = helpText("Plot takes some time to generate for large AE datasets, please be patient"),
#         code_data_processing = chunks$preprocessing
#       )
#     ),
#     tm_t_ds(
#       label = "Disposition Table",
#       dataname = "ASL",
#       arm_var = arm_var,
#       arm_var_choices = arm_var_choices,
#       class_var = "EOSSTT",
#       class_var_choices = c("EOSSTT", "DCSREAS_GRP"),
#       term_var = "DCSREAS",
#       term_var_choices = c("DCSREAS", "DCSREASP"),
#       total_col = TRUE,
#       code_data_processing = chunks$preprocessing
#     ),
#     tm_g_spiderplot(
#       label = "Spiderplot",
#       dataname = "ATR",
#       paramcd = "SLDINV",
#       paramcd_choices = c("SLDINV"),
#       x_var = "ADY",
#       x_var_choices = c("ADY", "AVISIT"),
#       y_var = "PCHG",
#       y_var_choices = c("PCHG", "CHG", "AVAL"),
#       marker_var = "USUBJID",
#       marker_var_choices = c("USUBJID", "SEX", "RACE"),
#       line_colorby_var = "USUBJID",
#       line_colorby_var_choices = c("USUBJID", "SEX", "RACE"),
#       vref_line = "0, 10",
#       href_line = "-20, 0",
#       anno_txt_var = TRUE,
#       # anno_disc_study = TRUE,
#       legend_on = TRUE,
#       xfacet_var = NULL,
#       xfacet_var_choices = arm_var_choices,
#       yfacet_var = NULL,
#       yfacet_var_choices = arm_var_choices,
#       plot_height = c(600, 200, 2000),
#       code_data_processing = chunks$preprocessing
#     ),
#     tm_g_swimlane(
#       label = "Swimlane Plot",
#       dataname = 'ARS_swim',
#       bar_var = "TRTDURD",
#       bar_var_choices = c("TRTDURD", "EOSDY"),
#       bar_color_var = "EOSSTT",
#       bar_color_var_choices = c("EOSSTT", arm_var_choices),
#       sort_var = "ACTARMCD",
#       sort_var_choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD"),
#       marker_pos_var = "ADY",
#       marker_pos_var_choices = c("None", "ADY"),
#       marker_shape_var = "AVALC",
#       marker_shape_var_choices = c("None", "AVALC", "AVISIT"),
#       marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
#       marker_color_var = "AVALC",
#       marker_color_var_choices = c("None", "AVALC", "AVISIT"),
#       marker_color_opt = c("CR" = "green", "PR" = "blue", "SD" = "goldenrod", "PD" = "red", "Death" = "black"),
#       vref_line = "30, 60",
#       anno_txt_var = c("ACTARM", "SEX"),
#       anno_txt_var_choices = c(arm_var_choices, "RACE","COUNTRY","DCSREAS", "DCSREASP"),
#       code_data_processing = chunks$preprocessing
#     ),
#     modules(
#       "Efficacy Analyses",
#       tm_g_forest_tte(
#         label = "Survival Forest Plot",
#         dataname = "ATE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         subgroup_var = strata_var,
#         subgroup_var_choices = strata_var_choices,
#         paramcd = paramcd_tte,
#         paramcd_choices = paramcd_choices_tte,
#         plot_height = c(800, 200, 4000)
#       ),
#       tm_g_forest_rsp(
#         label = "Response Forest Plot",
#         dataname = "ARS",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         subgroup_var = strata_var,
#         subgroup_var_choices = strata_var_choices,
#         paramcd = paramcd_rsp,
#         paramcd_choices = paramcd_choices_rsp,
#         plot_height = c(800, 200, 4000)
#       ),
#       tm_g_km(
#         label = "Kaplan Meier Plot",
#         dataname = "ATE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         paramcd = paramcd_tte,
#         paramcd_choices = paramcd_choices_tte,
#         facet_var = NULL,
#         facet_var_choices = c("SEX", "STRATA1", "STRATA2"),
#         strata_var = strata_var,
#         strata_var_choices = strata_var_choices,
#         plot_height = c(800, 200, 4000)
#       ),
#       tm_t_rsp(
#         label = "Response Table",
#         dataname = "ARS",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         paramcd = paramcd_rsp,
#         paramcd_choices = paramcd_choices_rsp,
#         strata_var = strata_var,
#         strata_var_choices = strata_var_choices
#       ),
#       tm_t_tte(
#         label = "Time To Event Table",
#         dataname = "ATE",
#         arm_var = arm_var,
#         arm_var_choices = arm_var_choices,
#         paramcd = paramcd_tte,
#         paramcd_choices = paramcd_choices_tte,
#         strata_var = strata_var,
#         strata_var_choices = strata_var_choices,
#         time_points = c(6, 12, 18),
#         time_points_choices = c(6, 12, 18, 24, 30, 36, 42),
#         time_unit = "month",
#         event_desrc_var = "EVNTDESC"
#       )
#     )
#   ),
#   header = div(
#     class="",
#     style="margin-bottom: 2px;",
#     tags$h1("Demo ED Onco teal app with random ADaM data", tags$span("ED SPA", class="pull-right"))
#   ),
#   footer = tags$p(class="text-muted",
#                   actionLink("showAboutModal", "Info About Authors"))
# )
#
#
# # Add server code
# body(x$server)[[length(body(x$server))+1]] <- quote(
#   observeEvent(input$showAboutModal, {
#     showModal(modalDialog(
#       title = "About this shiny app",
#       tags$p(
#         "This is shiny app was brought to you by ED SPA. For more information please contact either"
#       ),
#       tags$ul(
#         tags$li(tags$a(href="mailto:mika.maekinen@roche.com", "Mika Maekinen")),
#         tags$li(tags$a(href="mailto:chendi.liao@roche.com", "Chendi Liao")),
#         tags$li(tags$a(href="mailto:qi.ting@gene.com", "Nina Qi")),
#         tags$li(tags$a(href="mailto:zhang.carolyn@gene.com", "Carolyn Zhang"))
#       ),
#       tags$p(
#         class="text-muted",
#         "The app uses teal version", utils::packageDescription(pkg = "teal", field="Version"),
#         ", rtables version", utils::packageDescription(pkg = "rtables", field="Version"),
#         ", teal.modules.clinical version", utils::packageDescription(pkg = "teal.modules.clinical", field="Version"),
#         ", osprey version", utils::packageDescription(pkg = "osprey", field="Version"),
#         ", teal.osprey version", utils::packageDescription(pkg = "teal.osprey", field="Version")
#       ),
#       easyClose = TRUE
#     ))
#   })
# )
#
# ## Start Teal Shiny App ----
# shinyApp(x$ui, x$server)
# nolint stop