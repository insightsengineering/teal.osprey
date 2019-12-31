# Example App Using Random ADaM Dataset
# - to use, copy into a new R scrips file and uncomment scripts

source("https://raw.github.roche.com/NEST/nest_on_bee/master/bee_nest_utils.R")
bee_use_nest(release = "uat_2019_11_04")

library(teal.modules.clinical)
library(teal.modules.general)
library(teal.osprey)
library(dplyr)

options(teal_logging = FALSE)

# code>
ADSL <- rADSL
ADTTE <- rADTTE
ADAE <- rADAE
ADTR <- rADTR
ADRS <- rADRS

ADSL$RACE <- droplevels(ADSL$RACE)
ADSL$SEX <- droplevels(ADSL$SEX)

ADAE <- ADAE %>% mutate(RELFL = ifelse(AEREL == "Y", "Y", "N"),
                      CTC35FL = ifelse(AETOXGR %in% c("3", "4", "5"), "Y", "N"),
                      SERFL = ifelse(AESER == "Y", "Y", "N"),
                      RELSERFL = ifelse(AEREL == "Y" & AESER == "Y", "Y", "N"))

ADRS_SWIM <- ADRS %>%
  filter(PARAMCD == "LSTASDI" & DCSREAS == "Death") %>%
  mutate(AVALC = DCSREAS, ADY = EOSDY) %>%
  rbind(filter(ADRS, PARAMCD == "OVRINV" & AVALC != "NE")) %>%
  arrange(USUBJID)

ADRS <- filter(ADRS, AVISIT == "End of Treatment")

ADTR <- ADTR %>% mutate(PCHG = ifelse(is.na(PCHG), 0, PCHG),
                      CHG  = ifelse(is.na(CHG), 0, CHG),
                      AVAL = ifelse(is.na(AVAL), BASE, AVAL),
                      AVALC = ifelse(is.na(AVALC), as.character(BASE), AVALC))

# <code
## Create front page for app ----
srv_front_page <- function(input, output, session, datasets, dataname) {
  observeEvent(input$show_data_generation_rcode, {
    showModal(modalDialog(
      title = "R Code Used to Generate the random ADaM datasets - ADSL, ADAE, ADTTE, ADRS, and ADTR ",
      tags$pre(paste(readLines("https://raw.github.roche.com/Rpackages/osprey/master/inst/generate_random_data.R"),
                     collapse = "\n")),
      size = "l"
    ))
  })

  observeEvent(input$show_teal_setup_code, {
    showModal(modalDialog(
      title = "R Code Used to Setup the Current Teal Shiny App",
      tags$pre(paste(readLines(system.file("example_app.R", package = "teal.osprey")), collapse = "\n")),
      size = "l"
    ))
  })
}


ui_front_page <- function(id, ...) {
  ns <- NS(id)
  tagList(
    tags$p("The", tags$code("ADSL"), ",", tags$code("ADAE"), ",", tags$code("ADTTE"), ",", tags$code("ADRS"), ", and ",
           tags$code("ADTR"), "data in this example app has been created using random number generators."),
    tags$p("", style = "height: 15px;"),
    actionButton(ns("show_data_generation_rcode"),
                 "Show Data Generation R Code",
                 icon = icon("glyphicon-align-justify")),
    tags$p("", style = "height: 20px;"),
    tags$p(paste("These apps are relatively easily setup for a study.",
                 "That is, the teal framework is optimized to setup one",
                 "Shiny App per analysis purpose. For example, the code to setup",
                 "the current teal app can be requested with the following button:")),
    tags$p("", style = "height: 15px;"),
    actionButton(ns("show_teal_setup_code"), "Show Teal Shiny App Setup R-Code", icon = icon("glyphicon-align-justify"))
  )
}


## Setup App
## Need to add ADSL to validation
x <- teal::init(
  data = cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ADRS", ADRS),
    cdisc_dataset("ADRS_SWIM", ADRS_SWIM,
                  keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
                              foreign = c("STUDYID", "USUBJID"),
                              parent = "ADSL")),
    cdisc_dataset("ADTTE", ADTTE),
    cdisc_dataset("ADAE", ADAE,
                  keys = keys(primary = c("STUDYID", "USUBJID", "AETERM",  "AESEQ"),
                              foreign = c("STUDYID", "USUBJID"),
                              parent = "ADSL")),
    cdisc_dataset("ADTR",  ADTR, keys = keys(primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
                                             foreign = c("STUDYID", "USUBJID"),
                                             parent = "ADSL")),
    code = get_code(system.file("example_app.R", package = "teal.osprey"),
                    exclude_comments = TRUE,
                    read_sources = TRUE),
    check = FALSE),
  modules = root_modules(
    module(
      label = "App Information",
      server = srv_front_page,
      ui = ui_front_page,
      filters = "all"
    ),

    modules(
      "Adverse Events",
      tm_t_ae_oview(
        label = "AE Overview Summary Table",
        dataname = "ADAE",
        arm_var = choices_selected(choices = c("ARM", "ARMCD"),
                                   selected = "ARM"),
        total_col = FALSE
      ),
      tm_t_ae(
        label = "Adverse Events Table",
        dataname = "ADAE",
        filter_var = choices_selected(
          choices = c("DTHFL", "flag1"),
          selected = NULL
        ),
        arm_var = choices_selected(
          choices = c("ARM", "ARMCD"),
          selected = "ARM"
        ),
        class_var = choices_selected(
          choices = c("AEBODSYS", "AEHLTCD"),
          selected = "AEBODSYS"
        ),
        term_var = choices_selected(
          choices = c("AEDECOD", "AETERM"),
          selected = "AEDECOD"
        ),
        total_col = TRUE
      ),
      tm_t_ae_ctc(
        label = "Adverse Events Table By Highest NCI CTCAE Grade",
        dataname = "ADAE",
        filter_var = choices_selected(selected = NULL, choices = c("DTHFL", "flag1")),
        arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
        class_var = choices_selected(selected = "AEBODSYS", choices = c("AEBODSYS", "DEFAULT")),
        term_var = choices_selected(selected = "AEDECOD", choices = c("AEDECOD", "DEFAULT")),
        total_col = TRUE
      ),
      tm_g_butterfly(
        label = "Butterfly Plot",
        dataname = "ADAE",
        right_var = choices_selected(selected = "SEX", choices = c("DOSE", "SEX", "ARM",
                                                                   "RACE", "flag1", "flag2", "flag3")),
        left_var = choices_selected(selected = "RACE", choices = c("DOSE", "SEX", "ARM",
                                                                   "RACE", "flag1", "flag2", "flag3")),
        category_var = choices_selected(selected = "AEBODSYS", choices = c("AEDECOD", "AEBODSYS")),
        color_by_var = choices_selected(selected = "AETOXGR", choices = c("AETOXGR", "None")),
        count_by_var = choices_selected(selected = "# of patients",
                                        choices = c("# of patients", "# of AEs")),
        facet_var = choices_selected(selected = NULL, choices = c("RACE", "SEX", "ARM")),
        sort_by_var = choices_selected(selected = "count", choices = c("count", "alphabetical")),
        legend_on = TRUE,
        plot_height = c(600, 200, 2000)
      )
    ),
    tm_t_ds(
      label = "Patient Disposition Table",
      dataname = "ADSL",
      arm_var = choices_selected(selected = "ARM", choices = c("ARM", "ARMCD")),
      class_var =  choices_selected(selected = "EOSSTT", choices = "EOSSTT"),
      term_var = choices_selected(selected = "DCSREAS", choices = c("DCSREAS", "DCSREASP")),
      total_col = TRUE
    ),
    tm_g_spiderplot(
      label = "Spider plot",
      dataname = "ADTR",
      paramcd = choices_selected(choices = "SLDINV", selected = "SLDINV"),
      x_var = choices_selected(choices = "ADY", selected = "ADY"),
      y_var = choices_selected(choices = c("PCHG", "CHG", "AVAL"), selected = "PCHG"),
      marker_var = choices_selected(choices = c("SEX", "RACE", "USUBJID"), selected = "SEX"),
      line_colorby_var = choices_selected(choices = c("SEX", "USUBJID", "RACE"), selected = "SEX"),
      xfacet_var = choices_selected(choices = c("SEX", "ARM"), selected = "SEX"),
      yfacet_var = choices_selected(choices = c("SEX", "ARM"), selected = "ARM"),
      vref_line = "10, 37",
      href_line = "-20, 0",
      anno_txt_var = TRUE,
      legend_on = FALSE,
      plot_height = c(600, 200, 2000)
    ),
    tm_g_swimlane(
      label = "Swimlane Plot",
      dataname = "ADRS_SWIM",
      bar_var = choices_selected(selected = "TRTDURD", choices = c("TRTDURD", "EOSDY")),
      bar_color_var = choices_selected(
        selected = "EOSSTT",
        choices = c("EOSSTT", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1", "SEX")
      ),
      sort_var = choices_selected(
        selected = "ACTARMCD",
        choices = c("USUBJID", "SITEID", "ACTARMCD", "TRTDURD")
      ),
      marker_pos_var = choices_selected(selected = "ADY", choices = c("None", "ADY")),
      marker_shape_var = choices_selected(selected = "AVALC", c("None", "AVALC", "AVISIT")),
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
      marker_color_var = choices_selected(selected = "AVALC",
                                          choices = c("None", "AVALC", "AVISIT")),
      marker_color_opt = c("CR" = "green", "PR" = "blue", "SD" = "goldenrod",
                           "PD" = "red", "Death" = "black"),
      vref_line = c(30, 60),
      anno_txt_var = choices_selected(
        selected = c("ACTARM", "SEX"),
        choices = c("ARM", "ARMCD", "ACTARM", "ACTARMCD", "AGEGR1",
                    "SEX", "RACE", "COUNTRY", "DCSREAS", "DCSREASP")
      )
    ),
    modules(
      "Efficacy Analyses",
      tm_g_forest_tte(
        label = "Forest Survival",
        dataname = "ADTTE",
        arm_var = choices_selected(c("ARM", "ACTARMCD"), "ARM"),
        subgroup_var = choices_selected(names(ADSL), c("RACE", "SEX")),
        paramcd = choices_selected(c("OS", "PFS"), "OS"),
        plot_height = c(600, 200, 2000)
      ),
      tm_g_forest_rsp(
        label = "Forest Response",
        dataname = "ADRS",
        arm_var = choices_selected(c("ARM", "ACTARMCD"), "ARM"),
        paramcd = choices_selected(c("BESRSPI", "INVET", "OVRINV"), "OVRINV"),
        subgroup_var = choices_selected(names(ADSL), c("RACE", "SEX")),
        plot_height = c(600L, 200L, 2000L)
      ),
      tm_g_km(
        label = "KM PLOT",
        dataname = "ADTTE",
        arm_var = choices_selected(c("ARM", "ACTARMCD"), "ARM"),
        arm_ref_comp = list(
          ARM = list(
            ref = "B: Drug X",
            comp = c("A: Placebo", "C: Combination")
          ),
          ACTARMCD = list(
            ref = "ARM B",
            comp = "ARM A"
          )
        ),
        paramcd = choices_selected(c("OS", "PFS"), "OS"),
        facet_var = choices_selected(c("SEX", "BMK2"), "BMK2"),
        strata_var = choices_selected(c("SEX", "BMK2"), "SEX")
      ),
      tm_t_rsp(
        label = "Response Table",
        dataname = "ADRS",
        arm_var = choices_selected(c("ARM", "ACTARMCD"), "ARM"),
        paramcd = choices_selected(unique(ADRS$PARAMCD), "OVRINV"),
        strata_var = choices_selected(c("SEX", "BMK2"), "SEX")
      ),
      tm_t_tte(
        label = "Time To Event Table",
        dataname = "ADTTE",
        arm_var = choices_selected(c("ARM", "ACTARMCD"), "ARM"),
        paramcd = choices_selected(unique(ADTTE$PARAMCD), "OS"),
        strata_var = choices_selected(c("SEX", "BMK2"), "SEX"),
        time_points = choices_selected(c(6, 8), 6),
        time_unit = "month",
        event_desc_var = "EVNTDESC"
      )
    )
  ),
  header = div(
    class = "",
    style = "margin-bottom: 2px;",
    tags$h1("Demo ED Onco teal app with random ADaM data", tags$span("ED SPA", class = "pull-right"))
  ),
  footer = tags$p(class = "text-muted",
                  actionLink("showAboutModal", "Info About Authors"))
)


# Add server code
body(x$server)[[length(body(x$server)) + 1]] <- quote(
  observeEvent(input$showAboutModal, {
    showModal(modalDialog(
      title = "About this shiny app",
      tags$p(
        "This is shiny app was brought to you by ED SPA. For more information please contact either"
      ),
      tags$ul(
        tags$li(tags$a(href = "mailto:mika.maekinen@roche.com", "Mika Maekinen")),
        tags$li(tags$a(href = "mailto:chendi.liao@roche.com", "Chendi Liao")),
        tags$li(tags$a(href = "mailto:qi.ting@gene.com", "Nina Qi")),
        tags$li(tags$a(href = "mailto:zhang.carolyn@gene.com", "Carolyn Zhang"))
      ),
      tags$p(
        class = "text-muted",
        "The app uses teal version", utils::packageDescription(pkg = "teal", field = "Version"),
        ", rtables version", utils::packageDescription(pkg = "rtables", field = "Version"),
        ", teal.modules.clinical version", utils::packageDescription(pkg = "teal.modules.clinical", field = "Version"),
        ", osprey version", utils::packageDescription(pkg = "osprey", field = "Version"),
        ", teal.osprey version", utils::packageDescription(pkg = "teal.osprey", field = "Version")
      ),
      easyClose = TRUE
    ))
  })
)

## Start Teal Shiny App ----
shinyApp(x$ui, x$server)
