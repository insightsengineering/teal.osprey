#Example using stream (adam) dataset
library(teal)
library(teal.tern)
library(tern)
library(osprey)
suppressPackageStartupMessages(library(tidyverse))
library(rtables)
library(random.cdisc.data)
library(plyr)
library(dplyr)
library(gridExtra)
library(ggplot2)
require(lemon)
library(rocheBCE)

ASL <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adsl.sas7bdat") %>%
  mutate(EOSSTT = ifelse(EOSSTT == "Discontinued", EOSSTT, "Ongoing"),
         DCSREAS = ifelse(DCSREAS != "", DCSREAS, "NA"),
         DCSREASP = ifelse(DCSREASP != "", DCSREASP, "NA"))
ASL <- ASL[1:100,] %>% mutate(USUBJID = substr(USUBJID, 15,19))
AAE <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adae.sas7bdat")
AAE <- ASL %>% select(USUBJID) %>%
  left_join(AAE %>% mutate(USUBJID = substr(USUBJID, 15,19)), "USUBJID")  %>%
  mutate(EOSSTT = ifelse(EOSSTT == "Discontinued", EOSSTT, "Ongoing"),
         DCSREAS = ifelse(DCSREAS != "", DCSREAS, "NA"),
         DCSREASP = ifelse(DCSREASP != "", DCSREASP, "NA"),
         AEBODSYS = ifelse(AEBODSYS == "", "UNCODED", AEBODSYS),
         AESOC = ifelse(AESOC == "", "UNCODED", AESOC),
         AEDECOD = ifelse(AEDECOD == "", "UNCODED", AEDECOD))
ATR <- radam("ATR", N = 20)
ATR <- ATR %>% mutate(USUBJID = rep(unique(ASL$USUBJID)[1:20], each = 8)) %>%
  mutate(STUDYID = rep(ASL$STUDYID[1], each = 160))

ARS <- read.bce("/opt/BIOSTAT/qa/s30103j/libraries/xars.sas7bdat") %>%
  filter(PARAMCD == "OVRINV") %>%
  select(USUBJID, ADY, AVALC)
ARS <- ARS %>%
  mutate(USUBJID = rep(unique(ASL$USUBJID), length.out = nrow(ARS)),
         STUDYID = rep(ASL$STUDYID[1], each = nrow(ARS)))
ADS <- ASL %>%
  filter(EOSSTT == "Discontinued" | !is.na(EOSDY)) %>%
  mutate( ADY = EOSDY, AVALC = DCSREAS) %>%
  select(USUBJID, ADY, AVALC, STUDYID)
ARS <- rbind(ARS, ADS)

ARS <- ASL %>% inner_join(ARS, c("USUBJID", "STUDYID"))


x <- teal::init(
  data = list(ASL = ASL, AAE = AAE, ATR = ATR, ARS = ARS),
  modules = root_modules(
    tm_t_ae_oview(
      label = "AE Overview Table",
      dataname = "AAE",
      arm_var = "ARM",
      arm_var_choices = c("ARM", "ARMCD"),
      total_col = FALSE
    ),
    tm_t_ae(
      label = "Adverse Events Table",
      dataname = "AAE",
      arm_var = "ARM",
      arm_var_choices = c("ARM", "ARMCD"),
      class_var = "AEBODSYS",
      class_var_choices = c("AEBODSYS", "AESOC"),
      term_var = "AEDECOD",
      term_var_choices = c("AEDECOD", "AETERM"),
      total_col = TRUE
    ),
    tm_t_ds(
      label = "Patient Disposition Table",
      dataname = "ASL",
      arm_var = "ARM",
      arm_var_choices = c("ARM", "ARMCD"),
      class_var = "EOSSTT",
      class_var_choices = "EOSSTT",
      term_var = "DCSREAS",
      term_var_choices = c("DCSREAS", "DCSREASP"),
      total_col = TRUE
    ),
    tm_g_spiderplot(
      label = "Spiderplot",
      dataname = "ATR",
      paramcd = "SUMTGLES",
      paramcd_choices = c("SUMTGLES", "LDIAM"),
      x_var = "TUDY",
      x_var_choices = c("None", "TUDY"),
      y_var = "PCHG",
      y_var_choices = c("None", "PCHG"),
      marker_var = "RACE",
      marker_var_choices = c("None", "RACE"),
      marker_colorby_var = "RACE",
      marker_colorby_var_choices = c("None", "RACE"),
      line_colorby_var = "USUBJID",
      line_colorby_var_choices = c("USUBJID", "RACE"),
      vref_line = c(10, 37),
      href_line = c(-30, 30),
      anno_txt_var = TRUE,
      anno_disc_study = TRUE,
      legend_on = FALSE,
      xfacet_var = "SEX",
      xfacet_var_choices = c("None", "SEX"),
      yfacet_var = "ARM",
      yfacet_var_choices = c("None", "ARM"),
      plot_height = c(600, 200, 2000)
    ),
    tm_g_swimlane(
      label = "Swimlane Plot",
      dataname = 'ARS',
      bar_var = "TRTDURD",
      bar_var_choices = c("TRTDURD", "AGE"),
      bar_color_var = "None",
      bar_color_var_choices = c("None", "ARM", "ARMCD"),
      sort_var = "ARM",
      sort_var_choices = c("None", "ARM", "TRTDURD"),
      marker_shape_var = "None",
      marker_shape_var_choices = c("None", "AVALC", "AVISIT"),
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15,
                           "DEATH" = 8, "LOST TO FOLLOW-UP" = 10, "WITHDRAWAL BY SUBJECT" = 14),
      marker_color_var = "None",
      marker_color_var_choices = c("None", "AVALC", "AVISIT"),
      marker_color_opt = c("CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red",
                           "DEATH" = "black", "LOST TO FOLLOW-UP" = "purple", "WITHDRAWAL BY SUBJECT" = "darkred"),
      vref_line = c(100, 200),
      anno_txt_var = c("SEX", "RACE", "COUNTRY")

    )
  )
)

shinyApp(x$ui, x$server)