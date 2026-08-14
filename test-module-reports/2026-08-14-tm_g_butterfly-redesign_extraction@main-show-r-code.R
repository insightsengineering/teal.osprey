set.seed(23) # @linksto ADSL
library(nestcolor)
library(dplyr)
ADSL <- rADSL
ADAE <- rADAE
ADSL <- mutate(ADSL, DOSE = paste(sample(1:3, n(), replace = TRUE), "UG"))
ADAE <- mutate(ADAE, flag1 = ifelse(AETOXGR == 1, 1, 0), flag2 = ifelse(AETOXGR == 2, 1, 0), flag3 = ifelse(AETOXGR == 3, 1, 0), flag1_filt = rep("Y", n()))
stopifnot(rlang::hash(ADSL) == "db2532d6de1ff05e5a1a4667b2e37a77") # @linksto ADSL
stopifnot(rlang::hash(ADAE) == "b48cbc656ec2956e362fabc60221cf36") # @linksto ADAE
.raw_data <- list2env(list(ADSL = ADSL, ADAE = ADAE))
lockEnvironment(.raw_data) # @linksto .raw_data
ADAE <- dplyr::inner_join(x = ADAE, y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE], by = c("STUDYID", "USUBJID"))
ANL <- dplyr::select(ADAE, STUDYID, USUBJID, ASTDTM, AETERM, AESEQ, SEX, RACE, AEBODSYS, AETOXGR)
library(dplyr)
right <- ANL[["SEX"]] %in% "F"
right_name <- paste("F", collapse = " - ")
left <- ANL[["RACE"]] %in% "ASIAN"
left_name <- paste("ASIAN", collapse = " - ")
plot <- osprey::g_butterfly(category = ANL[["AEBODSYS"]], right_flag = right, left_flag = left, group_names = c(right_name, left_name), block_count = "# of patients", block_color = if (!is.null("AETOXGR")) {
    ANL[["AETOXGR"]]
} else {
    NULL
}, id = ANL$USUBJID, facet_rows = if (!is.null(NULL)) {
    ANL[[NULL]]
} else {
    NULL
}, x_label = "# of patients", y_label = "AEBODSYS", legend_label = if (!is.null("AETOXGR")) {
    "AETOXGR"
} else {
    ""
}, sort_by = "count", show_legend = TRUE)
plot
