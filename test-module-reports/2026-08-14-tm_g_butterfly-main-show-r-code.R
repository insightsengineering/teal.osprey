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
library(dplyr)
ADSL <- ADSL[, c("USUBJID", "STUDYID", "SEX", "RACE")] %>% as.data.frame()
ANL <- ADAE[, c("USUBJID", "STUDYID", "AEBODSYS", "AETOXGR")] %>% as.data.frame()
ANL_f <- left_join(ADSL, ANL, by = c("USUBJID", "STUDYID")) %>% as.data.frame()
ANL_f <- na.omit(ANL_f)
right <- ANL_f[, "SEX"] %in% "F"
right_name <- paste("F", collapse = " - ")
left <- ANL_f[, "RACE"] %in% "ASIAN"
left_name <- paste("ASIAN", collapse = " - ")
plot <- osprey::g_butterfly(category = ANL_f[, "AEBODSYS"], right_flag = right, left_flag = left, group_names = c(right_name, left_name), block_count = "# of patients", block_color = ANL_f[, "AETOXGR"], id = ANL_f$USUBJID, facet_rows = NULL, x_label = "# of patients", y_label = "AEBODSYS", legend_label = "AETOXGR", sort_by = "count", show_legend = TRUE)
