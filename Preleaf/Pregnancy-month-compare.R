library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)
library(tidyr)

anm_visit <- read.csv("anm_visit_detail.csv", stringsAsFactors = FALSE, header = TRUE,
                      na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))
str(anm_visit)
summary(anm_visit)

# The following variables represent categories and not character or integers.
# Hence they need to be converted to categories
categorical <- c("anm_visit_detail_breathlessness", "anm_visit_detail_urine",
                 "anm_visit_detail_abdomen_pain", "anm_visit_detail_edema",
                 "anm_visit_detail_body_edema", "anm_visit_detail_headache")

#Converting to categorical variables

anm_visit[, categorical] <- lapply(anm_visit[, categorical], as.factor)
str(anm_visit[, categorical])

#Following are date variables and should be changed to date
anm_visit$anm_visit_detail_followup_date <- ymd(anm_visit$anm_visit_detail_followup_date)

#Following are datetime variables and should be changed to datetime
datetimevars <- c("anm_visit_detail_form_start_time", "anm_visit_detail_form_fill_time",
                  "anm_visit_detail_receive_time")

anm_visit[, datetimevars] <- lapply(anm_visit[, datetimevars], ymd_hms)
str(anm_visit[, datetimevars])

#Following are numeric variables and should be changed to numeric
numvars <- c("anm_visit_detail_temp", "anm_visit_detail_gps_lat", "anm_visit_detail_gps_long")

anm_visit[, numvars] <- lapply(anm_visit[, numvars], as.numeric)
str(anm_visit[, numvars])

#---------------------pw case close-----------------------------------
pw_case_flag <- read.csv("pw_case_flag.csv", stringsAsFactors = FALSE, header = TRUE,
                         na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))

str(pw_case_flag)
summary(pw_case_flag)

#Following are date variables and should be changed to date
pw_case_flag$anm_visit_detail_followup_date <- ymd(pw_case_flag$anm_visit_detail_followup_date)
pw_case_flag$doc_visit_detail_followup_date <- ymd(pw_case_flag$doc_visit_detail_followup_date)

#Following are datetime variables and should be changed to datetime
datetimevars <- c("pw_case_flag_set_time", "doc_visit_detail_last_visit_date")

pw_case_flag[, datetimevars] <- lapply(pw_case_flag[, datetimevars], ymd_hms)
str(pw_case_flag[, datetimevars])

length(unique(pw_case_flag$pw_profile_id))
#There are 43783 unique pw ids 

pw_preg_all <- merge(pw_case_flag, anm_visit, by = "pw_profile_id", all.x = TRUE)

str(pw_preg_all)

pw_preg_closed <- sqldf("select * from pw_preg_all where pw_case_flag_closed = 1")

#------------pw_profile----------------------------------------------------
pw_profile <- read.csv("pw_profile.csv", stringsAsFactors = FALSE, 
                       na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))

#1. Pregnant women profile
str(pw_profile)
# The following variables represent categories and not character or intgers
categorical <- c("pw_profile_bpl_card", "pw_profile_edu", "pw_profile_type_uid",
                 "pw_profile_first_preg", "pw_profile_birth_order", "pw_profile_prev_pih",
                 "pw_profile_prev_sb", "pw_profile_prev_low_wt", "pw_profile_prev_miscarriage",
                 "pw_profile_prev_c_section", "pw_profile_prev_pph", "pw_profile_heart_disease",
                 "pw_profile_thyroid_disease", "pw_profile_epilepsy", "pw_profile_reproductive_surg",
                 "pw_profile_other_flag", "pw_profile_family_diabetes", "pw_profile_family_pih",
                 "pw_profile_multi_preg", "pw_profile_blood_group", "pw_profile_hiv")

#Converting to categorical variables
pw_profile[, categorical] <- lapply(pw_profile[, categorical], as.factor)
str(pw_profile[, categorical])

#Following are date variables and should be changed to date
datevars <- c("pw_profile_dob", "pw_profile_lmp", "pw_profile_edd")

pw_profile[, datevars] <- lapply(pw_profile[, datevars], ymd)
str(pw_profile[, datevars])

#Following are datetime variables and should be changed to datetime
datetimevars <- c("pw_profile_form_start_time", "pw_profile_form_fill_time", "pw_profile_reg_date")

pw_profile[, datetimevars] <- lapply(pw_profile[, datetimevars], ymd_hms)
str(pw_profile[, datetimevars])

#------------------pw_case close--------------------------------------------

pw_case_close <- read.csv("pw_case_close.csv", stringsAsFactors = FALSE, header = TRUE,
                          na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))
str(pw_case_close)
summary(pw_case_close)

# The following variables represent categories and not character or integers.
# Hence they need to be converted to categories
categorical <- c("pw_case_close_preg_outcome", "pw_case_close_delivery_outcome",
                 "pw_case_close_delivery_place", "pw_case_close_mother_status",
                 "pw_case_close_child_sex", "pw_case_close_delivery_type",
                 "pw_case_close_delivery_assistant")

#Converting to categorical variables
pw_case_close[, categorical] <- lapply(pw_case_close[, categorical], as.factor)
str(pw_case_close[, categorical])

#Following are date variables and should be changed to date
pw_case_close$pw_case_close_outcome_date <- ymd(pw_case_close$pw_case_close_outcome_date)

#Following are datetime variables and should be changed to datetime
datetimevars <- c("pw_case_close_date", "pw_case_close_receive_time")

pw_case_close[, datetimevars] <- lapply(pw_case_close[, datetimevars], ymd_hms)
str(pw_case_close[, datetimevars])

delivery_complete <- sqldf("select * from pw_case_close where pw_case_close_delivery_outcome = 1")

delivery_full <- merge(delivery_complete, pw_preg_closed, by = "pw_profile_id", all.x = TRUE)

#delivery_full <- merge(delivery_full, pw_profile, by = "pw_profile_id", all.x = TRUE)

#delivery_full <- delivery_full %>% filter(is.na(first_record))

delivery_registration <- sqldf("select * from delivery_full where first_record = 1")

#delivery_full <- delivery_full %>% select(-ends_with(".y"))
# delivery_registration <- delivery_full %>% 
#                          select(-ends_with(c(".x",".y"))) %>%
#                          filter(first_record == 1)

delivery_registration <- delivery_registration %>% mutate(calc_date = anm_visit_detail_receive_time %m+% months(9 - anm_visit_detail_preg_month),
                                                          calc_month = month(anm_visit_detail_receive_time %m+% months(9 - anm_visit_detail_preg_month)),
                                                          actual_month = month(pw_case_close_outcome_date),
                                                          diff_date = round(difftime(pw_case_close_outcome_date, calc_date, units = "days")),
                                                          diff_month = actual_month - calc_month)

delivery_reg_full <- merge(delivery_registration, pw_profile, by = "pw_profile_id", all.x = TRUE)

delivery_reg_full <- delivery_reg_full[, !colnames(delivery_reg_full) %in% c("qrcode.x", "qrcode.y")]

final_delivery <- delivery_reg_full %>% 
                  select(pw_profile_id, pw_profile_name,
                         pw_profile_lmp, pw_profile_edd, anm_visit_detail_preg_month,
                         anm_visit_detail_receive_time, calc_month, pw_case_close_outcome_date,
                         diff_date, diff_month)

write.csv(final_delivery, "Delivery_date_comparison.csv", row.names = FALSE)
