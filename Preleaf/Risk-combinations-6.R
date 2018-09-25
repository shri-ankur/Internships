library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(sqldf)

#Read in anm_visit file
pw_case_flag <- read.csv("pw_case_flag_05_apr.csv", stringsAsFactors = FALSE, header = TRUE,
                        na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))
str(pw_case_flag)

#------Data cleaning pw_case_flag-------------------------------------------

#Renaming the NAs in risk reason to No risk
pw_case_flag$risk_reason[is.na(pw_case_flag$risk_reason)] <- "No Risk"

pw_case_flag$risk_reason <- as.factor(pw_case_flag$risk_reason)
str(pw_case_flag$risk_reason)

#Following are date variables and should be changed to date
pw_case_flag$anm_visit_detail_followup_date <- ymd(pw_case_flag$anm_visit_detail_followup_date)
pw_case_flag$doc_visit_detail_followup_date <- ymd(pw_case_flag$doc_visit_detail_followup_date)

#Following are datetime variables and should be changed to datetime
datetimevars <- c("pw_case_flag_set_time", "doc_visit_detail_last_visit_date")

pw_case_flag[, datetimevars] <- lapply(pw_case_flag[, datetimevars], ymd_hms)
str(pw_case_flag[, datetimevars])

#Remove profiles whose cases are closed 
pw_open <- sqldf("select * from pw_case_flag where pw_case_flag_closed = 0")

#Removing profiles risk status is Normal
pw_risk_open <- sqldf("select * from pw_open where pw_case_flag_risk_status != 1")

#Retaining only anm profiles
pw_risk_open_anm <- sqldf("select * from pw_risk_open where anm_doc_visit_detail_id like 'ANM%' ")

#Summarising by various risk reasons
risk_combine_anm <- pw_risk_open_anm %>% group_by(pw_profile_id) %>%
                    arrange(pw_profile_id,desc(anm_doc_visit_detail_id)) %>%
                    distinct(pw_profile_id, .keep_all = TRUE) %>%
                    ungroup() %>%
                    group_by(risk_reason) %>%
                    tally(sort = TRUE) 

length(unique(pw_risk_open_anm$pw_profile_id))

sum(risk_combine_anm$n)

colnames(risk_combine_anm) <- c("Risk-Combinations-anm", "Number-of-Cases")

write.csv(risk_combine_anm, "Risk-Combinations-anm.csv", row.names = FALSE)

#-------Doc visit risk cobinations -------------------------------------------------
#Retaining only doc visit profiles
pw_risk_open_doc <- sqldf("select * from pw_risk_open where anm_doc_visit_detail_id like 'DOC%' ")

#Summarising by various risk reasons
risk_combine_doc <- pw_risk_open_doc %>% group_by(pw_profile_id) %>%
                    arrange(pw_profile_id,desc(anm_doc_visit_detail_id)) %>%
                    distinct(pw_profile_id, .keep_all = TRUE) %>%
                    ungroup() %>%
                    group_by(risk_reason) %>%
                    tally(sort = TRUE) 

length(unique(pw_risk_open_doc$pw_profile_id))

sum(risk_combine_doc$n)

colnames(risk_combine_doc) <- c("Risk-Combinations-doc", "Number-of-Cases")

write.csv(risk_combine_doc, "Risk-Combinations-doc.csv", row.names = FALSE)
