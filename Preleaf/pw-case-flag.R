library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(sqldf)

#Read in anm_visit file
anm_visit <- read.csv("anm_complete.csv", stringsAsFactors = FALSE, header = TRUE,
                   na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))
str(anm_visit)
summary(anm_visit)

#Read in pw case close file
pw_case_close <- read.csv("pw_case_close-03-mar.csv", stringsAsFactors = FALSE, header = TRUE,
                          na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))

str(pw_case_close)
summary(pw_case_close)

closed_cases <- unique(pw_case_close$pw_profile_id)

#------Data cleaning anm visit-------------------------------------------

#Renaming the NAs System risk to No risk
anm_visit$system_risk_reason[is.na(anm_visit$system_risk_reason)] <- "No Risk"

# The following variables represent categories and not character or integers.
# Hence they need to be converted to categories
categorical <- c("anm_visit_detail_breathlessness", "anm_visit_detail_urine",
                 "anm_visit_detail_abdomen_pain", "anm_visit_detail_edema",
                 "anm_visit_detail_body_edema", "anm_visit_detail_headache",
                 "system_risk_reason")

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

#Remove profiles whose cases are closed
pw_case_open <- anm_visit[!(anm_visit$pw_profile_id %in% closed_cases),]

#Summarising by various risk reasons
risk_combine <- pw_case_open %>% group_by(pw_profile_id) %>%
                arrange(desc(anm_visit_detail_id)) %>%
                distinct(pw_profile_id, .keep_all = TRUE) %>%
                # filter(risk_status == 2 | risk_status == 3)
                group_by(system_risk_reason) %>%
                tally(sort = TRUE) 

length(unique(pw_case_open$pw_profile_id))

sum(risk_combine$n)

colnames(risk_combine) <- c("Risk-Combinations", "Number-of-Cases")

write.csv(risk_combine, "Risk-Combinations-4.csv", row.names = FALSE)

#-------------------------------------------------------------

#Read in pw_case_flag file
pw_case_flag <- read.csv("pw_case_flag.csv", stringsAsFactors = FALSE, header = TRUE,
                      na.strings=c("NULL","NULL "," NULL ","NA"," ","", "-1"))

sqldf("select count(distinct(pw_profile_id)) from pw_case_flag where pw_case_flag_risk_status
      in (2,3) and pw_case_flag_closed = 0")

pw_case_flag_profiles <- sqldf("select distinct(pw_profile_id) from pw_case_flag where pw_case_flag_risk_status
                                in (2,3) and pw_case_flag_closed = 0")
nrow(pw_case_flag_profiles)

anm_visit_profiles <- sqldf("select distinct(pw_profile_id) from pw_case_open where risk_status
                             in (2,3)")
nrow(anm_visit_profiles)

missing_from_flag <- sqldf( "select * from anm_visit_profiles where pw_profile_id not in pw_case_flag_profiles" )

missing_risky_from_closed <- sqldf("select distinct(pw_profile_id) from pw_case_flag
                                   where pw_case_flag_closed = 1 and pw_case_flag_risk_status in (2,3)
                                   and pw_profile_id not in (select distinct(pw_profile_id) from pw_case_close)")

sqldf("select * from pw_case_open where pw_profile_id in missing_risky_from_closed")

pw_case_open1 <- pw_case_open[!(pw_case_open$pw_profile_id %in% missing_risky_from_closed$pw_profile_id),]

risk_combine1 <- pw_case_open1 %>% group_by(pw_profile_id) %>%
                 arrange(desc(anm_visit_detail_id)) %>%
                 distinct(pw_profile_id, .keep_all = TRUE) %>%
                 filter(risk_status == 2 | risk_status == 3) %>%
                 group_by(system_risk_reason) %>%
                 tally(sort = TRUE) 
