##########################################################
# Name of file: 03a_Get_Matched_TND.R
# Data release (if applicable):
# Original author(s): Chris Robertson chris.robertson@phs.scot
# Original date: 25 Aug 2021
# Latest update author (if not using version control) - Chris Robertson chris.robertson@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: creates a data set for a matched TND study
#                         matching on date of test
#                         run 01a_Vaccinations_Input.R to get all the data sets
# Approximate run time: Unknown
##########################################################

set.seed(0)

output_list <- list()
output_list$endpoint <- "pos_test"
a_begin <- a_begin_16_17  #beginning of vaccination for 16 - 17 year olds
output_list$a_begin <- a_begin
output_list$event_date <- "symptom" # "specimen"
a_end = as.Date("2022-04-18")

print("Total number of tests in the database")
print(nrow(cdw_full))

print("Number of tests within time period")
z_2 = cdw_full %>% filter(age >= 12 & age <= 17) %>% 
             filter(test_result_record_source == "NHS DIGITAL") %>% #omit non community cases
             mutate(date_onset_of_symptoms = as_date(date_onset_of_symptoms)) %>% 
             mutate(date_onset_of_symptoms = if_else(is.na(date_onset_of_symptoms), date_ecoss_specimen -5, date_onset_of_symptoms)) %>% 
             mutate(date_onset_of_symptoms = if_else(!is.na(date_onset_of_symptoms) & date_onset_of_symptoms > date_ecoss_specimen, date_ecoss_specimen -5, date_onset_of_symptoms)) %>% 
             filter(date_onset_of_symptoms >= date_ecoss_specimen-7)
z_2$event_date <- if (output_list$event_date == "symptom") z_2$date_onset_of_symptoms else z_2$date_ecoss_specimen

z_2 <- z_2 %>% filter(event_date >= a_begin_16_17 & event_date <= a_end)
z_x = nrow(z_2)
print(z_x)

#only use symptomatic tests
# Take only events from the beginning of vaccination for each age group
# and before the end of the study
z <- cdw_full %>%  filter(age >= 12 & age <= 17) %>% 
  filter(test_result_record_source == "NHS DIGITAL") %>% #omit non community cases
  filter(flag_covid_symptomatic == "true") %>% 
  dplyr::select(EAVE_LINKNO, test_id, subject_sex, age, date_ecoss_specimen, test_result, date_onset_of_symptoms) %>% 
  dplyr::rename(age_at_test=age) %>% 
  mutate(date_onset_of_symptoms = as_date(date_onset_of_symptoms)) %>% 
  mutate(date_onset_of_symptoms = if_else(is.na(date_onset_of_symptoms), date_ecoss_specimen -5, date_onset_of_symptoms)) %>% 
  mutate(date_onset_of_symptoms = if_else(!is.na(date_onset_of_symptoms) & date_onset_of_symptoms > date_ecoss_specimen, date_ecoss_specimen -5, date_onset_of_symptoms)) %>% 
  filter(date_onset_of_symptoms >= date_ecoss_specimen-7) # select those with symptom onset within previous 7 days of test
z$event_date <- if (output_list$event_date == "symptom") z$date_onset_of_symptoms else z$date_ecoss_specimen

z <- z %>% filter(event_date >= a_begin_16_17 & event_date <= a_end)

print("Total number of symptomatic community cases within our age specification")
print(nrow(z))
print("Number of tests excluded")
print(z_x - nrow(z))


print("Cases within our time period")
print(nrow(z))

#link in sgene status
z_sg <- sgene %>% dplyr::select(test_id, sgene_classification)  #EAVE_LINKNO is missing for about 10% of sgene - there are a few cases where the date ecoss specimen is not the same (273/59357)
z <- z %>%  left_join(z_sg, by="test_id")
#z <- z %>% filter(age_year >=  65)
print(table(z$test_result, z$sgene_classification, exclude=NULL))
#s gene is only from Nov 01 onwards - samples before this are all S positive
#now select those who are in EAVE cohort  
z_x = nrow(z)

z <- z %>% filter(EAVE_LINKNO %in% df_cohort$EAVE_LINKNO)
z <- z %>% left_join(dplyr::select(df_cohort, EAVE_LINKNO, Council, ageYear), by="EAVE_LINKNO")
z <- z %>% filter(Council != "Unknown")  # omit those with unknown location

#z <- z %>% mutate(age_gp = cut(age_year, breaks=c(-1,64,max(age_year) )))
#z <- z %>% mutate(age_gp = "0-100")

print("Removing those not in the EAVE cohort or without a location")
print(z_x - nrow(z))
print("Resulting number of rows")
print(nrow(z))

print("Removing those with inconsistent records")
z_x = nrow(z)
# Join the vaccinations in
z <- z %>% left_join(Vaccinations, by="EAVE_LINKNO") %>%
  filter(is.na(flag_incon) | flag_incon==0) %>% #omit those vaccinated with inconsistent records
  filter(vacc_type == "PB" | is.na(vacc_type)) %>%
  #correct missing date_vacc_2 - need to clean up Vaccinations - chaked and the vacc 3 looks OK
  mutate(date_vacc_2 = if_else(!is.na(date_vacc_2) & !is.na(date_vacc_3) & date_vacc_2==date_vacc_3, date_vacc_1+77, date_vacc_2)) %>%
  filter(!(!is.na(date_vacc_2) & !is.na(date_vacc_3) & date_vacc_3 <= date_vacc_2 + 28)) %>% 
  mutate(day_1 = as.numeric(event_date - date_vacc_1),
         day_2 = as.numeric(event_date - date_vacc_2),
         day_3 = as.numeric(event_date - date_vacc_3)) 

print(z_x - nrow(z))

# Keep a copy of this to use for analyzing hospitalisations
df_tests_hosp = z

# Remove anyone who was vaccinated before the start of the vaccination program
z_x = nrow(z)
z = z %>% filter(date_vacc_1 >= a_begin | is.na(date_vacc_1))
print("Removing anyone who was vaccinated before the start of the program")
print(z_x - nrow(z))
print("12-15 year olds who got a booster")
z_x = nrow(z)
# Anyone who got a booster is immunocomprimised
z = z %>% filter(is.na(date_vacc_3))
print(z_x - nrow(z))
print("Removing anyone who was vaccinated before the start of the program - resulting number of rows")
print(nrow(z))

#get the first positive test in the period
z_pos <- z %>% filter(test_result=="POSITIVE")
print("Number of positive tests")
print(nrow(z_pos))
z_x = nrow(z_pos)
z_pos = z_pos %>% arrange(EAVE_LINKNO, event_date) %>% 
  filter(!duplicated(EAVE_LINKNO))

print("Removing any positive tests after the first positive test")
print(nrow(z_pos))
print("Number of positive tests removed")
print(z_x - nrow(z_pos))

#negative tests - some of the positive can be validly in if the negative test is before the positive
#omit all negatives after the positive
z_neg <- z %>% filter(test_result=="NEGATIVE") %>% 
  left_join(dplyr::select(z_pos, EAVE_LINKNO, event_date), by="EAVE_LINKNO", suffix=c("","_pos"))

print("Number of negative tests")
print(nrow(z_neg))
z_x = nrow(z_neg)
z_neg <- z_neg %>% filter(is.na(event_date_pos) | event_date_pos > event_date) %>% 
  dplyr::select(-event_date_pos)

print("Removing any negative tests after the first positive - number left")
print(nrow(z_neg))
print("Number of negative tests removed")
print(z_x - nrow(z_neg))

z <- bind_rows(z_pos,z_neg)
table(nchar(z$test_id), exclude=NULL) # all have a test_id

print("Number of tests both positive and negative")
print(nrow(z))

z_merge <- dplyr::select(z_neg, EAVE_LINKNO, event_date, Council, age_at_test, test_id) %>% 
  left_join(dplyr::select(z_pos, EAVE_LINKNO, event_date, Council, age_at_test, test_id), by=c("event_date","Council", "age_at_test"), suffix=c("_neg","_pos"))

print("Number of tests in matched dataset")
print(nrow(z_merge))
z_x = nrow(z_merge)
z_merge <- z_merge %>% mutate(random_id = runif(nrow(z_merge)))
z_merge <- z_merge %>% arrange(EAVE_LINKNO_pos, random_id) %>% 
  group_by(EAVE_LINKNO_pos) %>% 
  mutate(id = row_number()) %>% ungroup() %>% 
  filter(id <= 5)

print("Removing any sample with appears more than five times")
print(nrow(z_merge))

print("Number of rows removed:")
print(z_x - nrow(z_merge))

z_t <- z_merge %>% group_by(EAVE_LINKNO_pos) %>% dplyr::summarise(N=n()) %>% ungroup() 
table(z_t$N) # most have 3
table(table(z_merge$EAVE_LINKNO_neg))
table(table(paste(z_merge$event_date,z_merge$EAVE_LINKNO_neg)))
z_t <- z_merge %>% group_by(EAVE_LINKNO_neg) %>% dplyr::summarise(N=n()) %>% ungroup() 
table(z_t$N) # most have 1

z_sel <- !(z_pos$EAVE_LINKNO %in% unique(z_merge$EAVE_LINKNO_pos))
print(table(z_sel))#cases not matched
output_list$no_match <- sum(z_sel)
z_no_match <- filter(z_pos, z_sel)

#put the cases and controls together
z_cc_pos <- z_merge %>% dplyr::select(event_date, Council, EAVE_LINKNO_pos, test_id_pos, age_at_test) %>% 
  filter(!duplicated(EAVE_LINKNO_pos)) %>% 
  mutate(EAVE_LINKNO = EAVE_LINKNO_pos, event=1, test_id=test_id_pos) %>% 
  relocate(EAVE_LINKNO, .before=event_date) %>% 
  dplyr::select(-test_id_pos)

z_cc_neg <- z_merge %>% dplyr::select(EAVE_LINKNO_neg, event_date, Council, EAVE_LINKNO_pos, test_id_neg, age_at_test) %>% 
  mutate(event=0) %>% 
  dplyr::rename(EAVE_LINKNO = EAVE_LINKNO_neg, test_id=test_id_neg) %>% 
  relocate(test_id, .after=last_col())
#some negatives will become positive in the future.

print("Number of positive rows")
print(nrow(z_cc_pos))

print("Number of negative rows")
print(nrow(z_cc_neg))

z_cc <- bind_rows(z_cc_pos, z_cc_neg) %>% 
  arrange(EAVE_LINKNO_pos, desc(event))  
print(table(z_cc$event))

print("Number of tests after combining")
print(nrow(z_cc))

z_cc <- z_cc %>% left_join(dplyr::select(z, test_id, sgene_classification), by="test_id")
table(z_cc$event, z_cc$sgene_classification, exclude=NULL)

#using the time of event
#z_n_pv_pers <- 13
df_cc <- z_cc %>% left_join(Vaccinations, by="EAVE_LINKNO") %>%
  filter(is.na(flag_incon) | flag_incon==0) %>% #omit those vaccinated with inconsistent records
  #correct missing date_vacc_2 - need to clean up Vaccinations - chaked and the vacc 3 looks OK
  mutate(date_vacc_2 = if_else(!is.na(date_vacc_2) & !is.na(date_vacc_3) & date_vacc_2==date_vacc_3, date_vacc_1+77, date_vacc_2)) %>%
  filter(!(!is.na(date_vacc_2) & !is.na(date_vacc_3) & date_vacc_3 <= date_vacc_2 + 28)) %>% 
  mutate(day_1 = as.numeric(event_date - date_vacc_1),
         day_2 = as.numeric(event_date - date_vacc_2),
         day_3 = as.numeric(event_date - date_vacc_3)) 
z <- z %>% mutate(day_1 = if_else(!is.na(day_1) & day_1 <= 0, NA_real_, day_1),
                  day_2 = if_else(!is.na(day_2) & day_2 <= 0, NA_real_, day_2),
                  day_3 = if_else(!is.na(day_3) & day_3 <= 0, NA_real_, day_3))
z_min_day_2 <- min(c(min(df_cc$day_1, na.rm=T)-1), -100) #just in case there are no second vaccs
df_cc <- df_cc %>%   mutate(vacc_1_gp = cut(day_1, breaks= c( 0, 13, 41, 69, 97, 125, max(day_1, na.rm=T)),labels=FALSE),
            vacc_2_gp = cut(day_2, breaks= c(0, 13, 41, 69, 97, max(day_2, na.rm=T)),labels=FALSE))
            #vacc_3_gp = cut(day_3, breaks= c( 0, 13,  max(day_3, na.rm=T)),labels=FALSE))
df_cc <- df_cc %>% mutate(vacc_1_gp = case_when(is.na(vacc_1_gp) ~ "v1_uv",
                                                vacc_1_gp==1 ~ "v1_0:1",
                                                vacc_1_gp==2 ~ "v1_2:5",
                                                vacc_1_gp==3 ~ "v1_6:9",
                                                vacc_1_gp==4 ~ "v1_10:13",
                                                vacc_1_gp==5 ~ "v1_14:17",
                                                vacc_1_gp==6 ~ "v1_18+") )
df_cc <- df_cc %>% mutate(vacc_2_gp = case_when(is.na(vacc_2_gp) ~ "v2_uv",
                                                       vacc_2_gp==1 ~ "v2_0:1",
                                                       vacc_2_gp==2 ~ "v2_2:5",
                                                       vacc_2_gp==3 ~ "v2_6:9",
                                                       vacc_2_gp==4 ~ "v2_10:13",
                                                       vacc_2_gp==5 ~ "v2_14+" ) )
#df_cc <- df_cc %>% mutate(vacc_3_gp = case_when(is.na(vacc_3_gp) ~ "v3_uv",
#                                                       vacc_3_gp==1 ~ "v3_0:1",
#                                                       vacc_3_gp==2 ~ "v3_2+") )

df_cc <- df_cc %>% mutate(vs=case_when(#vacc_3_gp != "v3_uv" ~ vacc_3_gp,
                                       vacc_2_gp != "v2_uv" ~ vacc_2_gp,
                                       TRUE ~ vacc_1_gp)) %>% 
  mutate(vs = if_else(vs=="v1_uv", "uv",vs))

#z_labs <- sort(unique(df_cc$vs))
z_labs <- c("uv" ,"v1_0:1", "v1_2:5", "v1_6:9", "v1_10:13", "v1_14:17", "v1_18+",
            "v2_0:1", "v2_2:5", "v2_6:9", "v2_10:13", "v2_14+")#, "v3_0:1", "v3_2+" )
df_cc <- df_cc %>% mutate(vs=factor(vs, levels=z_labs))

table(df_cc$vs, df_cc$event)

#add in days from event data to latest positive test before a_begin
z <- Positive_Tests %>% filter(date_ecoss_specimen < a_begin) %>% arrange(EAVE_LINKNO, desc(date_ecoss_specimen) ) %>% 
  filter(!duplicated(EAVE_LINKNO)) 
z1 <- df_cc %>% left_join(z, by="EAVE_LINKNO") %>% 
  mutate(days_from_previous_pos = as.numeric(event_date - date_ecoss_specimen)) %>% 
  mutate(days_from_previous_pos = if_else(!is.na(days_from_previous_pos) & days_from_previous_pos <=0, NA_real_, days_from_previous_pos))
z1 <- z1 %>% mutate(pos_before_test = case_when(is.na(days_from_previous_pos) ~ "not_prev_pos",
                                             days_from_previous_pos >= 1 & days_from_previous_pos <= 28 ~ "pos_1:28",
                                             days_from_previous_pos >= 29 & days_from_previous_pos <= 90 ~ "pos_29:90",
                                             TRUE ~ "pos_91+")) %>% 
  dplyr::select(-date_ecoss_specimen)
table(z1$pos_before_test, z1$event,exclude=NULL)
z1$pos_before_test = factor(z1$pos_before_test)
df_cc <- z1

#add in number of prior tests
#get number of tests before vaccination variable
z <- df_cc %>% dplyr::select(EAVE_LINKNO, event_date)
z <- z %>% left_join(dplyr::select(cdw_full, EAVE_LINKNO, date_ecoss_specimen), by="EAVE_LINKNO") %>%
  filter(!is.na(date_ecoss_specimen)) %>% #omit those never tested
  filter(date_ecoss_specimen < event_date) #keep those sample before vaccination
z <- z %>% group_by(EAVE_LINKNO, event_date) %>% dplyr::summarise(N=n()) 
#now link back
df_cc <- df_cc %>% left_join(z, by=c("EAVE_LINKNO","event_date")) %>% 
  mutate(N=if_else(is.na(N), 0L,N))
df_cc <- df_cc %>% mutate(n_tests_gp = cut(N, breaks=c(-1,0,1,2,3,4,9,19, max(N)), labels=c("0","1","2","3","4","5-9","10-19","20+")))
df_cc$N <- NULL 
table(df_cc$n_tests_gp, df_cc$event, exclude=NULL)

# See if they tested positive before they were vaccinated
z <- df_cc %>% dplyr::select(EAVE_LINKNO, event_date)
z <- z %>% left_join(dplyr::select(cdw_full, EAVE_LINKNO, date_ecoss_specimen, test_result), by="EAVE_LINKNO") %>%
  filter(!is.na(date_ecoss_specimen)) %>% #omit those never tested
  filter(date_ecoss_specimen < event_date) #keep those sample before vaccination

# Limit to 90 days before the event
#z = z %>% mutate(test_result_pos = test_result == "POSITIVE") %>% filter(event_date - date_ecoss_specimen < 90)
#z = z %>% group_by(EAVE_LINKNO) %>% summarize(pos_before_vacc = any(test_result_pos)) %>% 
#  if_else(is.na(pos_before_vacc), FALSE, pos_before_vacc)
#
#df_cc <- df_cc %>% left_join(z, by=c("EAVE_LINKNO"))

table(df_cc$vs, df_cc$event)

# Add in the other QCovid risk groups
z_vars <- names(df_cc)[grepl("Q_DIAG", names(df_cc))]
z_vars <- z_vars[!(z_vars %in% variables_hosp)]
z_vars <- z_vars[!(z_vars %in% c("Q_DIAG_DIABETES_1"  ,  "Q_DIAG_DIABETES_2" , "Q_DIAG_CKD_LEVEL"))]
#z_vars <- c(z_vars, "Q_HOME_CAT","Q_LEARN_CAT")  - no homeless children and learn cat included

z <- df_cc %>% dplyr::select_at(c("EAVE_LINKNO", z_vars))
z <- z %>% mutate(N = apply(z[,z_vars], 1, sum))
z <- z %>% mutate(n_oth_risk_gps = cut(N, breaks=c(-1,0,max(1, N)), labels=c("0","1+")) )
z <- z %>% dplyr::select(EAVE_LINKNO, n_oth_risk_gps)
z = z %>% filter(!duplicated(EAVE_LINKNO))

print(nrow(df_cc))

df_cc <- df_cc %>% left_join(z, by="EAVE_LINKNO")

print(nrow(df_cc))

z <- df_cc  %>% left_join(df_cohort, by="EAVE_LINKNO")  #eave_weight is 1 for all

df_cc <- z

df_cc <- df_cc %>% mutate(shielding = if_else(EAVE_LINKNO %in% shielding$EAVE_LINKNO, 1, 0))
df_cc <- df_cc %>% mutate(immuno = if_else(EAVE_LINKNO %in% immuno$EAVE_LINKNO, 1, 0))

#z <- df_cc %>% left_join(dplyr::select(cdw_full, test_id, care_home_role), by="test_id") # no care home staff/residents tested in lighhouse in Nov

df_cc <- df_cc %>% 
  mutate(vacc_type = if_else(is.na(vacc_type) , "uv", vacc_type) ) %>% 
  filter(vacc_type != "UNK") %>% 
  mutate(vacc_type = factor(vacc_type, levels=c("uv","AZ","Mo","PB") ) ) %>% 
  mutate(vt = fct_cross(vs,vacc_type, sep="_")) %>% 
  mutate(vt = fct_recode(vt, "uv" ="uv_uv", "uv" = "uv_AZ", "uv" = "uv_Mo","uv" = "uv_PB"))

# Keep the old copy of the dataframe so we can look at how the sgene status changes over time
df_s_gene = df_cc

#change sgene classification to be S positive for those before Nov 01 when ct values start
z <- df_cc %>% 
  mutate(sgene_classification = if_else(event_date < as_date("2021-11-01") &
                                          event==1, "Positive S Gene", sgene_classification) )

z <- z %>% 
  mutate(sgene_classification = if_else(event_date > as_date("2022-01-15") &
                                          event==1, "True S Gene Dropout", sgene_classification) )

z <- z %>%  
  mutate(event_s_neg = if_else(event==1 & !is.na(sgene_classification) & sgene_classification=="True S Gene Dropout", 1,0),
         event_s_pos = if_else(event==1 & !is.na(sgene_classification) & sgene_classification=="Positive S Gene", 1,0))

print(table(z$event, z$sgene_classification, exclude=NULL))
print(table(z$event_s_pos, z$sgene_classification, exclude=NULL))
print(table(z$event_s_neg, z$sgene_classification, exclude=NULL))
df_cc <- z



#correct the vaccine type for people vaccinated after the event date - vs is status at event date
z <- df_cc %>% 
  mutate(vacc_type = if_else(vs !="uv", vacc_type ,factor("uv" , levels=levels(df_cc$vacc_type)))) %>% 
  mutate(vacc_type_3 = if_else(!grepl("^v3", vs), NA_character_ , vacc_type_3)) 

df_cc <- z 

z = df_cc %>% filter(is.na(ageYear))

print(nrow(df_cc %>% filter(is.na(ageYear))))

df_cc <- df_cc %>% filter(!is.na(ageYear)) # one with an unknown age from linking to df_cohort

print("Removing anyone without an age in the cohort")
print(nrow(df_cc))



# We don't have any events for delta in the longer time periods, therefore we want to compress 
# the post second vaccination categogies down
z_df_cc_delta = df_cc %>% filter(event_s_pos == 1)
z_df_cc_delta = z_df_cc_delta %>% mutate(vs = recode_factor(vs, "v2_10:13" = "v2_6:9", "v2_14+" = "v2_6:9"))

z_df_cc_delta_neg = df_cc %>% filter(EAVE_LINKNO_pos %in% z_df_cc_delta$EAVE_LINKNO & event_s_pos == 0)
z_df_cc_delta_neg = z_df_cc_delta_neg %>% mutate(vs = recode_factor(vs, "v2_10:13" = "v2_6:9", "v2_14+" = "v2_6:9"))

z_df_cc_not_delta = df_cc %>% filter(event_s_pos == 0 & !(EAVE_LINKNO_pos %in% z_df_cc_delta$EAVE_LINKNO))

z_df_cc = z_df_cc_delta %>% bind_rows(z_df_cc_delta_neg) %>% bind_rows(z_df_cc_not_delta)

# Recreate the vs order from
z_df_cc$vs = factor(z_df_cc$vs, levels = c("uv", "v1_0:1", "v1_2:5", "v1_6:9", "v1_10:13", "v1_14:17", "v1_18+", "v2_0:1", "v2_2:5", "v2_6:9", "v2_10:13", "v2_14+"))

df_cc = z_df_cc

df_cc_12_15 = df_cc %>% filter(age_at_test <= 15 & age_at_test >= 12) %>% mutate(age_gp = factor(age_at_test))
df_cc_16_17 = df_cc %>% filter(age_at_test == 16 | age_at_test == 17) %>% mutate(age_gp = factor(age_at_test))

df_cc_12_15$vs = droplevels(df_cc_12_15$vs)

print("Number of rows for 12-15")
print(nrow(df_cc_12_15))

print("Number of controls 12 - 15")
print(nrow(df_cc_12_15 %>% filter(event == 0)))

print("Number of cases 12 - 15")
print(nrow(df_cc_12_15 %>% filter(event == 1)))
      
print("Number of s gene cases 12 - 15")
print(nrow(df_cc_12_15 %>% filter(event_s_neg == 1 | event_s_pos == 1)))

print("Number of rows for 16 and 17")
print(nrow(df_cc_16_17))

print("Number of controls 16 - 17")
print(nrow(df_cc_16_17 %>% filter(event == 0)))
   
print("Number of s gene cases 16 - 17")
print(nrow(df_cc_16_17 %>% filter(event_s_neg == 1 | event_s_pos == 1)))
   
print("Number of cases 16 - 17")
print(nrow(df_cc_16_17 %>% filter(event == 1)))

saveRDS(df_cc, paste0("./output/temp/df_cc_",output_list$endpoint,".RDS"))

