z = df_tests_hosp %>% filter(test_result == "POSITIVE")

# Join the testing data to the hospitalization data
z = covid_hosp %>% left_join(df_tests_hosp, by="EAVE_LINKNO")

# Remove any non matches
z = z %>% filter(!is.na(test_id))

# Remove any rows after the end of the study period
z = z %>% filter(covid_hosp_date < a_end)

# Were any of these hospitalizations within 21 days of a positive test?
z = z %>% mutate(test_hosp_diff = covid_hosp_date - date_ecoss_specimen) %>%
  filter(test_hosp_diff > 0 & test_hosp_diff < 21) %>% select(-test_hosp_diff)

# How long did they spend in hospital?
z = z %>% mutate(hosp_los = hosp_disc_date - covid_hosp_date)

# Was the person vaccinated before they went to hospital?
z = z %>% mutate(vacc_1_diff = as.numeric(covid_hosp_date - date_vacc_1))
z = z %>% mutate(vacc_2_diff = as.numeric(covid_hosp_date - date_vacc_2))
z = z %>% mutate(vacc_3_diff = as.numeric(covid_hosp_date - date_vacc_3))

z = z %>% mutate(vacc_1_dose = factor(is.na(vacc_1_diff) | vacc_1_diff < 0, labels=c("Vaccinated", "Unvaccinated")))
z = z %>% mutate(vacc_2_dose = factor(is.na(vacc_2_diff) | vacc_2_diff < 0, labels=c("Vaccinated", "Unvaccinated")))
z = z %>% mutate(vacc_3_dose = factor(is.na(vacc_3_diff) | vacc_3_diff < 0, labels=c("Vaccinated", "Unvaccinated")))

# Calculate vaccine status
z = z %>% mutate(day_1 = as.numeric(covid_hosp_date - date_vacc_1),
       day_2 = as.numeric(covid_hosp_date - date_vacc_2),
       day_3 = as.numeric(covid_hosp_date - date_vacc_3)) 
z <- z %>% mutate(day_1 = if_else(!is.na(day_1) & day_1 <= 0, NA_real_, day_1),
                  day_2 = if_else(!is.na(day_2) & day_2 <= 0, NA_real_, day_2),
                  day_3 = if_else(!is.na(day_3) & day_3 <= 0, NA_real_, day_3))

z <- z %>%   mutate(vacc_1_gp = cut(day_1, breaks= c( 0, 13, 41, 69, 97, 125, max(day_1, na.rm=T)),labels=FALSE),
                            vacc_2_gp = cut(day_2, breaks= c(0, 13, 41, 69, 97, max(day_2, na.rm=T)),labels=FALSE))
#vacc_3_gp = cut(day_3, breaks= c( 0, 13,  max(day_3, na.rm=T)),labels=FALSE))
z <- z %>% mutate(vacc_1_gp = case_when(is.na(vacc_1_gp) ~ "v1_uv",
                                                vacc_1_gp==1 ~ "v1_0:1",
                                                vacc_1_gp==2 ~ "v1_2:5",
                                                vacc_1_gp==3 ~ "v1_6:9",
                                                vacc_1_gp==4 ~ "v1_10:13",
                                                vacc_1_gp==5 ~ "v1_14:17",
                                                vacc_1_gp==6 ~ "v1_18+") )
z <- z %>% mutate(vacc_2_gp = case_when(is.na(vacc_2_gp) ~ "v2_uv",
                                                vacc_2_gp==1 ~ "v2_0:1",
                                                vacc_2_gp==2 ~ "v2_2:5",
                                                vacc_2_gp==3 ~ "v2_6:9",
                                                vacc_2_gp==4 ~ "v2_10:13",
                                                vacc_2_gp==5 ~ "v2_14+" ) )
z <- z %>% mutate(vs=case_when(#vacc_3_gp != "v3_uv" ~ vacc_3_gp,
  vacc_2_gp != "v2_uv" ~ vacc_2_gp,
  TRUE ~ vacc_1_gp)) %>% 
  mutate(vs = if_else(vs=="v1_uv", "uv",vs))

z = z %>% left_join(df, by="EAVE_LINKNO", suffix = c("", "_df"))

# Remove anyone who doesn't link in
z = z %>% filter(!is.na(eave_weight))

# Calculate number of readmissions
z = z %>% group_by(EAVE_LINKNO) %>% 
  arrange(EAVE_LINKNO, covid_hosp_date) %>% 
  mutate(num_prev_admissions = row_number()) %>% ungroup()

z = z %>% mutate(num_prev_admissions = num_prev_admissions - 1)

df_eave_hosp = z