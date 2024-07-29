library(haven)
#install.packages("remotes")
#remotes::install_github("wviechtb/esmpack")
library(esmpack)
#install.packages("vtable")
library(vtable)
#install.packages("mediation")
library(mediation)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("lubridate")
library(lubridate)
#install.packages("readxl")
library(readxl)
#install.packages("anytime")
library(anytime)
#install.packages("purrr")
library(purrr)
#version

# Wczytanie danych z pliku .sav
esm_cortisol_data <- read_sav("data/clear_esm_cortisol_data_20240729_162811.sav")
FKBP5_wywiad_data <- read_sav("data/clear_FKBP5_wywiad_data_20240729_162549.sav")
FKBP5_baseline_data <- read_sav("data/clear_FKBP5_baseline_data_20240729_161840.sav")
screening_data <- read_sav("data/clear_screening_data_20240729_150623.sav")


#DUPLIKATY
#screening_data
screening_data$ID <- toupper(screening_data$ID)
tmp1 <- subset(screening_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))

#FKBP5_wywiad_data
FKBP5_wywiad_data$ID <- toupper(FKBP5_wywiad_data$ID)
tmp2 <- subset(FKBP5_wywiad_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))

#FKBP5_baseline_data
FKBP5_baseline_data$ID <- toupper(FKBP5_baseline_data$ID)
tmp3 <- subset(FKBP5_baseline_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))


rm(tmp1)
rm(tmp2)
rm(tmp3)


#SCAL ID
# Pobranie nazw kolumn dla każdej ramki danych
ID_names_baseline <- FKBP5_baseline_data$ID
ID_names_wywiad <- FKBP5_wywiad_data$ID
ID_names_screening <- screening_data$ID
ID_names_esm_cortisol <- unique(esm_cortisol_data$Participant)

# Znalezienie różnic między kolumnami
#są obecne w pierwszym wektorze, ale nie występują w drugim

#baseline
diff_baseline_wywiad <- setdiff(ID_names_baseline, ID_names_wywiad)
diff_baseline_screening <- setdiff(ID_names_baseline, ID_names_screening)
diff_baseline_esm_cortisol <- setdiff(ID_names_baseline, ID_names_esm_cortisol)
diff_baseline_all <- setdiff(ID_names_baseline, c(ID_names_screening, ID_names_wywiad, ID_names_esm_cortisol))


print("baseline vs wywiad")
print(diff_baseline_wywiad)
print("baseline vs screening")
print(diff_baseline_screening)
print("baseline vs esm_cortisol")
print(diff_baseline_esm_cortisol)
print("baseline vs all")
print(diff_baseline_all)

#wywiad
diff_wywiad_screening <- setdiff(ID_names_wywiad, ID_names_screening)
diff_wywiad_baseline <- setdiff(ID_names_wywiad, ID_names_baseline)
diff_wywiad_esm_cortisol <- setdiff(ID_names_wywiad, ID_names_esm_cortisol)
diff_wywiad_all <- setdiff(ID_names_wywiad, c(ID_names_screening, ID_names_baseline, ID_names_esm_cortisol))


print("wywiad vs baseline")
print(diff_wywiad_baseline)
print("wywiad vs screening")
print(diff_wywiad_screening)
print("wywiad vs esm_cortisol")
print(diff_wywiad_esm_cortisol)
print("wywiad vs all")
print(diff_wywiad_all)

#screening
diff_screening_wywiad <- setdiff(ID_names_screening, ID_names_wywiad)
diff_screening_baseline <- setdiff(ID_names_screening, ID_names_baseline)
diff_screening_esm_cortisol <- setdiff(ID_names_screening, ID_names_esm_cortisol)
diff_screening_all <- setdiff(ID_names_screening, c(ID_names_wywiad, ID_names_baseline, ID_names_esm_cortisol))


print("screening vs baseline")
print(diff_screening_baseline)
print("screening vs wywiad")
print(diff_screening_wywiad)
print("screening vs esm_cortisol")
print(diff_screening_esm_cortisol)
print("screening vs all")
print(diff_screening_all)

#esm_cortisol
diff_esm_cortisol_wywiad <- setdiff(ID_names_esm_cortisol, ID_names_wywiad)
diff_esm_cortisol_baseline <- setdiff(ID_names_esm_cortisol, ID_names_baseline)
diff_esm_cortisol_screening <- setdiff(ID_names_esm_cortisol, ID_names_screening)
diff_esm_cortisol_all <- setdiff(ID_names_esm_cortisol, c(ID_names_wywiad, ID_names_baseline, ID_names_screening))


print("esm_cortisol vs baseline")
print(diff_esm_cortisol_baseline)
print("esm_cortisol vs wywiad")
print(diff_esm_cortisol_wywiad)
print("esm_cortisol vs screening")
print(diff_esm_cortisol_screening)
print("esm_cortisol vs all")
print(diff_esm_cortisol_all)

##################################
#all_base
unique_ID <- unique(c(FKBP5_baseline_data$ID, FKBP5_wywiad_data$ID, screening_data$ID, unique(esm_cortisol_data$Participant)))
all_base <- data.frame(
  ID = unique_ID)


##################################
#esm_cortisol_base

esm_cortisol_base <- data.frame(
  ID = esm_cortisol_data$Participant,
  obs = esm_cortisol_data$Trigger_counter,
  beep_d = esm_cortisol_data$meas_number_day,
  beep_w = esm_cortisol_data$meas_number_week,
  day = esm_cortisol_data$day,
  beeptime = as.numeric(hour(esm_cortisol_data$Trigger_time) * 60 + minute(esm_cortisol_data$Trigger_time)),
  resptime = as.numeric(hour(esm_cortisol_data$Form_start_time) * 60 + minute(esm_cortisol_data$Form_start_time)),
  resphour = as.numeric(hour(esm_cortisol_data$Form_start_time) + minute(esm_cortisol_data$Form_start_time) / 60),
  tothours = as.numeric(esm_cortisol_data$tothours),
  finishtime = as.numeric(hour(esm_cortisol_data$Form_finish_time) * 60 + minute(esm_cortisol_data$Form_finish_time)),
  cortisoltime = as.numeric(hour(esm_cortisol_data$Cortisol_time) * 60 + minute(esm_cortisol_data$Cortisol_time)),
  cortlevel = as.numeric(esm_cortisol_data$Cortisol_ng_ml * 100),
  sleeplength = as.numeric(esm_cortisol_data$sleep_hours_item_1972),
  sleep_quality = as.numeric(esm_cortisol_data$sleep_evaluation_item_a2)
) %>%
  mutate(
    eventstress = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$Importatnt_event_item_a3, esm_cortisol_data$Importatnt_event_item_1)),
    caffsmok = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$caffeine_tobacco_item_2109, esm_cortisol_data$caffeine_tobacco_item_1858)),
    eatdrink = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$food_soft_drink_item_2133, esm_cortisol_data$food_soft_drink_item_1901)),
    alcohol = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$alcohol_item_2157, esm_cortisol_data$alcohol_item_1931)),
    body = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$body_image_item_a7, esm_cortisol_data$body_image_item_5)),
    rumination = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$ruminations_item_2041, esm_cortisol_data$ruminations_item_1832)),
    actstress1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$rather_do_something_else_a8, esm_cortisol_data$rather_do_something_else_item_6)),
    actstress2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$difficult_event_item_a9, esm_cortisol_data$difficult_event_item_7)),
    actstress3 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$nice_event_item_a10, esm_cortisol_data$nice_event_item_8)),
    socstress1a = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_alone_item_a11_1, esm_cortisol_data$being_alone_item_9_1)),
    socstress1b = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_with_family_item_a11_2, esm_cortisol_data$being_with_family_item_9_2)),
    socstress1c = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_with_partner_item_a11_3, esm_cortisol_data$being_with_partner_item_9_3)),
    socstress1d = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_with_friends_item_a11_4, esm_cortisol_data$being_with_friends_item_9_4)),
    socstress1e = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_with_strangers_item_a11_5, esm_cortisol_data$being_with_strangers_item_9_5)),
    socstress1f = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$being_with_co_workers_item_a11_6, esm_cortisol_data$being_with_co_workers_item_9_6)),
    socstress1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$rather_be_alone_item_a12, esm_cortisol_data$rather_be_alone_item_10)),
    socstress2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$enjoyable_being_with_people_item_a13, esm_cortisol_data$enjoyable_being_with_people_item_11)),
    outsider = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$odludek_item_a14, esm_cortisol_data$odludek_item_12)),
    helpless1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$unsuspected_event_item_a15, esm_cortisol_data$unsuspected_event_item_13)),
    helpless2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$loss_of_control_item_a16, esm_cortisol_data$loss_of_control_item_14)),
    selfeff1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$coping_problems_item_a17, esm_cortisol_data$problems_coping_item_15)),
    selfeff2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$things_going_my_way_item_a18, esm_cortisol_data$things_going_my_way_item_16)),
    areastress = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$unpleasant_to_be_here_item_a19, esm_cortisol_data$unpleasant_to_be_here_item_17)),
    anxious = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$feel_anxious_item_a20, esm_cortisol_data$feel_anxious_item_18)),
    down = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$feel_depressed_item_a21, esm_cortisol_data$feel_depressed_item_19)),
    lonely = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$feel_alone_item_a22, esm_cortisol_data$feel_alone_item_20)),
    insecure = as.numeric(ifelse(beep_d== 1, esm_cortisol_data$dont_feel_safe_item_a23, esm_cortisol_data$dont_feel_safe_item_21)),
    annoyed = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$feel_irritated_a24, esm_cortisol_data$feel_irritated_item_22)),
    as1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$attention_attracts_item_a25, esm_cortisol_data$attention_attracts_item_23)),
    as2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$meaning_item_a26, esm_cortisol_data$meaning_item_24)),
    as3 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$noticing_item_a27, esm_cortisol_data$noticing_item_25)),
    ta1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$unpleasant_anticipation_item_a28, esm_cortisol_data$unpleasant_anticipation_item_26)),
    ta2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$carefulness_item_a29, esm_cortisol_data$carefulness_item_27)),
    ta3 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$details_item_a30, esm_cortisol_data$details_item_28)),
    h1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$hearing_thoughts_item_a31, esm_cortisol_data$hearing_thoughts_item_29)),
    h2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$voices_item_a32, esm_cortisol_data$voices_item_30)),
    h3 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$hallucinations_item_a33, esm_cortisol_data$hallucinations_item_31)),
    d1 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$invisible_force_item_a34, esm_cortisol_data$invisible_force_item_32)),
    d2 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$hidden_meaning_item_a35, esm_cortisol_data$hidden_meaning_item_33)),
    d3 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$unreal_experience_item_a36, esm_cortisol_data$unreal_experience_item_34)),
    d4 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$thought_control_item_a37, esm_cortisol_data$thought_control_item_35)),
    d5 = as.numeric(ifelse(beep_d == 1, esm_cortisol_data$get_rid_of_thoughts_item_38, esm_cortisol_data$get_rid_of_thoughts_item_36)),
    )

#Cortisol - meas1
mean_cortlevel_by_id <- esm_cortisol_base %>%
  filter(beep_d == 1) %>%
  group_by(ID) %>%
  summarize(Cortisol_1_mean = mean(cortlevel, na.rm = TRUE))

esm_cortisol_base <- esm_cortisol_base %>%
  left_join(select(mean_cortlevel_by_id, ID, Cortisol_1_mean), by = "ID")

rm(mean_cortlevel_by_id)

#Cortisol - meas23456
mean_cortlevel_by_id <- esm_cortisol_base %>%
  filter(beep_d != 1) %>%
  group_by(ID) %>%
  summarize(Cortisol_23456_mean = mean(cortlevel, na.rm = TRUE))

esm_cortisol_base <- esm_cortisol_base %>%
  left_join(select(mean_cortlevel_by_id, ID, Cortisol_23456_mean), by = "ID")

#Cortisol - week
mean_cortlevel_by_id <- esm_cortisol_base %>%
  group_by(ID) %>%
  summarize(Cortisol_mean_week = mean(cortlevel, na.rm = TRUE))

esm_cortisol_base <- esm_cortisol_base %>%
  left_join(select(mean_cortlevel_by_id, ID, Cortisol_mean_week), by = "ID")

rm(mean_cortlevel_by_id)

#Rumination_mean - week
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(Rumination_mean_week = mean(rumination, na.rm = TRUE))

#Rumination_mean - day
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID, day) %>%
  mutate(Rumination_mean_day = mean(rumination, na.rm = TRUE)) %>%
  ungroup()


#PLES_sum - meas - esm_cortisol
selected_columns <- c("as1", "as2", "as3", "ta1", "ta2", "ta3", "h1", "h2", "h3", "d1", "d2", "d3", "d4", "d5")
esm_cortisol_base$esm_PLEs_meas <- esmpack::combitems(selected_columns, esm_cortisol_base, fun = "sum")

#PLES_mean - day - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID, day) %>%
  mutate(esm_PLEs_mean_day = mean(esm_PLEs_meas, na.rm = TRUE)) %>%
  ungroup()

#PLES_sum/mean - week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(esm_PLEs_sum_week = sum(esm_PLEs_meas, na.rm = TRUE),
         esm_PLEs_mean_week = mean(esm_PLEs_meas, na.rm = TRUE)
         )

#socstress1a_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1a_sum_week = sum(socstress1a, na.rm = TRUE),
         socstress1a_mean_week = mean(socstress1a, na.rm = TRUE)
         )

#socstress1b_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1b_sum_week = sum(socstress1b, na.rm = TRUE),
         socstress1b_mean_week = mean(socstress1b, na.rm = TRUE)
         )

#socstress1c_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1c_sum_week = sum(socstress1c, na.rm = TRUE),
         socstress1c_mean_week = mean(socstress1c, na.rm = TRUE)
         )

#socstress1d_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1d_sum_week = sum(socstress1d, na.rm = TRUE),
         socstress1d_mean_week = mean(socstress1d, na.rm = TRUE)
         )

#socstress1e_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1e_sum_week = sum(socstress1e, na.rm = TRUE),
         socstress1e_mean_week = mean(socstress1e, na.rm = TRUE)
         )

#socstress1f_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1f_sum_week = sum(socstress1f, na.rm = TRUE),
         socstress1f_mean_week = mean(socstress1f, na.rm = TRUE)
         )

#socstress1_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress1_sum_week = sum(socstress1, na.rm = TRUE),
         socstress1_mean_week = mean(socstress1, na.rm = TRUE)
         )

#socstress2_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(socstress2_sum_week = sum(socstress2, na.rm = TRUE),
         socstress2_mean_week = mean(socstress2, na.rm = TRUE)
         )


#anxious_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(anxious_sum_week = sum(anxious, na.rm = TRUE),
         anxious_mean_week = mean(anxious, na.rm = TRUE)
         )

#down_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(down_sum_week = sum(down, na.rm = TRUE),
         down_mean_week = mean(down, na.rm = TRUE)
         )

#lonely_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  group_by(ID) %>%
  mutate(lonely_sum_week = sum(lonely, na.rm = TRUE),
         lonely_mean_week = mean(lonely, na.rm = TRUE)
         )

#insecure_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(insecure = as.numeric(insecure)) %>%
  group_by(ID) %>%
  mutate(insecure_sum_week = sum(insecure, na.rm = TRUE),
         insecure_mean_week = mean(insecure, na.rm = TRUE)
         )

#annoyed_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(annoyed = as.numeric(annoyed)) %>%
  group_by(ID) %>%
  mutate(annoyed_sum_week = sum(annoyed, na.rm = TRUE),
         annoyed_mean_week = mean(annoyed, na.rm = TRUE)
         )


#as_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(as_meas = as.numeric(as1) + as.numeric(as2) + as.numeric(as3)) %>%
  group_by(ID) %>%
  mutate(as_sum_week = sum(as_meas, na.rm = TRUE),
         as_mean_week = mean(as_meas, na.rm = TRUE)
         )

#ta_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(ta_meas = as.numeric(ta1) + as.numeric(ta2) + as.numeric(ta3)) %>%
  group_by(ID) %>%
  mutate(ta_sum_week = sum(ta_meas, na.rm = TRUE),
         ta_mean_week = mean(ta_meas, na.rm = TRUE)
         )

#h_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(h_meas = as.numeric(h1) + as.numeric(h2) + as.numeric(h3)) %>%
  group_by(ID) %>%
  mutate(h_sum_week = sum(h_meas, na.rm = TRUE),
         h_mean_week = mean(h_meas, na.rm = TRUE)
         )

#d_sum/mean by week - esm_cortisol
esm_cortisol_base <- esm_cortisol_base %>%
  mutate(d_meas = as.numeric(d1) + as.numeric(d2) + as.numeric(d3) + as.numeric(d4) + as.numeric(d5)) %>%
  group_by(ID) %>%
  mutate(d_sum_week = sum(d_meas, na.rm = TRUE),
         d_mean_week = mean(d_meas, na.rm = TRUE))









##################################
#FKBP5_wywiad_data


FKBP5_wywiad_selected <- FKBP5_wywiad_data %>%
  select(ID, 
         Depression_MINI = MINI_depression, 
         Mania_MINI = Mania_summary,
         Anxiety_MINI = Axiety_assessment,
         Agoraphobia_MINI = Agoraphobia,
         Social_phobia_MINI = Social_phobia,
         Specific_phobia_MINI = Specific_phobia,
         OCD_MINI = OCD,
         PZS_MINI = PZS_assessment,
         Alcoholism_now_MINI = Alcoholism_currently_summary,
         Alcoholism_abuse_now_MINI = Alcoholism_abuse_currently,
         Alcoholism_life_MINI = Alcoholism_lifetime_summary,
         Alcoholism_abuse_life_MINI = Alcoholism_abuse_lifetime,
         Drugs_MINI = Drugs_summary,
         Drugs_addiction_life_MINI = Drugs_assessment_addiction_lifetime,
         Drugs_addiction_12m_MINI = Drugs_addiction_last_12_months,
         Eating_Disorder_MINI = Eating_Disorder_assessment,
         Bulimia_MINI = Bulimia_summary,
         GAD_MINI = GAD_summary,
         Antisocial_life_MINI = Antisocial_dis_over_lifetime_assessment,
         Somatic_life_MINI = Somatic_dis_over_liftime,
         Somatic_now_MINI = Somatic_dis_currently,
         Hypohondria_MINI = Hypohondria_currently,
         Dysmorphophobia_MINI = Dysmorphophobia_currently,
         Pain_dis_MINI = Pain_dis_1,
         Pain_dis_A_MINI = Pain_dis_type_A_currently,
         Pain_dis_B_MINI = Pain_dis_type_B_currently,
         ADHD_life_MINI = ADHD_childhood_summary,
         ADHD_now_MINI = ADHD_assessment,
         Adaptation_MINI = Q21.A,
         PMDD_MINI = PMDD_currently,
         Mixed_dis_MINI = Mixed_dis_currently
         )

FKBP5_wywiad_selected <- FKBP5_wywiad_selected %>%
  mutate_all(~ifelse(is.na(.), "2", .))

# Następnie konwertuj wszystkie kolumny na factor
FKBP5_wywiad_selected <- FKBP5_wywiad_selected %>%
  mutate_all(~ as.factor(.))

all_base <- all_base %>%
  left_join(FKBP5_wywiad_selected, by = "ID")


#BMI
FKBP5_wywiad_data$BMI <- FKBP5_wywiad_data$Eating_Disorder_evaluation_2 / (FKBP5_wywiad_data$Eating_Disorder_evaluation_1 * 0.01)^2

tmp <- select(FKBP5_wywiad_data, ID, BMI, Eating_Disorder_evaluation_2, Eating_Disorder_evaluation_1) %>%
  filter(BMI > 50)

tmp_1 <- tmp %>%
  filter(Eating_Disorder_evaluation_2 >= 250)

tmp_1 <- tmp_1 %>%
  mutate(
    Eating_Disorder_evaluation_2 = tmp_1$Eating_Disorder_evaluation_2 / 10
  )

tmp_1$BMI <- tmp_1$Eating_Disorder_evaluation_2 / (tmp_1$Eating_Disorder_evaluation_1 * 0.01)^2

FKBP5_wywiad_data <- left_join(FKBP5_wywiad_data, tmp_1, by = "ID") %>%
  mutate(
    Eating_Disorder_evaluation_2 = coalesce(Eating_Disorder_evaluation_2.y, Eating_Disorder_evaluation_2.x),
    Eating_Disorder_evaluation_1 = coalesce(Eating_Disorder_evaluation_1.y, Eating_Disorder_evaluation_1.x),
    BMI = ifelse(is.na(BMI.y), BMI.x, BMI.y)
  ) %>%
  select(-starts_with("BMI."), -ends_with(".y"), -ends_with(".x"))


tmp_2 <- tmp %>%
  filter(Eating_Disorder_evaluation_2 < 250)

tmp_2 <- tmp_2 %>%
  mutate(
    tmp_value = Eating_Disorder_evaluation_2,
    Eating_Disorder_evaluation_2 = Eating_Disorder_evaluation_1,
    Eating_Disorder_evaluation_1 = tmp_value
  ) %>%
  select(-tmp_value)

tmp_2$BMI <- tmp_2$Eating_Disorder_evaluation_2 / (tmp_2$Eating_Disorder_evaluation_1 * 0.01)^2

FKBP5_wywiad_data <- left_join(FKBP5_wywiad_data, tmp_2, by = "ID")

FKBP5_wywiad_data <- left_join(FKBP5_wywiad_data, tmp_2, by = "ID") %>%
  mutate(
    Eating_Disorder_evaluation_2 = coalesce(Eating_Disorder_evaluation_2.y, Eating_Disorder_evaluation_2.x),
    Eating_Disorder_evaluation_1 = coalesce(Eating_Disorder_evaluation_1.y, Eating_Disorder_evaluation_1.x),
    BMI = ifelse(is.na(BMI.y), BMI.x, BMI.y)
  ) %>%
  select(-starts_with("BMI."), -ends_with(".y"), -ends_with(".x"))

all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, BMI), by = "ID")

rm(tmp)
rm(tmp_1)
rm(tmp_2)


#EDUCATION
all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, Education_duration_years), by = "ID") %>%
  dplyr::rename(Edu_year = Education_duration_years) %>%
  mutate(Edu_year = ifelse(Edu_year >= 40, Edu_year/10, Edu_year)) %>%
  left_join(select(FKBP5_wywiad_data, ID, Education_participant), by = "ID") %>%
  dplyr::rename(Edu_lvl = Education_participant)

#PLEs
#CAARMS_ples_poz
selected_columns <- c("CAARMS_Unusual_thinking_contents_assessment",
                      "CAARMS_other_thinking_content_assessment",
                      "CAARMS_hallucinations_assessment",
                      "CAARMS_speech_disorganization_assessment")
# Zamiana wartości NA na 1
FKBP5_wywiad_data[selected_columns][is.na(FKBP5_wywiad_data[selected_columns])] <- 1
# Obliczenie zmiennej CAA_poz
FKBP5_wywiad_data$CAA_poz <- esmpack::combitems(selected_columns, FKBP5_wywiad_data, fun = "sum") - length(selected_columns)

# Wykonanie left_join z esm_cortisol_base i dodanie kolumn z selected_columns
all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, CAA_poz, all_of(selected_columns)), by = "ID")

all_base <- dplyr::rename(all_base, CAA_utc = "CAARMS_Unusual_thinking_contents_assessment")
all_base <- dplyr::rename(all_base, CAA_otc = "CAARMS_other_thinking_content_assessment")
all_base <- dplyr::rename(all_base, CAA_h = "CAARMS_hallucinations_assessment")
all_base <- dplyr::rename(all_base, CAA_sd = "CAARMS_speech_disorganization_assessment")

#CAA_neg
selected_columns <- c("CAARMS_alogia_subj_scale",
                      "CAARMS_apathy_scale",
                      "CAARMS_anhedonia_scale")

FKBP5_wywiad_data[selected_columns][is.na(FKBP5_wywiad_data[selected_columns])] <- 1

FKBP5_wywiad_data$CAA_neg <- esmpack::combitems(selected_columns, FKBP5_wywiad_data, fun = "sum") - length(selected_columns)
all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, CAA_neg, all_of(selected_columns)), by = "ID")

all_base <- dplyr::rename(all_base, CAA_als = "CAARMS_alogia_subj_scale")
all_base <- dplyr::rename(all_base, CAA_ap = "CAARMS_apathy_scale")
all_base <- dplyr::rename(all_base, CAA_an = "CAARMS_anhedonia_scale")


#CAARMS_ples_sum
FKBP5_wywiad_data$CAA_sum = FKBP5_wywiad_data$CAA_poz + FKBP5_wywiad_data$CAA_neg
all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, CAA_sum), by = "ID")

#Menstruation
all_base <- all_base %>%
  left_join(select(FKBP5_wywiad_data, ID, Menstrual_cycle_phase), by = "ID")

FKBP5_wywiad_data <- FKBP5_wywiad_data %>%
  mutate(Menstrual_date = dmy(Menstrual_date))


# Grupowanie danych po Participant i obliczanie minimalnej oraz maksymalnej daty Trigger_date dla każdego ID
menstruation_date_summary <- esm_cortisol_data %>%
  group_by(Participant) %>%
  summarise(
    min_trigger_date = min(Trigger_date, na.rm = TRUE),  # Najwcześniejsza data
    max_trigger_date = max(Trigger_date, na.rm = TRUE)   # Najpóźniejsza data
  )
menstruation_date_summary <- menstruation_date_summary %>%
  dplyr::rename(ID = Participant)

menstruation_date_summary <- menstruation_date_summary %>%
  left_join(select(FKBP5_wywiad_data, ID, Menstrual_date), by = "ID")




# Funkcja do obliczania fazy cyklu z aktualizacją o 28 dni
calculate_cycle_phase <- function(menstruation_date, research_date) {
  # Sprawdzenie, czy daty są poprawne
  if (is.na(menstruation_date) | is.na(research_date)) {
    return(NA_real_)
  }
  
  # Funkcja do obliczenia fazy cyklu
  phase <- function(m_date, r_date) {
    if (m_date < r_date & r_date <= m_date + days(7)) {
      return(1)
    } else if (m_date + days(7) < r_date & r_date <= m_date + days(12)) {
      return(2)
    } else if (m_date + days(12) < r_date & r_date <= m_date + days(15)) {
      return(3)
    } else if (m_date + days(15) < r_date & r_date <= m_date + days(28)) {
      return(4)
    } else {
      return(NA_real_)
    }
  }
  
  # Oblicz fazę cyklu i aktualizuj datę o 28 dni, jeśli faza nie została określona
  current_phase <- phase(menstruation_date, research_date)
  while (is.na(current_phase) & menstruation_date <= research_date) {
    menstruation_date <- menstruation_date + days(28)
    current_phase <- phase(menstruation_date, research_date)
  }
  
  return(current_phase)
}

# Funkcja do obliczania faz cyklu dla wielu dat
compute_cycle_phases <- function(menstruation_dates, research_dates) {
  phases <- mapply(calculate_cycle_phase, menstruation_dates, research_dates)
  return(phases)
}


# Obliczanie faz cyklu
menstruation_date_summary$Menstrual_phase_start <- compute_cycle_phases(
  menstruation_date_summary$Menstrual_date,
  menstruation_date_summary$min_trigger_date
)

menstruation_date_summary$Menstrual_phase_end <- compute_cycle_phases(
  menstruation_date_summary$Menstrual_date,
  menstruation_date_summary$max_trigger_date
)

all_base <- left_join(all_base, select(menstruation_date_summary, ID, Menstrual_date, Menstrual_phase_start, Menstrual_phase_end), by = "ID")

rm(menstruation_date_summary)





##################################
#FKBP5_BASELINE_DATA
#TEC
selected_columns <- grep("^TEC", names(FKBP5_baseline_data), value = TRUE)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")


selected_columns <- grep("^TEC_[0-9]{1,2}$", names(FKBP5_baseline_data), value = TRUE)
all_base$TEC_sum <- esmpack::combitems(selected_columns, all_base, fun = "sum")

#PSTT & PMDD
selected_columns <- grep("^(PSTT_[0-9]{1,2}|PSTT_[a-z])$", names(FKBP5_baseline_data), value = TRUE)
print(selected_columns)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")

# Obliczenie sumy z uwzględnieniem NA
all_base <- all_base %>%
  mutate(
    PSTT_1_4_mod_to_sev = 
      ifelse(PSTT_1 >= 3, 1, 0) +
      ifelse(PSTT_2 >= 3, 1, 0) +
      ifelse(PSTT_3 >= 3, 1, 0) +
      ifelse(PSTT_4 >= 3, 1, 0)
  )

all_base <- all_base %>%
  mutate(
    PSTT_1_4_sev = 
      ifelse(PSTT_1 == 4, 1, 0) +
      ifelse(PSTT_2 == 4, 1, 0) +
      ifelse(PSTT_3 == 4, 1, 0) +
      ifelse(PSTT_4 == 4, 1, 0)
  )


all_base <- all_base %>%
  mutate(
    PSTT_1_14_mod_to_sev = 
      ifelse(PSTT_1 >= 3, 1, 0) +
      ifelse(PSTT_2 >= 3, 1, 0) +
      ifelse(PSTT_3 >= 3, 1, 0) +
      ifelse(PSTT_4 >= 3, 1, 0) +
      ifelse(PSTT_5 >= 3, 1, 0) +
      ifelse(PSTT_6 >= 3, 1, 0) +
      ifelse(PSTT_7 >= 3, 1, 0) +
      ifelse(PSTT_8 >= 3, 1, 0) +
      ifelse(PSTT_9 >= 3, 1, 0) +
      ifelse(PSTT_10 >= 3, 1, 0) +
      ifelse(PSTT_11 >= 3, 1, 0) +
      ifelse(PSTT_12 >= 3, 1, 0) +
      ifelse(PSTT_13 >= 3, 1, 0) +
      ifelse(PSTT_14 >= 3, 1, 0)
  )

all_base <- all_base %>%
  mutate(
    PSTT_abcde_mod_to_sev = 
      ifelse(PSTT_a >= 3, 1, 0) +
      ifelse(PSTT_b >= 3, 1, 0) +
      ifelse(PSTT_c >= 3, 1, 0) +
      ifelse(PSTT_d >= 3, 1, 0) +
      ifelse(PSTT_e >= 3, 1, 0)
  )

all_base <- all_base %>%
  mutate(
    PSTT_abcde_sev = 
      ifelse(PSTT_a == 4, 1, 0) +
      ifelse(PSTT_b == 4, 1, 0) +
      ifelse(PSTT_c == 4, 1, 0) +
      ifelse(PSTT_d == 4, 1, 0) +
      ifelse(PSTT_e == 4, 1, 0)
  )

all_base <- all_base %>%
  mutate(
    PMS_baseline = ifelse(
      (PSTT_1_4_mod_to_sev >= 1 & PSTT_1_14_mod_to_sev >= 4) | (PSTT_abcde_mod_to_sev >= 1),
      1,
      2
    ),
    PMDD_baseline = ifelse(
      (PSTT_1_4_sev >= 1 & PSTT_1_14_mod_to_sev >= 4) | (PSTT_abcde_sev >= 1),
      1,
      2
    )
  )

# Filtracja wierszy z różnicami między PSTT_baseline a PMDD_baseline
differences_PMS_PMDD <- all_base %>%
  filter(PMS_baseline != PMDD_baseline) %>%
  select(PMS_baseline, PMDD_baseline) # Wyświetlanie tylko wybranych kolumn

print(differences_PMS_PMDD)
rm(differences_PMS_PMDD)


#LW / RW
selected_columns <- grep("^LW9_[0-9]$", names(FKBP5_baseline_data), value = TRUE)
print(selected_columns)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")

all_base$LW9_sum <- esmpack::combitems(selected_columns, all_base, fun = "sum")

selected_columns <- grep("^RW_[0-9]{1,2}$", names(FKBP5_baseline_data), value = TRUE)
print(selected_columns)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")

all_base$RW_sum <- esmpack::combitems(selected_columns, all_base, fun = "sum")

all_base$LW9RW_sum <- esmpack::combitems(c("LW9_sum", "RW_sum"), all_base, fun = "sum")


#UCLA
selected_columns <- grep("^R_UCLA_[0-9]{1,2}$", names(FKBP5_baseline_data), value = TRUE)
print(selected_columns)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")

reverse_values <- function(x, min_val = NULL, max_val = NULL) {
  # Sprawdź czy wektor jest liczbą całkowitą
  if (!is.numeric(x)) {
    stop("Wektor musi zawierać liczby całkowite.")
  }
  
  # Ustalenie minimalnej i maksymalnej wartości
  if (is.null(min_val)) min_val <- min(x, na.rm = TRUE)
  if (is.null(max_val)) max_val <- max(x, na.rm = TRUE)
  
  # Przekształć wartości w wektorze
  reversed_x <- (max_val + min_val) - x
  
  return(reversed_x)
}

# Przekształcanie wartości w ramce danych
all_base <- all_base %>%
  mutate(across(
    c("R_UCLA_1", "R_UCLA_5", "R_UCLA_6", "R_UCLA_9", 
      "R_UCLA_10", "R_UCLA_15", "R_UCLA_16", "R_UCLA_19", "R_UCLA_20"),
    ~ reverse_values(.x, min_val = 1, max_val = 4)
  ))

all_base$UCLA_sum <- esmpack::combitems(selected_columns, all_base, fun = "sum")


# MINI_COPE
selected_columns <- grep("^Mini_COPE_[0-9]{1,2}$", names(FKBP5_baseline_data), value = TRUE)
print(selected_columns)
all_base <- left_join(all_base, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")

# Funkcja pomocnicza do obliczania średniej z uwzględnieniem NA
mean_na_rm <- function(x, ...) {
  # Sprawdzamy, czy wektor jest pusty lub ma same NA
  if (all(is.na(x)) || length(x) == 0) {
    return(NA)
  }
  
  # Oblicz średnią z zaokrągleniem do dwóch miejsc po przecinku
  round(mean(x, na.rm = TRUE), 2)
}


#Problem-Focused Coping (Items 2, 7, 10, 12, 14, 17, 23, 25)
all_base <- all_base %>%
  mutate(
    Mini_COPE_PFC = esmpack::combitems(
      c("Mini_COPE_2", "Mini_COPE_7", "Mini_COPE_10", "Mini_COPE_12", "Mini_COPE_14", "Mini_COPE_17", "Mini_COPE_23", "Mini_COPE_25"),
      all_base,
      fun = mean_na_rm
    )
  )
#Active coping
all_base <- all_base %>%
  mutate(
    Mini_COPE_PFC_AC = esmpack::combitems(
      c("Mini_COPE_2", "Mini_COPE_7"),
      all_base,
      fun = mean_na_rm
    )
  )
#Use of informational support
all_base <- all_base %>%
  mutate(
    Mini_COPE_PFC_UIS = esmpack::combitems(
      c("Mini_COPE_10", "Mini_COPE_23"),
      all_base,
      fun = mean_na_rm
    )
  )
#Positive reframing
all_base <- all_base %>%
  mutate(
    Mini_COPE_PFC_PF = esmpack::combitems(
      c("Mini_COPE_12", "Mini_COPE_17"),
      all_base,
      fun = mean_na_rm
    )
  )
#Planning
all_base <- all_base %>%
  mutate(
    Mini_COPE_PFC_P = esmpack::combitems(
      c("Mini_COPE_14", "Mini_COPE_25"),
      all_base,
      fun = mean_na_rm
    )
  )


#Emotion-Focused Coping (Items 5, 9, 13, 15, 18, 20, 21, 22, 24, 26, 27, 28)
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC = esmpack::combitems(
      c("Mini_COPE_5", "Mini_COPE_9", "Mini_COPE_13", "Mini_COPE_15", "Mini_COPE_18", "Mini_COPE_20", "Mini_COPE_21", "Mini_COPE_22", "Mini_COPE_24", "Mini_COPE_26", "Mini_COPE_27", "Mini_COPE_28"),
      all_base,
      fun = mean_na_rm
    )
  )
#Emotional Support
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_ES = esmpack::combitems(
      c("Mini_COPE_5", "Mini_COPE_15"),
      all_base,
      fun = mean_na_rm
    )
  )
#Venting
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_V = esmpack::combitems(
      c("Mini_COPE_9", "Mini_COPE_21"),
      all_base,
      fun = mean_na_rm
    )
  )
#Humour
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_H = esmpack::combitems(
      c("Mini_COPE_18", "Mini_COPE_28"),
      all_base,
      fun = mean_na_rm
    )
  )
#Acceptance
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_A = esmpack::combitems(
      c("Mini_COPE_20", "Mini_COPE_24"),
      all_base,
      fun = mean_na_rm
    )
  )
#Self-blame
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_SB = esmpack::combitems(
      c("Mini_COPE_13", "Mini_COPE_26"),
      all_base,
      fun = mean_na_rm
    )
  )
#Religion
all_base <- all_base %>%
  mutate(
    Mini_COPE_EFC_R = esmpack::combitems(
      c("Mini_COPE_22", "Mini_COPE_27"),
      all_base,
      fun = mean_na_rm
    )
  )


#Avoidant Coping (Items 1, 3, 4, 6, 8, 11, 16, 19)
all_base <- all_base %>%
  mutate(
    Mini_COPE_AC = esmpack::combitems(
      c("Mini_COPE_1", "Mini_COPE_3", "Mini_COPE_4", "Mini_COPE_6", "Mini_COPE_8", "Mini_COPE_11", "Mini_COPE_16", "Mini_COPE_19"),
      all_base,
      fun = mean_na_rm
    )
  )
#Self-distraction
all_base <- all_base %>%
  mutate(
    Mini_COPE_AC_SD = esmpack::combitems(
      c("Mini_COPE_1", "Mini_COPE_19"),
      all_base,
      fun = mean_na_rm
    )
  )
#Denial
all_base <- all_base %>%
  mutate(
    Mini_COPE_AC_D = esmpack::combitems(
      c("Mini_COPE_3", "Mini_COPE_8"),
      all_base,
      fun = mean_na_rm
    )
  )
#Substance Use
all_base <- all_base %>%
  mutate(
    Mini_COPE_AC_SU = esmpack::combitems(
      c("Mini_COPE_4", "Mini_COPE_11"),
      all_base,
      fun = mean_na_rm
    )
  )
#Behavioural disengagement
all_base <- all_base %>%
  mutate(
    Mini_COPE_AC_BD = esmpack::combitems(
      c("Mini_COPE_6", "Mini_COPE_16"),
      all_base,
      fun = mean_na_rm
    )
  )







##################################
#SCREENING_DATA 

#age, sex, status
all_base <- all_base %>%
  left_join(select(screening_data, ID, Age, Gender, Group), by = "ID") %>%
  dplyr::rename(age = Age, sex = Gender, status = Group)


#ASRS
selected_columns <- grep("^ASRS_[0-9]", names(screening_data), value = TRUE)
all_base <- left_join(all_base, select(screening_data, ID, ASRS_suma, all_of(selected_columns)), by = "ID")
all_base <- dplyr::rename(all_base, ASRS_sum = "ASRS_suma")

#Depression_family, Schizophrenia_family
tmp <- screening_data %>%
  select(ID, Depression_family, Schizophrenia_family) %>%
  mutate_at(vars(Depression_family, Schizophrenia_family), 
            ~ as.factor(recode(., "Nie" = "2", "Tak" = "1")))

all_base <- all_base %>%
  left_join(tmp, by = "ID")

rm(tmp)

#Religion
selected_columns <- grep("^Rel_[0-9]", names(screening_data), value = TRUE)
all_base <- left_join(all_base, select(screening_data, ID, all_of(selected_columns)), by = "ID")
all_base$Rel_sum <- esmpack::combitems(selected_columns, all_base, fun = "sum")








##################################
#ATTR
attr(all_base$ID, "label") <- "ID"
attr(all_base$status, "label") <- "Group (H-high, L-low)"
attr(all_base$age, "label") <- "Age"
attr(all_base$sex, "label") <- "Gender"
attr(all_base$Edu_lvl, "label") <- "Education level"
attr(all_base$Edu_year, "label") <- "Education - number of years"
attr(all_base$BMI, "label") <- "BMI"
attr(all_base$ASRS_sum, "label") <- "ASRS total"

attr(all_base$CAA_poz, "label") <- "CAARMS positive symptoms"
attr(all_base$CAA_neg, "label") <- "CAARMS negative symptoms"
attr(all_base$CAA_sum, "label") <- "CAARMS total"

attr(all_base$Depression_family, "label") <- "Depression history in family"
attr(all_base$Schizophrenia_family, "label") <- "Schizofrenia history in family"







##################################
#network_analysis_data
network_analysis_data <- left_join(esm_cortisol_base, all_base, by = "ID")

# Lista kolumn do usunięcia
columns_to_remove <- c(
  "Cortisol_1_mean",
  "Cortisol_23456_mean",
  "Cortisol_mean_week",
  "Rumination_mean_day",
  "Rumination_mean_week",
  "esm_PLEs_meas",
  "esm_PLEs_mean_day",
  "esm_PLEs_sum_week",
  "esm_PLEs_mean_week",
  "socstress1a_sum_week",
  "socstress1a_mean_week",
  "socstress1b_sum_week",
  "socstress1b_mean_week",
  "socstress1c_sum_week",
  "socstress1c_mean_week",
  "socstress1d_sum_week",
  "socstress1d_mean_week",
  "socstress1e_sum_week",
  "socstress1e_mean_week",
  "socstress1f_sum_week",
  "socstress1f_mean_week",
  "socstress1_sum_week",
  "socstress1_mean_week",
  "socstress2_sum_week",
  "socstress2_mean_week",
  "anxious_sum_week",
  "anxious_mean_week",
  "down_sum_week",
  "down_mean_week",
  "lonely_sum_week",
  "lonely_mean_week",
  "insecure_sum_week",
  "insecure_mean_week",
  "annoyed_sum_week",
  "annoyed_mean_week",
  "as_meas",
  "as_sum_week",
  "as_mean_week",
  "ta_meas",
  "ta_sum_week",
  "ta_mean_week",
  "h_meas",
  "h_sum_week",
  "h_mean_week",
  "d_meas",
  "d_sum_week",
  "d_mean_week",
  "CAA_sum",
  "PSTT_1_4_mod_to_sev",
  "PSTT_1_4_sev",
  "PSTT_1_14_mod_to_sev",
  "PSTT_abcde_sev",
  "PSTT_abcde_mod_to_sev",
  "LW9RW_sum"
)

# Usunięcie wybranych kolumn
network_analysis_data <- network_analysis_data %>%
  select(-all_of(columns_to_remove))


###################
#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("out/network_analysis_data_", current_time, ".sav")
# Zapis danych
write_sav(network_analysis_data, filename)











##################################
#MEDIATION_DATA
mediation_data <- data.frame(ID = FKBP5_wywiad_data$ID_participant)

mediation_data <- mediation_data %>%
  left_join(all_base, by = "ID")

mediation_data <- left_join(mediation_data, distinct(select(baza, id, esm_PLEs_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, Rumination_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1a_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1b_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1c_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1d_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1e_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1f_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress2_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, anxious_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, down_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, lonely_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, insecure_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, annoyed_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, as_sum_week, ta_sum_week, h_sum_week, d_sum_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, esm_PLEs_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1a_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1b_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1c_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1d_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1e_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1f_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress1_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, socstress2_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, anxious_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, down_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, lonely_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, insecure_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, annoyed_mean_week)), by = c("ID" = "id"))
mediation_data <- left_join(mediation_data, distinct(select(baza, id, as_mean_week, ta_mean_week, h_mean_week, d_mean_week)), by = c("ID" = "id"))


###################
#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("out/mediation_data_", current_time, ".sav")
# Zapis danych
write_sav(esm_cortisol_data, filename)

