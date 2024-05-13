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
#install.packages("hms")
library(hms)
library(base)
#install.packages("janitor")
library(janitor)


FKBP5_wywiad_data <- read_sav("data_mediation/clear_FKBP5_wywiad_data.sav")
FKBP5_baseline_data <- read_sav("data_mediation/clear_FKBP5_baseline_data.sav")
screening_data <- read_sav("data_mediation/clear_screening_data.sav")


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

# Znalezienie różnic między kolumnami
#baseline
diff_baseline_wywiad <- setdiff(ID_names_baseline, ID_names_wywiad)
diff_baseline_screening <- setdiff(ID_names_baseline, ID_names_screening)
diff_baseline_all <- setdiff(ID_names_baseline, c(ID_names_screening, ID_names_wywiad))


print("baseline vs wywiad")
print(diff_baseline_wywiad)
print("baseline vs screening")
print(diff_baseline_screening)
print("baseline vs all")
print(diff_baseline_all)

#wywiad
diff_wywiad_screening <- setdiff(ID_names_wywiad, ID_names_screening)
diff_wywiad_baseline <- setdiff(ID_names_wywiad, ID_names_baseline)
diff_wywiad_all <- setdiff(ID_names_wywiad, c(ID_names_screening, ID_names_baseline))


print("wywiad vs baseline")
print(diff_wywiad_baseline)
print("wywiad vs screening")
print(diff_wywiad_screening)
print("wywiad vs all")
print(diff_wywiad_all)

#screening
diff_screening_wywiad <- setdiff(ID_names_screening, ID_names_wywiad)
diff_screening_baseline <- setdiff(ID_names_screening, ID_names_baseline)
diff_screening_all <- setdiff(ID_names_screening, c(ID_names_wywiad, ID_names_baseline))


print("screening vs baseline")
print(diff_screening_baseline)
print("screening vs wywiad")
print(diff_screening_wywiad)
print("screening vs all")
print(diff_screening_all)




unikatowe_ID <- unique(c(FKBP5_baseline_data$ID, FKBP5_wywiad_data$ID, screening_data$ID))

mediation_data <- data.frame(ID = FKBP5_wywiad_data$ID)

#ZABURZENIA
ZABURZENIA <-  data.frame(
  ID = FKBP5_wywiad_data$ID,
  Depression_MINI = as.factor(FKBP5_wywiad_data$MINI_depression),
  Mania_MINI = as.factor(FKBP5_wywiad_data$Mania_summary),
  Anxiety_MINI = as.factor(FKBP5_wywiad_data$Anxiety_assessment),
  Agoraphobia_MINI = as.factor(FKBP5_wywiad_data$Agoraphobia),
  Social_phobia_MINI = as.factor(FKBP5_wywiad_data$Social_phobia),
  Specific_phobia_MINI = as.factor(FKBP5_wywiad_data$Specific_phobia),
  OCD_MINI = as.factor(FKBP5_wywiad_data$OCD),
  PZS_MINI = as.factor(FKBP5_wywiad_data$PZS_assessment),
  Alcoholism_now_MINI = as.factor(FKBP5_wywiad_data$Alcoholism_currently_summary),
  Alcoholism_abuse_now_MINI = as.factor(FKBP5_wywiad_data$Alcoholism_abuse_currently),
  Alcoholism_life_MINI = as.factor(FKBP5_wywiad_data$Alcoholism_lifetime_summary),
  Alcoholism_abuse_life_MINI = as.factor(FKBP5_wywiad_data$Alcoholism_abuse_lifetime),
  Drugs_MINI = as.factor(FKBP5_wywiad_data$Drugs_summary),
  Drugs_addiction_life_MINI = as.factor(FKBP5_wywiad_data$Drugs_assessment_addiction_lifetime),
  Drugs_addiction_12m_MINI = as.factor(FKBP5_wywiad_data$Drugs_addiction_last_12_months),
  Eating_Disorder_MINI = as.factor(FKBP5_wywiad_data$Eating_Disorder_assessment),
  Bulimia_MINI = as.factor(FKBP5_wywiad_data$Bulimia_summary),
  GAD_MINI = as.factor(FKBP5_wywiad_data$GAD_summary),
  Antisocial_life_MINI = as.factor(FKBP5_wywiad_data$Antisocial_dis_over_lifetime_assessment),
  Somatic_life_MINI = as.factor(FKBP5_wywiad_data$Somatic_dis_over_liftime),
  Somatic_now_MINI = as.factor(FKBP5_wywiad_data$Somatic_dis_currently),
  Hypohondria_MINI = as.factor(FKBP5_wywiad_data$Hypohondria_currently),
  Dysmorphophobia_MINI = as.factor(FKBP5_wywiad_data$Dysmorphophobia_currently),
  Pain_dis_MINI = as.factor(FKBP5_wywiad_data$Pain_dis_1),
  Pain_dis_A_MINI = as.factor(FKBP5_wywiad_data$Pain_dis_type_A_currently),
  Pain_dis_B_MINI = as.factor(FKBP5_wywiad_data$Pain_dis_type_B_currently),
  ADHD_life_MINI = as.factor(FKBP5_wywiad_data$ADHD_childhood_summary),
  ADHD_now_MINI = as.factor(FKBP5_wywiad_data$ADHD_assessment),
  Adaptation_MINI = as.factor(FKBP5_wywiad_data$Q21.A),
  PMDD_MINI = as.factor(FKBP5_wywiad_data$PMDD_currently),
  Mixed_dis_MINI = as.factor(FKBP5_wywiad_data$Mixed_dis_currently)
)

ZABURZENIA <- ZABURZENIA %>%
  mutate_all(~ifelse(is.na(.), "2", .))

mediation_data <- mediation_data %>%
  left_join(ZABURZENIA, by = "ID")

rm(ZABURZENIA)



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

mediation_data <- mediation_data %>%
  left_join(select(FKBP5_wywiad_data, ID, BMI), by = "ID")

rm(tmp)
rm(tmp_1)
rm(tmp_2)



#MEDIATION

#SCREENING_DATA age, sex, status
mediation_data <- mediation_data %>%
  left_join(select(screening_data, ID, Age, Gender, Group, ASRS_suma), by = "ID") %>%
  rename(age = Age, sex = Gender, status = Group)


#asrs
selected_columns <- grep("^ASRS_[0-9]", names(screening_data), value = TRUE)
mediation_data <- left_join(mediation_data, select(screening_data, ID, all_of(selected_columns)), by = "ID")


#CHOROBY_W_RODZINIE
CHOROBY_W_RODZINIE <- screening_data %>%
  select(ID, Depression_family, Schizophrenia_family) %>%
  mutate_at(vars(Depression_family, Schizophrenia_family), 
            ~ as.factor(recode(., "Nie" = "2", "Tak" = "1")))

mediation_data <- mediation_data %>%
  left_join(CHOROBY_W_RODZINIE, by = "ID")

rm(CHOROBY_W_RODZINIE)





#FKBP5_WYWIAD_DATA 

#education
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_wywiad_data, ID, Education_duration_years), by = "ID") %>%
  rename(education_years = Education_duration_years) %>%
  mutate(education_years = ifelse(education_years >= 40, education_years/10, education_years)) %>%
  left_join(select(FKBP5_wywiad_data, ID, Education_participant), by = "ID") %>%
  rename(education_lvl = Education_participant)

#PLEs - wywiad
selected_columns <- c("CAARMS_Unusual_thinking_contents_assessment",
                      "CAARMS_other_thinking_content_assessment",
                      "CAARMS_hallucinations_assessment",
                      "CAARMS_speech_disorganization_assessment")
FKBP5_wywiad_data[selected_columns][is.na(FKBP5_wywiad_data[selected_columns])] <- 1
FKBP5_wywiad_data$CAARMS_ples_poz <- esmpack::combitems(selected_columns, FKBP5_wywiad_data, fun = "sum") - length(selected_columns)
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_wywiad_data, ID, CAARMS_ples_poz), by = "ID")



selected_columns <- c("CAARMS_alogia_subj_scale",
                      "CAARMS_apathy_scale",
                      "CAARMS_anhedonia_scale")

FKBP5_wywiad_data[selected_columns][is.na(FKBP5_wywiad_data[selected_columns])] <- 1

FKBP5_wywiad_data$CAARMS_ples_neg <- esmpack::combitems(selected_columns, FKBP5_wywiad_data, fun = "sum") - length(selected_columns)
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_wywiad_data, ID, CAARMS_ples_neg), by = "ID")

FKBP5_wywiad_data$CAARMS_ples_sum = FKBP5_wywiad_data$CAARMS_ples_poz + FKBP5_wywiad_data$CAARMS_ples_neg
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_wywiad_data, ID, CAARMS_ples_sum), by = "ID")




#FKBP5_BASELINE_DATA
#tec
selected_columns <- grep("^TEC", names(FKBP5_baseline_data), value = TRUE)
mediation_data <- left_join(mediation_data, select(FKBP5_baseline_data, ID, all_of(selected_columns)), by = "ID")






tmp_mediation_data <- data.frame(
  ID = mediation_data$ID,
  grupa = mediation_data$status,
  płeć = mediation_data$sex,
  wiek = mediation_data$age,
  Edu_lvl = mediation_data$education_lvl,
  Edu_year = mediation_data$education_years,
  CAA_poz = mediation_data$CAARMS_ples_poz,
  CAA_neg = mediation_data$CAARMS_ples_neg,
  CAA_sum = mediation_data$CAARMS_ples_sum,
  ASRS_sum = mediation_data$ASRS_suma,
  #PLES_sum = mediation_data$esm_PLEs_mean_week,
  #Ru_m_we =  mediation_data$Rumination_mean_week,
  #PTQ_sum = mediation_data$PTQ_suma,
  Depression_MINI = mediation_data$Depression_MINI,
  Mania_MINI = mediation_data$Mania_MINI,
  Anxiety_MINI = mediation_data$Anxiety_MINI,
  Agoraphobia_MINI = mediation_data$Agoraphobia_MINI,
  Social_phobia_MINI = mediation_data$Social_phobia_MINI,
  Specific_phobia_MINI = mediation_data$Specific_phobia_MINI,
  OCD_MINI = mediation_data$OCD_MINI,
  PZS_MINI = mediation_data$PZS_MINI,
  Alcoholism_now_MINI = mediation_data$Alcoholism_now_MINI,
  Alcoholism_abuse_now_MINI = mediation_data$Alcoholism_abuse_now_MINI,
  Alcoholism_life_MINI = mediation_data$Alcoholism_life_MINI,
  Alcoholism_abuse_life_MINI = mediation_data$Alcoholism_abuse_life_MINI,
  Drugs_MINI = mediation_data$Drugs_MINI,
  Drugs_addiction_life_MINI = mediation_data$Drugs_addiction_life_MINI,
  Drugs_addiction_12m_MINI = mediation_data$Drugs_addiction_12m_MINI,
  Eating_Disorder_MINI = mediation_data$Eating_Disorder_MINI,
  Bulimia_MINI = mediation_data$Bulimia_MINI,
  GAD_MINI = mediation_data$GAD_MINI,
  Antisocial_life_MINI = mediation_data$Antisocial_life_MINI,
  Somatic_life_MINI = mediation_data$Somatic_life_MINI,
  Somatic_now_MINI = mediation_data$Somatic_now_MINI,
  Hypohondria_MINI = mediation_data$Hypohondria_MINI,
  Dysmorphophobia_MINI = mediation_data$Dysmorphophobia_MINI,
  Pain_dis_MINI = mediation_data$Pain_dis_MINI,
  Pain_dis_A_MINI = mediation_data$Pain_dis_A_MINI,
  Pain_dis_B_MINI = mediation_data$Pain_dis_B_MINI,
  ADHD_life_MINI = mediation_data$ADHD_life_MINI,
  ADHD_now_MINI = mediation_data$ADHD_now_MINI,
  Adaptation_MINI = mediation_data$Adaptation_MINI,
  PMDD_MINI = mediation_data$PMDD_MINI,
  Mixed_dis_MINI = mediation_data$Mixed_dis_MINI,
  BMI = mediation_data$BMI,
  Depression_family = mediation_data$Depression_family,
  Schizophrenia_family = mediation_data$Schizophrenia_family
)


selected_columns <- grep("^ASRS_[0-9]", names(mediation_data), value = TRUE)
selected_columns <- mediation_data[selected_columns]
tmp_mediation_data <- cbind(selected_columns, tmp_mediation_data)

selected_columns <- grep("^TEC", names(mediation_data), value = TRUE)
selected_columns <- mediation_data[selected_columns]
tmp_mediation_data <- cbind(selected_columns, tmp_mediation_data)

attr(tmp_mediation_data$ID, "label") <- "ID"
attr(tmp_mediation_data$grupa, "label") <- "Group (H-high, L-low)"
attr(tmp_mediation_data$wiek, "label") <- "Age"
attr(tmp_mediation_data$płeć, "label") <- "Gender"
attr(tmp_mediation_data$Edu_lvl, "label") <- "Education level"
attr(tmp_mediation_data$Edu_year, "label") <- "Education - number of years"
attr(tmp_mediation_data$BMI, "label") <- "BMI"
attr(tmp_mediation_data$ASRS_sum, "label") <- "ASRS total"

attr(tmp_mediation_data$CAA_poz, "label") <- "CAARMS positive symptoms"
attr(tmp_mediation_data$CAA_neg, "label") <- "CAARMS negative symptoms"
attr(tmp_mediation_data$CAA_sum, "label") <- "CAARMS total"

attr(tmp_mediation_data$Depression_family, "label") <- "Depression history in family"
attr(tmp_mediation_data$Schizophrenia_family, "label") <- "Schizofrenia history in family"


#ZAPISYWANIE
write_sav(tmp_mediation_data, "out_mediation/clear_mediation_data.sav")








