
# Model dla zmiennej mediatora
model.m <- lm(Cortisol_ng_ml ~ 1, data = esm_cortisol_data_aggregated)

# Model dla zmiennej wynikowej
model.y <- lm(f1_PLEs_sum ~ Cortisol_ng_ml, data = esm_cortisol_data_aggregated)

# Wywołanie funkcji mediate()
mediacja <- mediate(model.m, model.y, sims = 1000, mediator = "Cortisol_ng_ml", data = esm_cortisol_data_aggregated)



db <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                 x1 = c(9, 8, 7, 6, 5, 4, 3, 2, 1),
                 x2 = c(9, 9, 7, 7, 5, 5, 3, 3, 1),
                 x3 = c(1, 1, 1, 1, 1, 1, 1, 1, 1))

model.M <- lm(x2 ~ x1 + x3 + y, db)  # Dodanie zmiennej y do modelu M
model.Y <- lm(y ~ x1 + x2 + x3, db)

results <- mediate(model.M, model.Y, treat = "x1", mediator = "x2", boot = TRUE, sims = 500)
summary(results)

# Przypisanie nazw kolumn zaczynających się od "PLES" do nowego wektora
#ASRS_cols <- names(WWA_all_screening_data)[startsWith(names(WWA_all_screening_data), "ASRS")]
selected_columns <- names(screening_data)[grep("^ASRS_[0-9]", names(screening_data))]
# ASRS_cols <- names(FKBP5_wywiad_March_2024_data)[startsWith(names(FKBP5_wywiad_March_2024_data), "PLEs")]
#print(ASRS_cols)
# print(names(esm_cortisol_data))
#WWA_all_screening_data$ASRS_suma



selected_columns <- c("meaning_item_24", "noticing_item_25", "unpleasant_anticipation_item_26", 
                      "carefulness_item_27", "details_item_28", "hearing_thoughts_item_29", 
                      "voices_item_30", "hallucinations_item_31", "invisible_force_item_32", 
                      "hidden_meaning_item_33", "unreal_experience_item_34", 
                      "thought_control_item_35", "get_rid_of_thoughts_item_36")

esm_cortisol_data$f1_PLEs_sum <- esmpack::combitems(selected_columns, esm_cortisol_data, fun = "sum")

#PLEs_sum_treshold <- mean(esm_cortisol_data$PLEs_sum, na.rm = TRUE)
#esm_cortisol_data$PLEs_high <- ifelse(esm_cortisol_data$PLEs_sum > PLEs_sum_treshold, 1, 0)


esm_cortisol_data_aggregated <- esm_cortisol_data_by_date %>%
  group_by(Participant, meas_number) %>%
  summarize(
    PLEs_sum = mean(PLEs_sum, na.rm = TRUE),
    Cortisol_ng_ml = mean(Cortisol_ng_ml, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = meas_number, values_from = c(PLEs_sum, Cortisol_ng_ml), names_prefix = "meas_")


names(WWA_all_screening_data)[1] <- "Participant"
esm_cortisol_data_aggregated <- merge(esm_cortisol_data_aggregated, WWA_all_screening_data[, c("Participant", "Group", "ASRS_suma")], by = "Participant", all = TRUE)
esm_cortisol_data_aggregated <- na.omit(esm_cortisol_data_aggregated)

#relig
selected_columns <- c("Rel_1", "Rel_2", "Rel_3", 
                      "Rel_4", "Rel_5", "Rel_6", 
                      "Rel_7", "Rel_8", "Rel_9", 
                      "Rel_10")

screening_data$relig <- esmpack::combitems(selected_columns, screening_data, fun = "sum")
baza <- baza %>%
  left_join(select(screening_data, ID, relig), by = c("id" = "ID"))

selected_columns <- names(FKBP5_wywiad_data)[grep("^PLEs_[0-9]+_patient_control_thought", names(FKBP5_wywiad_data))]
screening_data$PLEs_control_thought <- esmpack::combitems(selected_columns, screening_data, fun = "sum")
baza <- baza %>%
  left_join(select(screening_data, ID, PLEs_control_thought), by = c("id" = "ID"))

selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_Unusual_thinking_contents_[A-Za-z]", names(FKBP5_wywiad_data))]
print(selected_columns)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, all_of(selected_columns)), by = c("id" = "ID"), relationship = "many-to-many")

#sex
baza <- baza %>%
  left_join(select(screening_data, ID, Gender, Group), by = c("id" = "ID")) %>%
  mutate(sex = ifelse(Gender == "1", "male", "female"),
         status = ifelse(Group == "1", "high", "low")
  ) %>%
  select(-Gender, -Group)

FKBP5_wywiad_data$Date_of_study <- anytime(FKBP5_wywiad_data$Date_of_study)
FKBP5_wywiad_data <- FKBP5_wywiad_data[order(as.Date(FKBP5_wywiad_data$Date_of_study, format="%d.%m.%Y"), decreasing = TRUE),]
FKBP5_wywiad_data <- FKBP5_wywiad_data[!duplicated(FKBP5_wywiad_data$ID), ]

#write_sav(esm_cortisol_data_aggregated, "out/esm_cortisol_data_aggregated.sav")

tmp<- factor(FKBP5_wywiad_data$MINI_depression, levels = c("Wartość1", "Wartość2"))

                  
data_frame$MINI_depression <- factor(data_frame$MINI_depression, labels = c("Tak", "Nie"))
                  
# Dodanie opisu zmiennej
attr(data_frame$MINI_depression, "label") <- "Czy doświadczał(a) pan(i) epizodu depresyjnego zgodnie z MINI?"

#PLEs - wywiad
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_ples_[0-9]", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$ples <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, ples), by = c("id" = "ID"), relationship = "many-to-many")

#Unusual_thinking
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_Unusual_thinking_contents_[0-9]", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$utc <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, utc), by = c("id" = "ID"), relationship = "many-to-many")


#other_thinking_content
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_other_thinking_content_[0-9]", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$otc <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, otc), by = c("id" = "ID"), relationship = "many-to-many")


#hallucinations
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_hallucinations_[0-9]", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$hal <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, hal), by = c("id" = "ID"), relationship = "many-to-many")

#wywiad suma
baza$wywiad_sum <- rowSums(baza[, c("ples", "utc", "otc", "hal")], na.rm = TRUE)

#Dodanie kolumny meas_number_day do esm_cortisol_data
esm_cortisol_data <- esm_cortisol_data %>%
  left_join(esm_cortisol_data_by_date %>% select(Participant, Trigger_date, Trigger_time, meas_number_day),
            by = c("Participant", "Trigger_date", "Trigger_time"))


tmp <- esm_cortisol_data %>% 
  group_by(Participant) %>%
  filter(n() != 42) %>%
  select(Participant, day, meas_number_day, Trigger_date, Trigger_time) %>%
  arrange(Participant, day, meas_number_day)

tmp_missing_days <- tmp %>%
  group_by(Participant) %>%
  filter(length(unique(day)) != 7) %>%
  distinct(Participant) %>%
  left_join(tmp, by = "Participant") %>%
  group_by(Participant) %>%
  summarise(missing_days = setdiff(1:7, day))


generate_missing_measurements <- function(df) {
  missing_measurements <- df %>%
    group_by(Participant) %>%
    filter(length(unique(day)) != 7) %>%
    distinct(Participant, .keep_all = TRUE) %>%
    group_by(Participant, day) %>%
    summarise(missing_days = list(setdiff(1:7, day))) %>%
    unnest(missing_days) %>%
    mutate(Trigger_date = ifelse(!is.na(Trigger_date) & day == 1, Trigger_date, 
                                 ifelse(!is.na(Trigger_date), Trigger_date + lubridate::days(day - 1), NA)),
           meas_number_day = 1:6,
           Trigger_time = NA)
  
  return(missing_measurements)
}


# Dodanie brakujących pomiarów do oryginalnych danych
esm_cortisol_data_with_missing <- bind_rows(esm_cortisol_data, generate_missing_measurements(esm_cortisol_data))

esm_cortisol_data3$Form_upload_time <- format(esm_cortisol_data3$Form_upload_time, "%H:%M:%S")


#FKBP5_EMA_DATA
#PSWQ
selected_columns <- names(FKBP5_ema_data)[grep("^PSWQ_[0-9]", names(FKBP5_ema_data))]
print(selected_columns)
FKBP5_ema_data$PSWQ_suma <- rowSums(FKBP5_ema_data[selected_columns])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PSWQ_suma), by = "ID")

#PTQ
selected_columns <- names(FKBP5_ema_data)[grep("^PTQ_[0-9]", names(FKBP5_ema_data))]
print(selected_columns)
FKBP5_ema_data$PTQ_suma <- rowSums(FKBP5_ema_data[selected_columns])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_suma), by = "ID")

#PTQ - repetitiveness
selected_columns_2 <- selected_columns[c(1, 6, 11)]
print(selected_columns_2)
FKBP5_ema_data$PTQ_repetitiveness <- rowSums(FKBP5_ema_data[selected_columns_2])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_repetitiveness), by = "ID")

#PTQ - intrusiveness
selected_columns_2 <- selected_columns[c(2, 7, 12)]
print(selected_columns_2)
FKBP5_ema_data$PTQ_intrusiveness <- rowSums(FKBP5_ema_data[selected_columns_2])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_intrusiveness), by = "ID")

#PTQ - disengaging
selected_columns_2 <- selected_columns[c(3, 8, 13)]
print(selected_columns_2)
FKBP5_ema_data$PTQ_disengaging <- rowSums(FKBP5_ema_data[selected_columns_2])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_disengaging), by = "ID")

#PTQ - unproductiveness
selected_columns_2 <- selected_columns[c(4, 9, 14)]
print(selected_columns_2)
FKBP5_ema_data$PTQ_unproductiveness <- rowSums(FKBP5_ema_data[selected_columns_2])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_unproductiveness), by = "ID")

#PTQ - mental_resources
selected_columns_2 <- selected_columns[c(5, 10, 15)]
print(selected_columns_2)
FKBP5_ema_data$PTQ_mental_resources <- rowSums(FKBP5_ema_data[selected_columns_2])
mediation_data <- mediation_data %>%
  left_join(select(FKBP5_ema_data, ID, PTQ_mental_resources), by = "ID")

rm(selected_columns)
rm(selected_columns_2)


FKBP5_ema_data <- FKBP5_ema_data %>%
  filter(as.character(Finished) == "1")
tmp2 <- subset(FKBP5_ema_data, duplicated(Q72) | duplicated(Q72, fromLast = TRUE))
FKBP5_ema_data <- FKBP5_ema_data %>%
  filter(!(Q72 == "PUM03K" & as.character(Q1_1) == "3"))
FKBP5_ema_data <- FKBP5_ema_data %>%
  filter(!(Q72 == "PUM18B" & as.character(Q1_1) == "4"))
tmp3 <- subset(FKBP5_ema_data, duplicated(Q72) | duplicated(Q72, fromLast = TRUE))
tmp4 <- FKBP5_ema_data[!grepl("^(WWA|PUM|WRO)", FKBP5_ema_data$Q72), ]


attr(tmp_mediation_data$ID, "label") <- "ID"
attr(tmp_mediation_data$grupa, "label") <- "Group (H-high, L-low)"
attr(tmp_mediation_data$wiek, "label") <- "Age"
attr(tmp_mediation_data$płeć, "label") <- "Gender"
attr(tmp_mediation_data$Edu_lvl, "label") <- "Education level"
attr(tmp_mediation_data$Edu_year, "label") <- "Education - number of years"
attr(tmp_mediation_data$BMI, "label") <- "BMI"
attr(tmp_mediation_data$ASRS_sum, "label") <- "ASRS total"
attr(tmp_mediation_data$PTQ_sum, "label") <- "PTQ total"
attr(tmp_mediation_data$CAA_poz, "label") <- "CAARMS positive symptoms"
attr(tmp_mediation_data$CAA_neg, "label") <- "CAARMS negative symptoms"
attr(tmp_mediation_data$CAA_sum, "label") <- "CAARMS total"
attr(tmp_mediation_data$Ru_m_we, "label") <- "Ruminations mean for the week"
attr(tmp_mediation_data$Depression_family, "label") <- "Depression history in family"
attr(tmp_mediation_data$Schizophrenia_family, "label") <- "Schizofrenia history in family"