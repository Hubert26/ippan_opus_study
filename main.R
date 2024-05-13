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
esm_cortisol_data <- read_sav("data/clear_esm_cortisol_data.sav")
mediation_data <- read_sav("data/clear_mediation_data.sav")





###################
#KORTYZOL
esm_cortisol_data_by_date <- esm_cortisol_data %>% 
  group_by(Participant, Trigger_date) %>%
  filter(n() == 6) %>% #Filtruje grupy, pozostawiając tylko te, które mają dokładnie 6 obserwacji
  select(Participant, Trigger_date, Trigger_time) %>%
  arrange(Participant, Trigger_date, Trigger_time) %>%
  mutate(meas_number_day = row_number()) #Dodaje nową kolumnę meas_number, która numeruje obserwacje w każdej grupie.



df_2 <- esm_cortisol_data_by_date %>%
  group_by(meas_number_day) %>%
  summarize(min_time = min(Trigger_time),
            max_time = max(Trigger_time))

df_2 <- df_2 %>%
  mutate(diff_time = lead(min_time) - max_time)

df_2 <- df_2 %>%
  mutate(
    max_time = max_time + floor(diff_time/2),
    min_time = min_time - ceiling(lag(diff_time)/2-1)
  ) %>%
  mutate(
    min_time = ifelse(row_number() == 1, 0, min_time),
    max_time = ifelse(row_number() == n(), Inf, max_time)
  ) %>%
  select(-diff_time)

esm_cortisol_data_missing_rows <- esm_cortisol_data %>%
  group_by(Participant, Trigger_date) %>%
  filter(n() != 6) %>%
  select(Participant, Trigger_date, Trigger_time) %>%
  arrange(Participant, Trigger_date, Trigger_time)

esm_cortisol_data_missing_rows <- esm_cortisol_data_missing_rows %>%
  mutate(meas_number_day = NA)  # Inicjalizacja kolumny row_number jako NA

# Przyporządkowanie meas_number na podstawie czasów z df_2
esm_cortisol_data_missing_rows$meas_number_day <- sapply(1:nrow(esm_cortisol_data_missing_rows), function(i) {
  row <- esm_cortisol_data_missing_rows[i, ]  # Wybierz i-ty wiersz
  # Znajdź odpowiedni wiersz z df_2 dla aktualnego wiersza
  df_row <- df_2 %>%
    filter(row$Trigger_time >= min_time & row$Trigger_time <= max_time) %>%
    slice(1)  # Wybierz pierwszy pasujący wiersz
  return(df_row$meas_number_day)
})

#Sprawdzenie czy są jakieś zduplikowane wartości meas_number u Participant w Trigger_date
df_duplicated <- esm_cortisol_data_missing_rows %>%
  group_by(Participant, Trigger_date) %>%
  filter(any(duplicated(meas_number_day)))

#Wybranie zduplikownanych wierszy
df_duplicated_row <- df_duplicated %>%
  filter(duplicated(meas_number_day))

#Usunięcie zduplikowanych wierszy
esm_cortisol_data_missing_rows <- anti_join(esm_cortisol_data_missing_rows, df_duplicated_row)


#Połączenie i posortowanie
esm_cortisol_data_by_date <- bind_rows(esm_cortisol_data_missing_rows, esm_cortisol_data_by_date)
esm_cortisol_data_by_date <- esm_cortisol_data_by_date %>%
  arrange(Participant, Trigger_date, Trigger_time)

#Dodanie kolumny meas_number_day do esm_cortisol_data
esm_cortisol_data <- left_join(esm_cortisol_data_by_date, 
                               esm_cortisol_data,
                               by = c("Participant", "Trigger_date", "Trigger_time"))


#Sprawdzenie czy wszystkie PORANKI mają meas_number == 1
esm_cortisol_data_filtered <- esm_cortisol_data %>%
  filter(meas_number_day == 1 & Form != "PORANEK")

rm(df_2)
rm(df_duplicated)
rm(df_duplicated_row)
rm(esm_cortisol_data_missing_rows)
rm(esm_cortisol_data_filtered)
rm(esm_cortisol_data_by_date)


#day, tothours
esm_cortisol_data <- esm_cortisol_data %>% 
  group_by(Participant) %>%
  arrange(Participant, Trigger_date) %>%
  mutate(
    day = as.integer(difftime(Trigger_date, min(Trigger_date), units = "days")) + 1,
    tothours = as.integer(difftime(Trigger_date, min(Trigger_date), units = "hours")) + as.numeric(hour(Form_start_time)) + minute(Form_start_time)/60
    )


#MISSING DAYS
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
  summarise(day = setdiff(1:7, day))

tmp_missing_days_multiplied <- map_dfr(1:6, ~ tmp_missing_days)

tmp_missing_days_multiplied <- tmp_missing_days_multiplied %>%
  group_by(Participant) %>%
  mutate(meas_number_day = row_number()) %>%
  arrange(Participant, meas_number_day)

esm_cortisol_data <- bind_rows(esm_cortisol_data, tmp_missing_days_multiplied)

rm(tmp)
rm(tmp_missing_days_multiplied)
rm(tmp_missing_days)


#MISSING MEAS
tmp_2 <- esm_cortisol_data %>% 
  group_by(Participant) %>%
  filter(n() != 42) %>%
  select(Participant, day, meas_number_day, Trigger_date, Trigger_time) %>%
  arrange(Participant, day, meas_number_day)

tmp_missing_meas <- tmp_2 %>%
  group_by(Participant, day) %>%
  filter(length(unique(meas_number_day)) != 6)

tmp_missing_meas_1 <- tmp_missing_meas %>%
  select(Participant, day) %>%
  distinct() %>%
  ungroup()

tmp_missing_meas_1 <- bind_rows(replicate(6, tmp_missing_meas_1, simplify = FALSE))

tmp_missing_meas_1 <- tmp_missing_meas_1 %>%
  group_by(Participant, day)  %>%
  arrange(Participant, day) %>%
  mutate(meas_number_day = row_number()) %>%
  ungroup()

difference <- anti_join(tmp_missing_meas_1, tmp_missing_meas, by = c("Participant", "day", "meas_number_day"))

esm_cortisol_data <- bind_rows(esm_cortisol_data, difference)

rm(difference)
rm(tmp_missing_meas_1)
rm(tmp_missing_meas)
rm(tmp_2)

#UZUPEŁNIENIE TRRIGER_DATE
esm_cortisol_data <- esm_cortisol_data %>%
  group_by(Participant) %>%
  mutate(Trigger_date = case_when(
    is.na(Trigger_date) ~ min(Trigger_date) + days(day - 1),
    TRUE ~ Trigger_date
  )) %>%
  ungroup()

#MEAS_NUMBER_WEEK
esm_cortisol_data <- esm_cortisol_data %>%
  group_by(Participant) %>%
  arrange(day, meas_number_day) %>%
  mutate(meas_number_week = row_number()) %>%
  ungroup()

esm_cortisol_data <- esm_cortisol_data %>% 
  arrange(Participant, day, meas_number_day) %>%
  select(Participant, day, meas_number_day, meas_number_week, everything())







#BAZA

baza <- data.frame(
  id = esm_cortisol_data$Participant,
  obs = esm_cortisol_data$Trigger_counter,
  beep_d = esm_cortisol_data$meas_number_day,
  beep_w = esm_cortisol_data$meas_number_week,
  day = esm_cortisol_data$day,
  beeptime = as.numeric(hour(esm_cortisol_data$Trigger_time) * 60 + minute(esm_cortisol_data$Trigger_time)),
  resptime = as.numeric(hour(esm_cortisol_data$Form_start_time) * 60 + minute(esm_cortisol_data$Form_start_time)),
  resphour = as.numeric(hour(esm_cortisol_data$Form_start_time) + minute(esm_cortisol_data$Form_start_time) / 60),
  tothours = esm_cortisol_data$tothours,
  finishtime = as.numeric(hour(esm_cortisol_data$Form_finish_time) * 60 + minute(esm_cortisol_data$Form_finish_time)),
  cortisoltime = as.numeric(hour(esm_cortisol_data$Cortisol_time) * 60 + minute(esm_cortisol_data$Cortisol_time)),
  cortlevel = esm_cortisol_data$Cortisol_ng_ml * 100,
  sleeplength = esm_cortisol_data$sleep_hours_item_1972,
  sleep_quality = esm_cortisol_data$sleep_evaluation_item_a2
) %>%
  mutate(
    eventstress = ifelse(beep_d == 1, esm_cortisol_data$Importatnt_event_item_a3, esm_cortisol_data$Importatnt_event_item_1),
    caffsmok = ifelse(beep_d == 1, esm_cortisol_data$caffeine_tobacco_item_2109, esm_cortisol_data$caffeine_tobacco_item_1858),
    eatdrink = ifelse(beep_d == 1, esm_cortisol_data$food_soft_drink_item_2133, esm_cortisol_data$food_soft_drink_item_1901),
    alcohol = ifelse(beep_d == 1, esm_cortisol_data$alcohol_item_2157, esm_cortisol_data$alcohol_item_1931),
    body = ifelse(beep_d == 1, esm_cortisol_data$body_image_item_a7, esm_cortisol_data$body_image_item_5),
    rumination = ifelse(beep_d == 1, esm_cortisol_data$ruminations_item_2041, esm_cortisol_data$ruminations_item_1832),
    actstress1 = ifelse(beep_d == 1, esm_cortisol_data$rather_do_something_else_a8, esm_cortisol_data$rather_do_something_else_item_6),
    actstress2 = ifelse(beep_d == 1, esm_cortisol_data$difficult_event_item_a9, esm_cortisol_data$difficult_event_item_7),
    actstress2 = ifelse(beep_d == 1, esm_cortisol_data$nice_event_item_a10, esm_cortisol_data$nice_event_item_8),
    socstress1a = ifelse(beep_d == 1, esm_cortisol_data$being_alone_item_a11_1, esm_cortisol_data$being_alone_item_9_1),
    socstress1b = ifelse(beep_d == 1, esm_cortisol_data$being_with_family_item_a11_2, esm_cortisol_data$being_with_family_item_9_2),
    socstress1c = ifelse(beep_d == 1, esm_cortisol_data$being_with_partner_item_a11_3, esm_cortisol_data$being_with_partner_item_9_3),
    socstress1d = ifelse(beep_d == 1, esm_cortisol_data$being_with_friends_item_a11_4, esm_cortisol_data$being_with_friends_item_9_4),
    socstress1e = ifelse(beep_d == 1, esm_cortisol_data$being_with_strangers_item_a11_5, esm_cortisol_data$being_with_strangers_item_9_5),
    socstress1f = ifelse(beep_d == 1, esm_cortisol_data$being_with_co_workers_item_a11_6, esm_cortisol_data$being_with_co_workers_item_9_6),
    socstress1 = ifelse(beep_d == 1, esm_cortisol_data$rather_be_alone_item_a12, esm_cortisol_data$rather_be_alone_item_10),
    socstress2 = ifelse(beep_d == 1, esm_cortisol_data$enjoyable_being_with_people_item_a13, esm_cortisol_data$enjoyable_being_with_people_item_11),
    outsider = ifelse(beep_d == 1, esm_cortisol_data$odludek_item_a14, esm_cortisol_data$odludek_item_12),
    helpless1 = ifelse(beep_d == 1, esm_cortisol_data$unsuspected_event_item_a15, esm_cortisol_data$unsuspected_event_item_13),
    helpless2 = ifelse(beep_d == 1, esm_cortisol_data$loss_of_control_item_a16, esm_cortisol_data$loss_of_control_item_14),
    selfeff1 = ifelse(beep_d == 1, esm_cortisol_data$coping_problems_item_a17, esm_cortisol_data$problems_coping_item_15),
    selfeff2 = ifelse(beep_d == 1, esm_cortisol_data$things_going_my_way_item_a18, esm_cortisol_data$things_going_my_way_item_16),
    areastress = ifelse(beep_d == 1, esm_cortisol_data$unpleasant_to_be_here_item_a19, esm_cortisol_data$unpleasant_to_be_here_item_17),
    anxious = ifelse(beep_d == 1, esm_cortisol_data$feel_anxious_item_a20, esm_cortisol_data$feel_anxious_item_18),
    down = ifelse(beep_d == 1, esm_cortisol_data$feel_depressed_item_a21, esm_cortisol_data$feel_depressed_item_19),
    lonely = ifelse(beep_d == 1, esm_cortisol_data$feel_alone_item_a22, esm_cortisol_data$feel_alone_item_20),
    insecure = ifelse(beep_d== 1, esm_cortisol_data$dont_feel_safe_item_a23, esm_cortisol_data$dont_feel_safe_item_21),
    annoyed = ifelse(beep_d == 1, esm_cortisol_data$feel_irritated_a24, esm_cortisol_data$feel_irritated_item_22),
    as = ifelse(beep_d == 1, esm_cortisol_data$attention_attracts_item_a25, esm_cortisol_data$attention_attracts_item_23),
    as2 = ifelse(beep_d == 1, esm_cortisol_data$meaning_item_a26, esm_cortisol_data$meaning_item_24),
    as3 = ifelse(beep_d == 1, esm_cortisol_data$noticing_item_a27, esm_cortisol_data$noticing_item_25),
    ta1 = ifelse(beep_d == 1, esm_cortisol_data$unpleasant_anticipation_item_a28, esm_cortisol_data$unpleasant_anticipation_item_26),
    ta2 = ifelse(beep_d == 1, esm_cortisol_data$carefulness_item_a29, esm_cortisol_data$carefulness_item_27),
    ta3 = ifelse(beep_d == 1, esm_cortisol_data$details_item_a30, esm_cortisol_data$details_item_28),
    h1 = ifelse(beep_d == 1, esm_cortisol_data$hearing_thoughts_item_a31, esm_cortisol_data$hearing_thoughts_item_29),
    h2 = ifelse(beep_d == 1, esm_cortisol_data$voices_item_a32, esm_cortisol_data$voices_item_30),
    h3 = ifelse(beep_d == 1, esm_cortisol_data$hallucinations_item_a33, esm_cortisol_data$hallucinations_item_31),
    d1 = ifelse(beep_d == 1, esm_cortisol_data$invisible_force_item_a34, esm_cortisol_data$invisible_force_item_32),
    d2 = ifelse(beep_d == 1, esm_cortisol_data$hidden_meaning_item_a35, esm_cortisol_data$hidden_meaning_item_33),
    d3 = ifelse(beep_d == 1, esm_cortisol_data$unreal_experience_item_a36, esm_cortisol_data$unreal_experience_item_34),
    d4 = ifelse(beep_d == 1, esm_cortisol_data$thought_control_item_a37, esm_cortisol_data$thought_control_item_35),
    d5 = ifelse(beep_d == 1, esm_cortisol_data$get_rid_of_thoughts_item_38, esm_cortisol_data$get_rid_of_thoughts_item_36),
    )

#Kortyzol - pomiar1
mean_cortlevel_by_id <- baza %>%
  filter(beep_d == 1) %>%
  group_by(id) %>%
  summarize(Cortisol_1_mean = mean(cortlevel, na.rm = TRUE))

baza <- baza %>%
  left_join(select(mean_cortlevel_by_id, id, Cortisol_1_mean), by = "id")

rm(mean_cortlevel_by_id)

#Kortyzol - pomiar23456
mean_cortlevel_by_id <- baza %>%
  filter(beep_d != 1) %>%
  group_by(id) %>%
  summarize(Cortisol_23456_mean = mean(cortlevel, na.rm = TRUE))

baza <- baza %>%
  left_join(select(mean_cortlevel_by_id, id, Cortisol_23456_mean), by = "id")

#Kortyzol - week
mean_cortlevel_by_id <- baza %>%
  group_by(id) %>%
  summarize(Cortisol_mean_week = mean(cortlevel, na.rm = TRUE))

baza <- baza %>%
  left_join(select(mean_cortlevel_by_id, id, Cortisol_mean_week), by = "id")

rm(mean_cortlevel_by_id)

#Ruminacje mean - week
baza <- baza %>%
  group_by(id) %>%
  mutate(Rumination_mean_week = mean(rumination, na.rm = TRUE))

#Ruminacje mean - day
baza <- baza %>%
  group_by(id, day) %>%
  mutate(Rumination_mean_day = mean(rumination, na.rm = TRUE)) %>%
  ungroup()


#PLES by meas - esm_cortisol
selected_columns <- c("as2", "as3", "ta1", "ta2", "ta3", "h1", "h2", "h3", "d1", "d2", "d3", "d4", "d5")
baza$esm_PLEs_meas <- esmpack::combitems(selected_columns, baza, fun = "sum")

#PLES by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(esm_PLEs_mean_week = mean(esm_PLEs_meas, na.rm = TRUE))

#PLES by day - esm_cortisol
baza <- baza %>%
  group_by(id, day) %>%
  mutate(esm_PLEs_mean_day = mean(esm_PLEs_meas, na.rm = TRUE)) %>%
  ungroup()

#PLES_sum by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(esm_PLEs_sum_week = sum(esm_PLEs_meas, na.rm = TRUE),
         esm_PLEs_mean_week = mean(esm_PLEs_meas, na.rm = TRUE)
         )

#socstress1a by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1a_sum_week = sum(socstress1a, na.rm = TRUE),
         socstress1a_mean_week = mean(socstress1a, na.rm = TRUE)
         )

#socstress1b by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1b_sum_week = sum(socstress1b, na.rm = TRUE),
         socstress1b_mean_week = mean(socstress1b, na.rm = TRUE)
         )

#socstress1c by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1c_sum_week = sum(socstress1c, na.rm = TRUE),
         socstress1c_mean_week = mean(socstress1c, na.rm = TRUE)
         )

#socstress1d by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1d_sum_week = sum(socstress1d, na.rm = TRUE),
         socstress1d_mean_week = mean(socstress1d, na.rm = TRUE)
         )

#socstress1e by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1e_sum_week = sum(socstress1e, na.rm = TRUE),
         socstress1e_mean_week = mean(socstress1e, na.rm = TRUE)
         )

#socstress1f by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1f_sum_week = sum(socstress1f, na.rm = TRUE),
         socstress1f_mean_week = mean(socstress1f, na.rm = TRUE)
         )

#socstress1 by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress1_sum_week = sum(socstress1, na.rm = TRUE),
         socstress1_mean_week = mean(socstress1, na.rm = TRUE)
         )

#socstress2 by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(socstress2_sum_week = sum(socstress2, na.rm = TRUE),
         socstress2_mean_week = mean(socstress2, na.rm = TRUE)
         )


#anxious by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(anxious_sum_week = sum(anxious, na.rm = TRUE),
         anxious_mean_week = mean(anxious, na.rm = TRUE)
         )

#down by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(down_sum_week = sum(down, na.rm = TRUE),
         down_mean_week = mean(down, na.rm = TRUE)
         )

#lonely by week - esm_cortisol
baza <- baza %>%
  group_by(id) %>%
  mutate(lonely_sum_week = sum(lonely, na.rm = TRUE),
         lonely_mean_week = mean(lonely, na.rm = TRUE)
         )

#insecure by week - esm_cortisol
baza <- baza %>%
  mutate(insecure = as.numeric(insecure)) %>%
  group_by(id) %>%
  mutate(insecure_sum_week = sum(insecure, na.rm = TRUE),
         insecure_mean_week = mean(insecure, na.rm = TRUE)
         )

#annoyed by week - esm_cortisol
baza <- baza %>%
  mutate(annoyed = as.numeric(annoyed)) %>%
  group_by(id) %>%
  mutate(annoyed_sum_week = sum(annoyed, na.rm = TRUE),
         annoyed_mean_week = mean(annoyed, na.rm = TRUE)
         )


#as by week - esm_cortisol
baza <- baza %>%
  mutate(as_meas = as.numeric(as) + as.numeric(as2) + as.numeric(as3)) %>%
  group_by(id) %>%
  mutate(as_sum_week = sum(as_meas, na.rm = TRUE),
         as_mean_week = mean(as_meas, na.rm = TRUE)
         )

#ta by week - esm_cortisol
baza <- baza %>%
  mutate(ta_meas = as.numeric(ta1) + as.numeric(ta2) + as.numeric(ta3)) %>%
  group_by(id) %>%
  mutate(ta_sum_week = sum(ta_meas, na.rm = TRUE),
         ta_mean_week = mean(ta_meas, na.rm = TRUE)
         )

#h by week - esm_cortisol
baza <- baza %>%
  mutate(h_meas = as.numeric(h1) + as.numeric(h2) + as.numeric(h3)) %>%
  group_by(id) %>%
  mutate(h_sum_week = sum(h_meas, na.rm = TRUE),
         h_mean_week = mean(h_meas, na.rm = TRUE)
         )

#d by week - esm_cortisol
baza <- baza %>%
  mutate(d_meas = as.numeric(d1) + as.numeric(d2) + as.numeric(d3) + as.numeric(d4)) %>%
  group_by(id) %>%
  mutate(d_sum_week = sum(d_meas, na.rm = TRUE),
         d_mean_week = mean(d_meas, na.rm = TRUE))

#MEDIATION_DATA
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














#ZAPISYWANIE
#write_sav(baza, "out/baza.sav")
write_sav(mediation_data, "out/mediation_data.sav")

