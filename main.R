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
#version

# Wczytanie danych z pliku .sav
WWA_all_screening_data <- read_sav("data/WWA_all_screening.sav")
esm_cortisol_data <- read_sav("data/esm_cortisol.sav")
FKBP5_wywiad_March_2024_data <- read_sav("data/FKBP5_wywiad_March_2024.sav")

# Przypisanie nazw kolumn zaczynających się od "PLES" do nowego wektora
#ASRS_cols <- names(WWA_all_screening_data)[startsWith(names(WWA_all_screening_data), "ASRS")]
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

esm_cortisol_data$Cortisol_ng_ml
# Zamiana przecinków na kropki
esm_cortisol_data$Cortisol_ng_ml <- gsub(",", ".", esm_cortisol_data$Cortisol_ng_ml)

# Konwersja kolumny na typ numeryczny
esm_cortisol_data$Cortisol_ng_ml <- as.numeric(esm_cortisol_data$Cortisol_ng_ml)

#Sortowanie najpierw wzglendem Participant, a potem wzglendem Trigger_date
esm_cortisol_data <- esm_cortisol_data %>%
  arrange(Participant, Trigger_date)

esm_cortisol_data_by_date <- esm_cortisol_data %>% 
  group_by(Participant, Trigger_date) %>%
  filter(n() == 6) %>%
  select(Participant, Trigger_date, Trigger_time, f1_PLEs_sum, Cortisol_ng_ml) %>%
  arrange(Participant, Trigger_date, Trigger_time) %>%
  mutate(meas_number = row_number())

df_2 <- esm_cortisol_data_by_date %>%
  group_by(meas_number) %>%
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
  select(Participant, Trigger_date, Trigger_time, f1_PLEs_sum, Cortisol_ng_ml) %>%
  arrange(Participant, Trigger_date, Trigger_time)


esm_cortisol_data_missing_rows <- esm_cortisol_data_missing_rows %>%
  mutate(meas_number = NA)  # Inicjalizacja kolumny row_number jako NA

# Iteracja po wierszach esm_cortisol_data_missing_rows_2
esm_cortisol_data_missing_rows$meas_number <- sapply(1:nrow(esm_cortisol_data_missing_rows), function(i) {
  row <- esm_cortisol_data_missing_rows[i, ]  # Wybierz i-ty wiersz
  # Znajdź odpowiedni wiersz z df_2 dla aktualnego wiersza
  df_row <- df_2 %>%
    filter(row$Trigger_time >= min_time & row$Trigger_time <= max_time) %>%
    slice(1)  # Wybierz pierwszy pasujący wiersz
  return(df_row$meas_number)
})

df_duplicated <- esm_cortisol_data_missing_rows %>%
  group_by(Participant, Trigger_date) %>%
  filter(any(duplicated(meas_number))) %>%
  ungroup()


esm_cortisol_data_by_date <- bind_rows(esm_cortisol_data_missing_rows, esm_cortisol_data_by_date)
esm_cortisol_data_by_date <- esm_cortisol_data_by_date %>%
  arrange(Participant, Trigger_date)

esm_cortisol_data_aggregated <- esm_cortisol_data_by_date %>%
  group_by(Participant, meas_number) %>%
  summarize(
    PLEs_sum = mean(f1_PLEs_sum, na.rm = TRUE),
    Cortisol_ng_ml = mean(Cortisol_ng_ml, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = meas_number, values_from = c(PLEs_sum, Cortisol_ng_ml), names_prefix = "meas_")


names(WWA_all_screening_data)[1] <- "Participant"
esm_cortisol_data_aggregated <- merge(esm_cortisol_data_aggregated, WWA_all_screening_data[, c("Participant", "Group", "ASRS_suma")], by = "Participant", all = TRUE)
esm_cortisol_data_aggregated <- na.omit(esm_cortisol_data_aggregated)

write_sav(esm_cortisol_data_aggregated, "out/esm_cortisol_data_aggregated.sav")