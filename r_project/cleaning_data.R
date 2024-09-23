library(haven)
#install.packages("haven")
#remotes::install_github("wviechtb/esmpack")
#library(esmpack)
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

#version



# ================================================
# esm_cortisol_data
# ================================================

# Wczytanie danych z pliku

# Funkcja do wczytania wszystkich arkuszy z pliku Excel i konwersji wszystkich kolumn na typ tekstowy
read_all_sheets <- function(file_path) {
  sheets <- excel_sheets(file_path)
  data_list <- lapply(sheets, function(sheet) {
    data <- read_excel(file_path, sheet = sheet)
    data <- mutate_all(data, as.character)  # Konwersja wszystkich kolumn na tekst
    return(data)
  })
  combined_data <- bind_rows(data_list, .id = "sheet")
  return(combined_data)
}

# Wczytanie danych ze wszystkich arkuszy z dwóch plików
esm_cortisol_data_WRO <- read_all_sheets("data/data_cleaning/esm-cortisol-WRO_20240715.xlsx")
esm_cortisol_data_SZCZ <- read_all_sheets("data/data_cleaning/esm-cortisol-SZCZ_20240923.xlsx")
esm_cortisol_data_WWA <- read_excel("data/data_cleaning/esm-cortisol-WWA_20240917.xlsx")

# Usunięcie kolumn z nazwą arkusza
esm_cortisol_data_SZCZ <- esm_cortisol_data_SZCZ %>%
  select(-sheet)
esm_cortisol_data_WRO <- esm_cortisol_data_WRO %>%
  select(-sheet)

# Usunięcie duplikatów na podstawie wszystkich kolumn
esm_cortisol_data_WRO <- esm_cortisol_data_WRO %>% 
  distinct()
esm_cortisol_data_SZCZ <- esm_cortisol_data_SZCZ %>% 
  distinct()
esm_cortisol_data_WWA <- esm_cortisol_data_WWA %>% 
  distinct()


# Przekonwertowanie całego dataframe na tekst
esm_cortisol_data_WWA <- esm_cortisol_data_WWA %>%
  mutate_all(as.character)
esm_cortisol_data_SZCZ <- esm_cortisol_data_SZCZ %>%
  mutate_all(as.character)
esm_cortisol_data_WRO <- esm_cortisol_data_WRO %>%
  mutate_all(as.character)


#----------STANDARYZACJA KOLUMN----------
# Pobranie nazw kolumn dla każdej ramki danych
col_names_WWA <- colnames(esm_cortisol_data_WWA)
col_names_WRO <- colnames(esm_cortisol_data_WRO)
col_names_SZCZ <- colnames(esm_cortisol_data_SZCZ)

# Znalezienie różnic między kolumnami
diff_WWA_WRO <- setdiff(col_names_WWA, col_names_WRO)
diff_WRO_WWA <- setdiff(col_names_WRO, col_names_WWA)

diff_WWA_SZCZ <- setdiff(col_names_WWA, col_names_SZCZ)
diff_SZCZ_WWA <- setdiff(col_names_SZCZ, col_names_WWA)

diff_WRO_SZCZ <- setdiff(col_names_WRO, col_names_SZCZ)
diff_SZCZ_WRO <- setdiff(col_names_SZCZ, col_names_WRO)

# Wyświetlenie różnic
print("Różnice między esm_cortisol_data_WWA a esm_cortisol_data_WRO:")
print(diff_WWA_WRO)
print(diff_WRO_WWA)

print("Różnice między esm_cortisol_data_WWA a esm_cortisol_data_SZCZ:")
print(diff_WWA_SZCZ)
print(diff_SZCZ_WWA)

print("Różnice między esm_cortisol_data_WRO a esm_cortisol_data_SZCZ:")
print(diff_WRO_SZCZ)
print(diff_SZCZ_WRO)

#zamiana nazw kolumn 
esm_cortisol_data_WWA <- dplyr::rename(esm_cortisol_data_WWA, Bad_interval_over_20_min_or_missing = `Bad interval >20 min or missing`)

print("----------CHECKING COLUMNS----------")

# Pobranie nazw kolumn dla każdej ramki danych
col_names_WWA <- colnames(esm_cortisol_data_WWA)
col_names_WRO <- colnames(esm_cortisol_data_WRO)
col_names_SZCZ <- colnames(esm_cortisol_data_SZCZ)

# Znalezienie różnic między kolumnami
diff_WWA_WRO <- setdiff(col_names_WWA, col_names_WRO)
diff_WRO_WWA <- setdiff(col_names_WRO, col_names_WWA)

diff_WWA_SZCZ <- setdiff(col_names_WWA, col_names_SZCZ)
diff_SZCZ_WWA <- setdiff(col_names_SZCZ, col_names_WWA)

diff_WRO_SZCZ <- setdiff(col_names_WRO, col_names_SZCZ)
diff_SZCZ_WRO <- setdiff(col_names_SZCZ, col_names_WRO)

# Wyświetlenie różnic
print("Różnice między esm_cortisol_data_WWA a esm_cortisol_data_WRO:")
print(diff_WWA_WRO)
print(diff_WRO_WWA)

print("Różnice między esm_cortisol_data_WWA a esm_cortisol_data_SZCZ:")
print(diff_WWA_SZCZ)
print(diff_SZCZ_WWA)

print("Różnice między esm_cortisol_data_WRO a esm_cortisol_data_SZCZ:")
print(diff_WRO_SZCZ)
print(diff_SZCZ_WRO)


rm(diff_WWA_WRO)
rm(diff_WWA_SZCZ)
rm(diff_WRO_WWA)
rm(diff_WRO_SZCZ)
rm(diff_SZCZ_WWA)
rm(diff_SZCZ_WRO)

rm(col_names_WWA)
rm(col_names_WRO)
rm(col_names_SZCZ)


#dont_feel_safe_item_a23
#esm_cortisol_data_WWA$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_WWA$dont_feel_safe_item_a23)
#esm_cortisol_data_WRO$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_WRO$dont_feel_safe_item_a23)
#esm_cortisol_data_SZCZ$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_SZCZ$dont_feel_safe_item_a23)


#----------SCALANIE----------
esm_cortisol_data <- bind_rows(esm_cortisol_data_WWA, esm_cortisol_data_WRO, esm_cortisol_data_SZCZ)

#----------WRONG ID----------
tmp_wrongID <- esm_cortisol_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", esm_cortisol_data$Participant), ]

rm(tmp_wrongID)


#----------DATA FORMATING----------
#Trigger_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Trigger_date = as.Date(Trigger_date))
#Trigger_time
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Trigger_time = as_hms(sub(".*\\s", "", Trigger_time)))

#Form_start_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_start_date = as.Date(Form_start_date))
#Form_start_time
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_start_time = as_hms(sub(".*\\s", "", Form_start_time)))

#Form_finish_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_finish_date = as.Date(Form_finish_date))
#Form_finish_time
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_finish_time = as_hms(sub(".*\\s", "", Form_finish_time)))

#Form_upload_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_upload_date = as.Date(Form_upload_date))
#Form_upload_time
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_upload_time = as_hms(sub(".*\\s", "", Form_upload_time)))


#Cortisol_time
value <- as.double(esm_cortisol_data$Cortisol_time)*24
hours <- as.integer(floor(value))
decimal_part <- value - hours
# Konwertuj resztę z dzielenia na minuty i sekundy
minutes <- as.integer(decimal_part * 60)
seconds <- as.integer((decimal_part * 60 - minutes) * 60)
hms_values <- paste0(sprintf("%02d", hours), ":", sprintf("%02d", minutes), ":", sprintf("%02d", seconds))
esm_cortisol_data$Cortisol_time <- hms_values

esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Cortisol_time = if_else(!grepl("^\\d{2}:\\d{2}:\\d{2}$", Cortisol_time), NA_character_, Cortisol_time))
esm_cortisol_data$Cortisol_time <- as_hms(esm_cortisol_data$Cortisol_time)

rm(value)
rm(hours)
rm(minutes)
rm(seconds)
rm(hms_values)
rm(decimal_part)


#Cortisol_ng_ml
# Zamiana przecinków na kropki
esm_cortisol_data$Cortisol_ng_ml <- gsub(",", ".", esm_cortisol_data$Cortisol_ng_ml)
# Konwersja kolumny na typ numeryczny
esm_cortisol_data$Cortisol_ng_ml <- as.double(esm_cortisol_data$Cortisol_ng_ml)


#----------DUPLIKATYx2----------
# Usunięcie duplikatów na podstawie wszystkich kolumn
esm_cortisol_data <- esm_cortisol_data %>% 
  distinct()

# Usunięcie duplikatów na podstawie "Participant", "Trigger_date", "Trigger_time"
duplikaty <- duplicated(esm_cortisol_data[, c("Participant", "Trigger_date", "Trigger_time")]) | duplicated(esm_cortisol_data[, c("Participant", "Trigger_date", "Trigger_time")], fromLast = TRUE)
wiersze_duplikatow <- esm_cortisol_data[duplikaty, ]
wiersze_duplikatow <- wiersze_duplikatow %>%
  arrange(Participant, Trigger_date, Trigger_time)

# Usunięcie wierszy z esm_cortisol_data, które mają duplikaty z wiersze_duplikatow i gdzie kolumna "Missing" nie jest NA
esm_cortisol_data <- anti_join(esm_cortisol_data, 
                               wiersze_duplikatow %>% 
                                 filter(!(Missing == "NA" | is.na(Missing))),
                               by = c("Participant", "Trigger_date", "Trigger_time", "Missing"))

rm(duplikaty)
rm(wiersze_duplikatow)


#----------UZUPEŁNIANIE NIEZAREJESTROWANYCH POMIARÓW----------
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


###################
#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("data/data_main/clear_esm_cortisol_data_", current_time, ".sav")
# Zapis danych
write_sav(esm_cortisol_data, filename)









# ================================================
# SCREENING_DATA
# ================================================

screening_data <- read_sav("data/data_cleaning/ALL_screening_20240426.sav")


screening_data$ID <- toupper(screening_data$ID)
screening_data$ID <- gsub(" ", "", screening_data$ID) #usunięcie spacji
screening_data <- arrange(screening_data, ID) #sortowanie po ID


#sprawdzenie
tmp_duplicated <- subset(screening_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_wrongID <- screening_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", screening_data$ID), ]


#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("data_main/clear_screening_data_", current_time, ".sav")
# Zapis danych
write_sav(screening_data, filename)








# ================================================
# FKBP5_WYWIAD_DATA
# ================================================

# Sprawdzenie bieżącego katalogu roboczego
print(getwd())

FKBP5_wywiad_data <- read_sav("data/data_cleaning/FKBP5+-+wywiad_July+12,+2024_16.03.sav")
#FKBP5_wywiad_data <- read_sav("C:/Users/huber/OneDrive/Dokumenty/GitHub/IPPAN_opus/data_cleaning/FKBP5+-+wywiad_May+2,+2024_10.57.sav")

#zamiana nazw kolumn
tmp_progress <- select(FKBP5_wywiad_data, Progress, Finished, StartDate, EndDate)
FKBP5_wywiad_col_names <- read_sav("data/data_cleaning/clear_FKBP5_wywiad_data.sav")
FKBP5_wywiad_col_names <- names(FKBP5_wywiad_col_names)
columns_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration__in_seconds_", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")
columns_to_keep <- !(names(FKBP5_wywiad_data) %in% columns_to_remove)
FKBP5_wywiad_data <- subset(FKBP5_wywiad_data, select = columns_to_keep)
names(FKBP5_wywiad_data) <- FKBP5_wywiad_col_names
FKBP5_wywiad_data <- cbind(tmp_progress, FKBP5_wywiad_data)

rm(tmp_progress)

FKBP5_wywiad_data$ID <- toupper(FKBP5_wywiad_data$ID) #wielkie litery
FKBP5_wywiad_data$ID <- gsub(" ", "", FKBP5_wywiad_data$ID) #usunięcie spacji
FKBP5_wywiad_data <- arrange(FKBP5_wywiad_data, ID) #sortowanie po ID

FKBP5_wywiad_data$ID_participant <- toupper(FKBP5_wywiad_data$ID_participant)
FKBP5_wywiad_data$ID <- gsub(" ", "", FKBP5_wywiad_data$ID_participant) #usunięcie spacji

tmp_mismatched <- subset(FKBP5_wywiad_data, ID != ID_participant)
tmp_wrongID <- FKBP5_wywiad_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_wywiad_data$ID), ]
tmp_duplicated <- subset(FKBP5_wywiad_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))


#czyszczenie
#mismatched
#FKBP5_wywiad_data$ID <- ifelse(FKBP5_wywiad_data$ID_participant == "WWA121", "WWA121", FKBP5_wywiad_data$ID)

#wrongID
FKBP5_wywiad_data$ID[FKBP5_wywiad_data$ID == "PUM12G"] <- "PUM12B"
FKBP5_wywiad_data$ID_participant[FKBP5_wywiad_data$ID_participant == "PUM12G"] <- "PUM12B"

tmp <- FKBP5_wywiad_data %>%
  filter(Participant_first_last_name == "Monika Augustyn")
FKBP5_wywiad_data$ID_participant[FKBP5_wywiad_data$Participant_first_last_name == "Monika Augustyn"] <- "WRO116"
FKBP5_wywiad_data$ID[FKBP5_wywiad_data$Participant_first_last_name == "Monika Augustyn"] <- "WRO116"

tmp_wrongID <- FKBP5_wywiad_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_wywiad_data$ID), ]
FKBP5_wywiad_data <- anti_join(FKBP5_wywiad_data, tmp_wrongID)

#duplicated
tmp_duplicated <- subset(FKBP5_wywiad_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))

FKBP5_wywiad_data$ID_participant[FKBP5_wywiad_data$Participant_first_last_name == "Agata Pilawska"] <- "WRO106"
FKBP5_wywiad_data$ID[FKBP5_wywiad_data$Participant_first_last_name == "Agata Pilawska"] <- "WRO106"


FKBP5_wywiad_data <- FKBP5_wywiad_data %>%
  filter(!(ID == "WWA53" & Date_of_study == "23/10/2023"))
FKBP5_wywiad_data <- FKBP5_wywiad_data %>%
  filter(!(ID == "PUM03K" & Participant_first_last_name == "Radosław Wróblewski"))
FKBP5_wywiad_data <- FKBP5_wywiad_data %>%
  filter(!(ID == "WWA57" & Date_of_study == "14.02.2024"))
FKBP5_wywiad_data <- FKBP5_wywiad_data %>%
  filter(!(ID == "PUM00B"))


#sprawdzenie
tmp_mismatched <- subset(FKBP5_wywiad_data, ID != ID_participant)
tmp_duplicated <- subset(FKBP5_wywiad_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_wrongID <- FKBP5_wywiad_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_wywiad_data$ID), ]


###################
#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("data_main/clear_FKBP5_wywiad_data_", current_time, ".sav")
# Zapis danych
write_sav(FKBP5_wywiad_data, filename)




# ================================================
# FKBP5_BASELINE_DATA
# ================================================

FKBP5_baseline_data <- read_sav("data/data_cleaning/FKBP5+-+baseline_July+12,+2024_16.04.sav")


#zamiana nazw kolumn
tmp_progress <- select(FKBP5_baseline_data, Progress, Finished, StartDate, EndDate)
FKBP5_baseline_col_names <- read_sav("data//data_cleaning/clear_FKBP5_baseline_data.sav")
FKBP5_baseline_col_names <- names(FKBP5_baseline_col_names)
columns_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration__in_seconds_", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")
columns_to_keep <- !(names(FKBP5_baseline_data) %in% columns_to_remove)
FKBP5_baseline_data <- subset(FKBP5_baseline_data, select = columns_to_keep)
names(FKBP5_baseline_data) <- FKBP5_baseline_col_names
FKBP5_baseline_data <- cbind(tmp_progress, FKBP5_baseline_data)

rm(tmp_progress)

FKBP5_baseline_data$ID <- toupper(FKBP5_baseline_data$ID)
FKBP5_baseline_data$ID <- gsub(" ", "", FKBP5_baseline_data$ID) #usunięcie spacji
FKBP5_baseline_data <- arrange(FKBP5_baseline_data, ID) #sortowanie po ID

tmp_duplicated <- subset(FKBP5_baseline_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_duplicated <- arrange(tmp_duplicated, ID, StartDate)
tmp_wrongID <- FKBP5_baseline_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_baseline_data$ID), ]


#czyszczenie
#wrongID
FKBP5_baseline_data$ID <- gsub("PUMK24", "PUM24K", FKBP5_baseline_data$ID)
FKBP5_baseline_data$ID <- gsub("WAWA05", "WAW05", FKBP5_baseline_data$ID)
tmp_wrongID <- FKBP5_baseline_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_baseline_data$ID), ]
FKBP5_baseline_data <- anti_join(FKBP5_baseline_data, tmp_wrongID)

#duplicated
tmp_duplicated <- subset(FKBP5_baseline_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_duplicated <- arrange(tmp_duplicated, ID, StartDate)

#Wybranie najmłodszych tych co mają oba duplikaty progress=100 
tmp_duplicated_to_remove <- tmp_duplicated %>%
  mutate(StartDate = as.POSIXct(StartDate)) %>%
  group_by(ID) %>%
  filter(Progress == 100) %>%
  slice_max(order_by = StartDate, n = 1, with_ties = FALSE) %>%
  ungroup()

#Usunięcie z tmp_duplicated 
tmp_duplicated_to_keep <- anti_join(tmp_duplicated, tmp_duplicated_to_remove)

#Wybranie tych co mają odpowiedni wymagany progress i wybranie potem najmłodszych
tmp_duplicated_to_keep_2 <- tmp_duplicated_to_keep %>%
  filter(Progress > 10) %>%
  mutate(StartDate = as.POSIXct(StartDate)) %>%
  group_by(ID) %>%
  summarise(StartDate = min(StartDate, na.rm = TRUE))
tmp_duplicated_to_keep_2 <- left_join(tmp_duplicated_to_keep_2, tmp_duplicated_to_keep)

tmp_duplicated_to_remove_2 <- anti_join(tmp_duplicated, tmp_duplicated_to_keep_2)


FKBP5_baseline_data <- anti_join(FKBP5_baseline_data, tmp_duplicated_to_remove_2)


#sprawdzenie
tmp_duplicated <- subset(FKBP5_baseline_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_duplicated <- arrange(tmp_duplicated, ID, StartDate)
tmp_wrongID <- FKBP5_baseline_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_baseline_data$ID), ]


###################
#ZAPISYWANIE
# Generowanie aktualnej daty i czasu w odpowiednim formacie
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Tworzenie nazwy pliku z aktualną datą i czasem
filename <- paste0("data_main/clear_FKBP5_baseline_data_", current_time, ".sav")
# Zapis danych
write_sav(FKBP5_baseline_data, filename)
