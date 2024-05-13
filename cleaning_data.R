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

#version

# Wczytanie danych z pliku .sav
esm_cortisol_data_WWA <- read_excel("data_cleaning/25_04 esm-cortisol-WWA.xlsx")
esm_cortisol_data_WRO <- read_excel("data_cleaning/25_04 esm-cortisol-WRO.xlsx")
esm_cortisol_data_SZCZ <- read_excel("data_cleaning/25_04 esm-cortisol-SZCZ.xlsx")

##############
#esm_cortisol_data
# Pobranie nazw kolumn dla każdej ramki danych
col_names1 <- colnames(esm_cortisol_data_WWA)
col_names2 <- colnames(esm_cortisol_data_WRO)
col_names3 <- colnames(esm_cortisol_data_SZCZ)

# Znalezienie różnic między kolumnami
diff_1_2 <- setdiff(col_names1, col_names2)
diff_2_1 <- setdiff(col_names2, col_names1)

diff_1_3 <- setdiff(col_names1, col_names3)
diff_3_1 <- setdiff(col_names3, col_names1)

diff_2_3 <- setdiff(col_names2, col_names3)
diff_3_2 <- setdiff(col_names3, col_names2)

# Wyświetlenie różnic
print("Różnice między esm_cortisol_data1 a esm_cortisol_data2:")
print(diff_1_2)
print(diff_2_1)

#zamiana nazw kolumn
esm_cortisol_data_WWA <- esm_cortisol_data_WWA %>%
  rename(Bad_interval_over_20_min_or_missing = `Bad interval >20 min or missing`)


print("Różnice między esm_cortisol_data1 a esm_cortisol_data3:")
print(diff_1_3)
print(diff_3_1)

print("Różnice między esm_cortisol_data2 a esm_cortisol_data3:")
print(diff_2_3)
print(diff_3_2)


rm(diff_1_2)
rm(diff_1_3)
rm(diff_2_1)
rm(diff_2_3)
rm(diff_3_1)
rm(diff_3_2)

rm(col_names1)
rm(col_names2)
rm(col_names3)


#dont_feel_safe_item_a23
esm_cortisol_data_WWA$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_WWA$dont_feel_safe_item_a23)
esm_cortisol_data_WRO$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_WRO$dont_feel_safe_item_a23)
esm_cortisol_data_SZCZ$dont_feel_safe_item_a23 <- as.character(esm_cortisol_data_SZCZ$dont_feel_safe_item_a23)


# Scalenie trzech baz danych w jedną
esm_cortisol_data <- bind_rows(esm_cortisol_data_WWA, esm_cortisol_data_WRO, esm_cortisol_data_SZCZ)


tmp_wrongID <- esm_cortisol_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", esm_cortisol_data$Participant), ]




#Trigger_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Trigger_date = as.Date(Trigger_date))
#Trigger_time
esm_cortisol_data$Trigger_time <- as_hms(esm_cortisol_data$Trigger_time)


#Form_start_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_start_date = as.Date(Form_start_date))
#Form_start_time
esm_cortisol_data$Form_start_time <- as_hms(esm_cortisol_data$Form_start_time)


#Form_finish_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_finish_date = as.Date(Form_finish_date))
#Form_finish_time
esm_cortisol_data$Form_finish_time <- as_hms(esm_cortisol_data$Form_finish_time)

#Form_upload_date
esm_cortisol_data <- esm_cortisol_data %>%
  mutate(Form_upload_date = as.Date(Form_upload_date))
#Form_upload_time
esm_cortisol_data$Form_upload_time <- as_hms(esm_cortisol_data$Form_upload_time)



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


#duplikaty
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


#ZAPISYWANIE
write_sav(esm_cortisol_data, "out_cleaning/clear_esm_cortisol_data.sav")









#SCREENING_DATA
screening_data <- read_sav("data_cleaning/ALL_screening.sav")


screening_data$ID <- toupper(screening_data$ID)
screening_data$ID <- gsub(" ", "", screening_data$ID) #usunięcie spacji
screening_data <- arrange(screening_data, ID) #sortowanie po ID


#sprawdzenie
tmp_duplicated <- subset(screening_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_wrongID <- screening_data[!grepl("^(WWA[0-9]|PUM[0-9]|WRO[0-9])", screening_data$ID), ]


#ZAPISYWANIE
write_sav(screening_data, "out_cleaning/clear_screening_data.sav")








# FKBP5_WYWIAD_DATA
FKBP5_wywiad_data <- read_sav("data_cleaning/FKBP5+-+wywiad_May+2,+2024_10.57.sav")


#zamiana nazw kolumn
tmp_progress <- select(FKBP5_wywiad_data, Progress, Finished, StartDate, EndDate)
FKBP5_wywiad_col_names <- read_sav("data_cleaning/clear_FKBP5_wywiad_data.sav")
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
FKBP5_wywiad_data$ID[FKBP5_wywiad_data$ID == "PUM12G"] <- "PUM12B"
FKBP5_wywiad_data$ID_participant[FKBP5_wywiad_data$ID_participant == "PUM12G"] <- "PUM12B"

tmp_wrongID <- FKBP5_wywiad_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_wywiad_data$ID), ]
FKBP5_wywiad_data <- anti_join(FKBP5_wywiad_data, tmp_wrongID)

FKBP5_wywiad_data$ID <- ifelse(FKBP5_wywiad_data$ID_participant == "WWA121", "WWA121", FKBP5_wywiad_data$ID)

tmp_duplicated <- subset(FKBP5_wywiad_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))

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


#ZAPISYWANIE
write_sav(FKBP5_wywiad_data, "out_cleaning/clear_FKBP5_wywiad_data.sav")






#FKBP5_BASELINE_DATA
FKBP5_baseline_data <- read_sav("data_cleaning/FKBP5+-+baseline_May+2,+2024_10.56.sav")


#zamiana nazw kolumn
tmp_progress <- select(FKBP5_baseline_data, Progress, Finished, StartDate, EndDate)
FKBP5_baseline_col_names <- read_sav("data_cleaning/clear_FKBP5_baseline_data.sav")
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
FKBP5_baseline_data$ID <- gsub("PUMK24", "PUM24K", FKBP5_baseline_data$ID)
FKBP5_baseline_data$ID <- gsub("WAWA05", "WAW05", FKBP5_baseline_data$ID)
tmp_wrongID <- FKBP5_baseline_data[!grepl("^(WWA[0-9]|PUM[0-9][0-9]K|PUM[0-9][0-9]B|WRO[0-9])", FKBP5_baseline_data$ID), ]
FKBP5_baseline_data <- anti_join(FKBP5_baseline_data, tmp_wrongID)


tmp_duplicated <- subset(FKBP5_baseline_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_duplicated <- arrange(tmp_duplicated, ID, StartDate)

tmp_duplicated_to_remove <- tmp_duplicated %>%
  mutate(StartDate = as.POSIXct(StartDate)) %>%
  group_by(ID) %>%
  filter(Progress == 100) %>%
  summarise(StartDate = max(StartDate, na.rm = TRUE))

tmp_duplicated_to_keep <- anti_join(tmp_duplicated, tmp_duplicated_to_remove)

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


#ZAPISYWANIE
write_sav(FKBP5_baseline_data, "out_cleaning/clear_FKBP5_baseline_data.sav")