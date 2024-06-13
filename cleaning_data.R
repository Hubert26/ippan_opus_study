library(haven)
#install.packages("remotes")
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

#SPRAWDZENIE ILOŚĆ NAN W KOLUMNACH
# Liczenie ilości wartości NA w każdej kolumnie
na_counts <- colSums(is.na(esm_cortisol_data))
# Konwersja wyniku do dataframe
na_counts_df <- as.data.frame(t(na_counts))
# Ustawienie nazw kolumn
colnames(na_counts_df) <- colnames(esm_cortisol_data)

#BAZA

baza <- data.frame(
  id = esm_cortisol_data$Participant,
  obs = esm_cortisol_data$Trigger_counter,
  beeptime = as.numeric(hour(esm_cortisol_data$Trigger_time) * 60 + minute(esm_cortisol_data$Trigger_time)),
  resptime = as.numeric(hour(esm_cortisol_data$Form_start_time) * 60 + minute(esm_cortisol_data$Form_start_time)),
  resphour = as.numeric(hour(esm_cortisol_data$Form_start_time) + minute(esm_cortisol_data$Form_start_time) / 60),
  finishtime = as.numeric(hour(esm_cortisol_data$Form_finish_time) * 60 + minute(esm_cortisol_data$Form_finish_time)),
  cortisoltime = as.numeric(hour(esm_cortisol_data$Cortisol_time) * 60 + minute(esm_cortisol_data$Cortisol_time)),
  cortlevel = esm_cortisol_data$Cortisol_ng_ml * 100,
  sleeplength = esm_cortisol_data$sleep_hours_item_1972,
  sleep_quality = esm_cortisol_data$sleep_evaluation_item_a2,
  Form = esm_cortisol_data$Form
) %>%
  mutate(
    eventstress = ifelse(Form == "PORANEK", esm_cortisol_data$Importatnt_event_item_a3, esm_cortisol_data$Importatnt_event_item_1),
    caffsmok = ifelse(Form == "PORANEK", esm_cortisol_data$caffeine_tobacco_item_2109, esm_cortisol_data$caffeine_tobacco_item_1858),
    eatdrink = ifelse(Form == "PORANEK", esm_cortisol_data$food_soft_drink_item_2133, esm_cortisol_data$food_soft_drink_item_1901),
    alcohol = ifelse(Form == "PORANEK", esm_cortisol_data$alcohol_item_2157, esm_cortisol_data$alcohol_item_1931),
    body = ifelse(Form == "PORANEK", esm_cortisol_data$body_image_item_a7, esm_cortisol_data$body_image_item_5),
    rumination = ifelse(Form == "PORANEK", esm_cortisol_data$ruminations_item_2041, esm_cortisol_data$ruminations_item_1832),
    actstress1 = ifelse(Form == "PORANEK", esm_cortisol_data$rather_do_something_else_a8, esm_cortisol_data$rather_do_something_else_item_6),
    actstress2 = ifelse(Form == "PORANEK", esm_cortisol_data$difficult_event_item_a9, esm_cortisol_data$difficult_event_item_7),
    actstress2 = ifelse(Form == "PORANEK", esm_cortisol_data$nice_event_item_a10, esm_cortisol_data$nice_event_item_8),
    socstress1a = ifelse(Form == "PORANEK", esm_cortisol_data$being_alone_item_a11_1, esm_cortisol_data$being_alone_item_9_1),
    socstress1b = ifelse(Form == "PORANEK", esm_cortisol_data$being_with_family_item_a11_2, esm_cortisol_data$being_with_family_item_9_2),
    socstress1c = ifelse(Form == "PORANEK", esm_cortisol_data$being_with_partner_item_a11_3, esm_cortisol_data$being_with_partner_item_9_3),
    socstress1d = ifelse(Form == "PORANEK", esm_cortisol_data$being_with_friends_item_a11_4, esm_cortisol_data$being_with_friends_item_9_4),
    socstress1e = ifelse(Form == "PORANEK", esm_cortisol_data$being_with_strangers_item_a11_5, esm_cortisol_data$being_with_strangers_item_9_5),
    socstress1f = ifelse(Form == "PORANEK", esm_cortisol_data$being_with_co_workers_item_a11_6, esm_cortisol_data$being_with_co_workers_item_9_6),
    socstress1 = ifelse(Form == "PORANEK", esm_cortisol_data$rather_be_alone_item_a12, esm_cortisol_data$rather_be_alone_item_10),
    socstress2 = ifelse(Form == "PORANEK", esm_cortisol_data$enjoyable_being_with_people_item_a13, esm_cortisol_data$enjoyable_being_with_people_item_11),
    outsider = ifelse(Form == "PORANEK", esm_cortisol_data$odludek_item_a14, esm_cortisol_data$odludek_item_12),
    helpless1 = ifelse(Form == "PORANEK", esm_cortisol_data$unsuspected_event_item_a15, esm_cortisol_data$unsuspected_event_item_13),
    helpless2 = ifelse(Form == "PORANEK", esm_cortisol_data$loss_of_control_item_a16, esm_cortisol_data$loss_of_control_item_14),
    selfeff1 = ifelse(Form == "PORANEK", esm_cortisol_data$coping_problems_item_a17, esm_cortisol_data$problems_coping_item_15),
    selfeff2 = ifelse(Form == "PORANEK", esm_cortisol_data$things_going_my_way_item_a18, esm_cortisol_data$things_going_my_way_item_16),
    areastress = ifelse(Form == "PORANEK", esm_cortisol_data$unpleasant_to_be_here_item_a19, esm_cortisol_data$unpleasant_to_be_here_item_17),
    anxious = ifelse(Form == "PORANEK", esm_cortisol_data$feel_anxious_item_a20, esm_cortisol_data$feel_anxious_item_18),
    down = ifelse(Form == "PORANEK", esm_cortisol_data$feel_depressed_item_a21, esm_cortisol_data$feel_depressed_item_19),
    lonely = ifelse(Form == "PORANEK", esm_cortisol_data$feel_alone_item_a22, esm_cortisol_data$feel_alone_item_20),
    insecure = ifelse(Form == "PORANEK", esm_cortisol_data$dont_feel_safe_item_a23, esm_cortisol_data$dont_feel_safe_item_21),
    annoyed = ifelse(Form == "PORANEK", esm_cortisol_data$feel_irritated_a24, esm_cortisol_data$feel_irritated_item_22),
    as = ifelse(Form == "PORANEK", esm_cortisol_data$attention_attracts_item_a25, esm_cortisol_data$attention_attracts_item_23),
    as2 = ifelse(Form == "PORANEK", esm_cortisol_data$meaning_item_a26, esm_cortisol_data$meaning_item_24),
    as3 = ifelse(Form == "PORANEK", esm_cortisol_data$noticing_item_a27, esm_cortisol_data$noticing_item_25),
    ta1 = ifelse(Form == "PORANEK", esm_cortisol_data$unpleasant_anticipation_item_a28, esm_cortisol_data$unpleasant_anticipation_item_26),
    ta2 = ifelse(Form == "PORANEK", esm_cortisol_data$carefulness_item_a29, esm_cortisol_data$carefulness_item_27),
    ta3 = ifelse(Form == "PORANEK", esm_cortisol_data$details_item_a30, esm_cortisol_data$details_item_28),
    h1 = ifelse(Form == "PORANEK", esm_cortisol_data$hearing_thoughts_item_a31, esm_cortisol_data$hearing_thoughts_item_29),
    h2 = ifelse(Form == "PORANEK", esm_cortisol_data$voices_item_a32, esm_cortisol_data$voices_item_30),
    h3 = ifelse(Form == "PORANEK", esm_cortisol_data$hallucinations_item_a33, esm_cortisol_data$hallucinations_item_31),
    d1 = ifelse(Form == "PORANEK", esm_cortisol_data$invisible_force_item_a34, esm_cortisol_data$invisible_force_item_32),
    d2 = ifelse(Form == "PORANEK", esm_cortisol_data$hidden_meaning_item_a35, esm_cortisol_data$hidden_meaning_item_33),
    d3 = ifelse(Form == "PORANEK", esm_cortisol_data$unreal_experience_item_a36, esm_cortisol_data$unreal_experience_item_34),
    d4 = ifelse(Form == "PORANEK", esm_cortisol_data$thought_control_item_a37, esm_cortisol_data$thought_control_item_35),
    d5 = ifelse(Form == "PORANEK", esm_cortisol_data$get_rid_of_thoughts_item_38, esm_cortisol_data$get_rid_of_thoughts_item_36),
  )

#SPRAWDZENIE ILOŚĆ NAN W KOLUMNACH
# Liczenie ilości wartości NA w każdej kolumnie
na_counts <- colSums(is.na(baza))
# Konwersja wyniku do dataframe
na_counts_baza <- as.data.frame(t(na_counts))
# Ustawienie nazw kolumn
colnames(na_counts_baza) <- colnames(baza)

# Dodanie drugiego wiersza z odsetkiem wartości NA
na_percentage <- na_counts / nrow(baza)*100
na_counts_baza <- rbind(na_counts_baza, as.data.frame(t(na_percentage)))

# Ustawienie nazw wierszy
rownames(na_counts_baza) <- c("NaN_Counts", "NaN_Percentage")

na_counts_baza <- na_counts_baza %>%
  select(as, as2, as3, ta1, ta2, ta3, h1, h2, h3, d1, d2, d3, d4, d5, rumination, anxious, down, lonely, insecure, annoyed)

# Konwersja wiersza "NaN_Percentage" do typu numeric
na_counts_baza["NaN_Percentage", ] <- as.numeric(na_counts_baza["NaN_Percentage", ])

# Obliczenie średniej z wiersza NaN_Percentage
mean_nan_percentage <- mean(as.numeric(na_counts_baza["NaN_Percentage", ]), na.rm = TRUE)

# Wyświetlenie średniej
print(mean_nan_percentage)

#write_sav(na_counts_baza, "out_cleaning/na_counts_baza.sav")
#write_sav(baza, "out_cleaning/baza_alpha.sav")

#ZAPISYWANIE
#write_sav(esm_cortisol_data, "out_cleaning/clear_esm_cortisol_data.sav")









#SCREENING_DATA
screening_data <- read_sav("data_cleaning/ALL_screening.sav")


screening_data$ID <- toupper(screening_data$ID)
screening_data$ID <- gsub(" ", "", screening_data$ID) #usunięcie spacji
screening_data <- arrange(screening_data, ID) #sortowanie po ID


#sprawdzenie
tmp_duplicated <- subset(screening_data, duplicated(ID) | duplicated(ID, fromLast = TRUE))
tmp_wrongID <- screening_data[!grepl("^(WWA[0-9]|PUM[0-9]|WRO[0-9])", screening_data$ID), ]


#ZAPISYWANIE
#write_sav(screening_data, "out_cleaning/clear_screening_data.sav")








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
#write_sav(FKBP5_wywiad_data, "out_cleaning/clear_FKBP5_wywiad_data.sav")

#M, SD
#Unusual_thinking
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_Unusual_thinking_contents_[0-9]", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$utc <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, utc), by = c("id" = "ID"), relationship = "many-to-many")

#speech_disorganization
selected_columns <- names(FKBP5_wywiad_data)[grep("^CAARMS_speech_disorganization_[0-9]_subj", names(FKBP5_wywiad_data))]
print(selected_columns)
FKBP5_wywiad_data$sdis <- rowSums(FKBP5_wywiad_data[selected_columns] == 1)
baza <- baza %>%
  left_join(select(FKBP5_wywiad_data, ID, sdis), by = c("id" = "ID"), relationship = "many-to-many")

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


# Obliczanie średnich i odchyleń standardowych
mean_sd <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
}

# Stworzenie dataframe ze średnimi i odchyleniami standardowymi
results <- data.frame(
  Variable = c("utc", "sdis", "otc", "hal"),
  Mean = c(mean_sd(baza$utc)["mean"], 
           mean_sd(baza$sdis)["mean"], 
           mean_sd(baza$otc)["mean"], 
           mean_sd(baza$hal)["mean"]),
  SD = c(mean_sd(baza$utc)["sd"], 
         mean_sd(baza$sdis)["sd"], 
         mean_sd(baza$otc)["sd"], 
         mean_sd(baza$hal)["sd"])
)

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
#write_sav(FKBP5_baseline_data, "out_cleaning/clear_FKBP5_baseline_data.sav")