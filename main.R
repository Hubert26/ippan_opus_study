library(haven)
#install.packages("remotes")
#remotes::install_github("wviechtb/esmpack")
library(esmpack)
#install.packages("vtable")
library(vtable)
#install.packages("mediation")
library(mediation)
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


#Aggregacja danych
esm_cortisol_data_aggregated <- aggregate(esm_cortisol_data[, c("f1_PLEs_sum", "Cortisol_ng_ml")], by = list(esm_cortisol_data$Participant), FUN = mean, na.rm = TRUE)
names(esm_cortisol_data_aggregated)[1] <- "ID"
esm_cortisol_data_aggregated <- merge(esm_cortisol_data_aggregated, WWA_all_screening_data[, c("ID", "Group", "ASRS_suma")], by = "ID", all = TRUE)
esm_cortisol_data_aggregated <- na.omit(esm_cortisol_data_aggregated)

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
