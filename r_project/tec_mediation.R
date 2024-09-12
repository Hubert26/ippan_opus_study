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

mediation_data <- read_sav("tec_mediation_data/input/mediation_data.sav")

selected_columns <- grep("^(TEC_[0-9][0-9]|TEC_[0-9])", names(mediation_data), value = TRUE)
mediation_data <- mediation_data %>%
  mutate_at(vars(selected_columns), ~as.integer(replace_na(., 0)))


selected_columns <- grep("^(TEC_[0-9][0-9]_age_u12|TEC_[0-9]_age_u12)", names(mediation_data), value = TRUE)
mediation_data <- mediation_data %>%
  mutate_at(vars(selected_columns), ~as.integer(replace_na(., 0)))

mediation_data <- mediation_data %>%
  mutate(TEC_et = as.numeric(TEC_14) + as.numeric(TEC_15) + as.numeric(TEC_16) + as.numeric(TEC_17) + as.numeric(TEC_18) + as.numeric(TEC_19))

mediation_data <- mediation_data %>%
  mutate(TEC_et_u12 = if_else(TEC_14_age_u12 == 1, as.numeric(TEC_14), 0) +
           if_else(TEC_15_age_u12 == 1, as.numeric(TEC_15), 0) +
           if_else(TEC_16_age_u12 == 1, as.numeric(TEC_16), 0) +
           if_else(TEC_17_age_u12 == 1, as.numeric(TEC_17), 0) +
           if_else(TEC_18_age_u12 == 1, as.numeric(TEC_18), 0) +
           if_else(TEC_19_age_u12 == 1, as.numeric(TEC_19), 0))


mediation_data <- mediation_data %>%
  mutate(TEC_st = as.numeric(TEC_24) + as.numeric(TEC_25) + as.numeric(TEC_26) + as.numeric(TEC_27) + as.numeric(TEC_28) + as.numeric(TEC_29))

mediation_data <- mediation_data %>%
  mutate(TEC_st_u12 = ifelse(TEC_24_age_u12 == 1, as.numeric(replace_na(TEC_24, 0)), 0) +
           ifelse(TEC_25_age_u12 == 1, as.numeric(replace_na(TEC_25, 0)), 0) +
           ifelse(TEC_26_age_u12 == 1, as.numeric(replace_na(TEC_26, 0)), 0) +
           ifelse(TEC_27_age_u12 == 1, as.numeric(replace_na(TEC_27, 0)), 0) +
           ifelse(TEC_28_age_u12 == 1, as.numeric(replace_na(TEC_28, 0)), 0) +
           ifelse(TEC_29_age_u12 == 1, as.numeric(replace_na(TEC_29, 0)), 0))


mediation_data <- mediation_data %>%
  mutate(TEC_bt = as.numeric(TEC_20) + as.numeric(TEC_21) + as.numeric(TEC_22) + as.numeric(TEC_23) + as.numeric(TEC_9) + as.numeric(TEC_10))

mediation_data <- mediation_data %>%
  mutate(TEC_bt_u12 = ifelse(TEC_20_age_u12 == 1, as.numeric(replace_na(TEC_20, 0)), 0) +
           ifelse(TEC_21_age_u12 == 1, as.numeric(replace_na(TEC_21, 0)), 0) +
           ifelse(TEC_22_age_u12 == 1, as.numeric(replace_na(TEC_22, 0)), 0) +
           ifelse(TEC_23_age_u12 == 1, as.numeric(replace_na(TEC_23, 0)), 0) +
           ifelse(TEC_9_age_u12 == 1, as.numeric(replace_na(TEC_9, 0)), 0) +
           ifelse(TEC_10_age_u12 == 1, as.numeric(replace_na(TEC_10, 0)), 0))

tmp <- select(mediation_data, TEC_st, TEC_st_u12, TEC_24_age_u12, TEC_24, TEC_25_age_u12, TEC_25, TEC_26_age_u12, TEC_26, TEC_27_age_u12, TEC_27, TEC_28_age_u12, TEC_28, TEC_29_age_u12, TEC_29)




write_sav(mediation_data, "tec_mediation_data/out/mediation_data.sav")