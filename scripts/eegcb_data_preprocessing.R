########### Main model ################

### PREPEARING WORKING SPACE
install.packages("ggplot2")
install.packages("writexl")
install.packages("dplyr") 

library(lme4)
library(performance)
library(ordinal)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(writexl)
library(nlme)
library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1

### DATA LOADING AND PREPROCESSING
CB <- EEGCB_ALL
colnames(CB)

### Attitude factor combines dependent from the issue of the topic
CB = CB |> filter(ID != 'EEGCB12012') ## убираем 12, т.к. для него еще не посчитан IAT

## создаем колонку со значениями IAT
CB = CB |> mutate(
  IAT = case_when(
    ID == "EEGCB12011" ~ 0.634710609,
    ID == "EEGCB11010" ~ 0.2257430605,
    ID == "EEGCB12009" ~ -0.2742406897,
    ID == "EEGCB11008" ~ 0.1206581881,
    ID == "EEGCB12007" ~ -0.6711003127,
    ID == "EEGCB11006" ~ 0.2842380924))

## создаем колонку со значением EXP_general

CB = CB |> mutate(
  EXP_general = case_when(
    ID == "EEGCB12011" ~ 2.166666667,
    ID == "EEGCB11010" ~ 2.75,
    ID == "EEGCB12009" ~ 1.916666667,
    ID == "EEGCB11008" ~ 2,
    ID == "EEGCB12007" ~ 4.083333333,
    ID == "EEGCB11006" ~ 2.916666667))

## создаем колонку со значением EXP_consp

CB = CB |> mutate(
  EXP_consp = case_when(
    ID == "EEGCB12011" ~ 1.142857143,
    ID == "EEGCB11010" ~ 1.857142857,
    ID == "EEGCB12009" ~ 1,
    ID == "EEGCB11008" ~ 2.571428571,
    ID == "EEGCB12007" ~ 2.714285714,
    ID == "EEGCB11006" ~ 1))

## создаем колонку со значением EXP_covid

CB = CB |> mutate(
  EXP_covid = case_when(
    ID == "EEGCB12011" ~ 2.205882353,
    ID == "EEGCB11010" ~ 3.529411765,
    ID == "EEGCB12009" ~ 1.5,
    ID == "EEGCB11008" ~ 2.764705882,
    ID == "EEGCB12007" ~ 4.764705882,
    ID == "EEGCB11006" ~ 2.647058824))

## создаем колонку со значением CB_IAT2
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное.

CB = CB |> mutate(
  CB_IAT2 = case_when(Valence == 1 & IAT < 0 ~ "match",
                      Valence == 1 & IAT > 0 ~ "mismatch",
                      Valence == 2 & IAT > 0 ~ "match",
                      Valence == 2 & IAT < 0 ~ "mismatch",
                      Valence == 0 ~ "neutral"))

## создаем колонку со значением CB_IAT4
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное.
                     
CB = CB |> mutate(
  CB_IAT4 = case_when(Valence == 1 & IAT < 0 ~ "match_negative",
                      Valence == 1 & IAT > 0 ~ "mismatch_positive",
                      Valence == 2 & IAT > 0 ~ "match_positive",
                      Valence == 2 & IAT < 0 ~ "mismatch_negative",
                      Valence == 0 ~ "neutral"))

## создаем колонку со значением CB_EXP2 - создается по EXP_general
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное.

#CB = CB |> mutate(
#  CB_EXP2 = case_when(Valence == 1 & EXP_general < 0 ~ "match_negative",
#                      Valence == 1 & EXP_general > 0 ~ "mismatch_positive",
#                      Valence == 2 & EXP_general > 0 ~ "match_positive",
#                      Valence == 2 & EXP_general < 0 ~ "mismatch_negative",
#                      Valence == 0 ~ "neutral"))

## создаем колонку со значением CB_EXP4 - создается по EXP_general
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное.

#CB = CB |> mutate(
#  CB_EXP4 = case_when(Valence == 1 & EXP_general < 0 ~ "match_negative",
#                      Valence == 1 & EXP_general > 0 ~ "mismatch_positive",
#                      Valence == 2 & EXP_general > 0 ~ "match_positive",
#                      Valence == 2 & EXP_general < 0 ~ "mismatch_negative",
#                      Valence == 0 ~ "neutral"))

### DATA SAVING
write_xlsx(CB, "C:/Users/cybergnom/Documents/eegcb/data/CB.xls")

