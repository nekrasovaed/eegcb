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

## создаем колонку со значениями IAT
CB = CB |> mutate(
  IAT = case_when(
    ID == "EEGCB12018" ~ -0.1005694483,
    ID == "EEGCB12017" ~ 0.2182121651,
    ID == "EEGCB11016" ~ -0.1905619843,
    ID == "EEGCB12015" ~ 0.06957209508,
    ID == "EEGCB11014" ~ 0.7457023126,
    ID == "EEGCB11013" ~ -0.05498800629,
    ID == "EEGCB12012" ~ 0.2949198283,
    ID == "EEGCB12011" ~ 0.634710609,
    ID == "EEGCB11010" ~ 0.2257430605,
    ID == "EEGCB12009" ~ -0.2742406897,
    ID == "EEGCB11008" ~ 0.1206581881,
    ID == "EEGCB12007" ~ -0.6711003127,
    ID == "EEGCB11006" ~ 0.2842380924,
    ID == "EEGCB11005" ~ 0.2842380924,
    ID == "EEGCB12003" ~ 0.5549722965,
    ID == "EEGCB12002" ~ -0.0452171823))

## создаем колонку со значением EXP_general

CB = CB |> mutate(
  EXP_general = case_when(
    ID == "EEGCB12018" ~ 4.25,
    ID == "EEGCB12017" ~ 1.75,
    ID == "EEGCB11016" ~ 2.25,
    ID == "EEGCB12015" ~ 1.333333333,
    ID == "EEGCB11014" ~ 1.583333333,
    ID == "EEGCB11013" ~ 2.75,
    ID == "EEGCB12012" ~ 1.416666667,
    ID == "EEGCB12011" ~ 2.166666667,
    ID == "EEGCB11010" ~ 2.75,
    ID == "EEGCB12009" ~ 1.916666667,
    ID == "EEGCB11008" ~ 2,
    ID == "EEGCB12007" ~ 4.083333333,
    ID == "EEGCB11006" ~ 2.916666667,
    ID == "EEGCB11005" ~ 0.2842380924,
    ID == "EEGCB12003" ~ 2.333333333,
    ID == "EEGCB12002" ~ 1.833333333))

## создаем колонку со значением EXP_consp

CB = CB |> mutate(
  EXP_consp = case_when(
    ID == "EEGCB12018" ~ 4.571428571,
    ID == "EEGCB12017" ~ 1.285714286,
    ID == "EEGCB11016" ~ 1.714285714,
    ID == "EEGCB12015" ~ 1,
    ID == "EEGCB11014" ~ 1.285714286,
    ID == "EEGCB11013" ~ 1.714285714,
    ID == "EEGCB12012" ~ 1.142857143,
    ID == "EEGCB12011" ~ 1.142857143,
    ID == "EEGCB11010" ~ 1.857142857,
    ID == "EEGCB12009" ~ 1,
    ID == "EEGCB11008" ~ 2.571428571,
    ID == "EEGCB12007" ~ 2.714285714,
    ID == "EEGCB11006" ~ 1,
    ID == "EEGCB11005" ~ 0.2842380924,
    ID == "EEGCB12003" ~ 1.285714286,
    ID == "EEGCB12002" ~ 1.285714286))

## создаем колонку со значением EXP_covid

CB = CB |> mutate(
  EXP_covid = case_when(
    ID == "EEGCB12018" ~ 5.294117647,
    ID == "EEGCB12017" ~ 1.941176471,
    ID == "EEGCB11016" ~ 2.441176471,
    ID == "EEGCB12015" ~ 2.205882353,
    ID == "EEGCB11014" ~ 2.176470588,
    ID == "EEGCB11013" ~ 2.147058824,
    ID == "EEGCB12012" ~ 1.176470588,
    ID == "EEGCB12011" ~ 2.205882353,
    ID == "EEGCB11010" ~ 3.529411765,
    ID == "EEGCB12009" ~ 1.5,
    ID == "EEGCB11008" ~ 2.764705882,
    ID == "EEGCB12007" ~ 4.764705882,
    ID == "EEGCB11006" ~ 2.647058824,
    ID == "EEGCB11005" ~ 4.352941176,
    ID == "EEGCB12003" ~ 2.235294118,
    ID == "EEGCB12002" ~ 2.470588235))

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
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное. <3 = позитивное, > 3 = негативное


CB = CB |> mutate(
  CB_EXP_covid2 = case_when(Valence == 1 & EXP_covid < 3.5 ~ "mismatch",
                      Valence == 1 & EXP_covid > 3.5 ~ "match",
                      Valence == 2 & EXP_covid > 3.5 ~ "mismatch",
                      Valence == 2 & EXP_covid < 3.5 ~ "match",
                      Valence == 0 ~ "neutral"))

CB$CB_EXP_covid2

CB = CB |> mutate(
  CB_EXP_covid4 = case_when(Valence == 1 & EXP_covid < 3.5 ~ "mismatch_negative",
                      Valence == 1 & EXP_covid > 3.5 ~ "match_negative",
                      Valence == 2 & EXP_covid > 3.5 ~ "mismatch_positive",
                      Valence == 2 & EXP_covid < 3.5 ~ "match_positive",
                      Valence == 0 ~ "neutral"))


CB = CB |> mutate(
  CB_EXP_general2 = case_when(Valence == 1 & EXP_general < 3.5 ~ "mismatch",
                            Valence == 1 & EXP_general > 3.5 ~ "match",
                            Valence == 2 & EXP_general > 3.5 ~ "mismatch",
                            Valence == 2 & EXP_general < 3.5 ~ "match",
                            Valence == 0 ~ "neutral"))

CB$CB_EXP_covid2

CB = CB |> mutate(
  CB_EXP_general4 = case_when(Valence == 1 & EXP_general < 3.5 ~ "mismatch_negative",
                            Valence == 1 & EXP_general > 3.5 ~ "match_negative",
                            Valence == 2 & EXP_general > 3.5 ~ "mismatch_positive",
                            Valence == 2 & EXP_general < 3.5 ~ "match_positive",
                            Valence == 0 ~ "neutral"))


CB = CB |> mutate(
  CB_EXP_consp2 = case_when(Valence == 1 & EXP_consp < 3 ~ "mismatch",
                              Valence == 1 & EXP_consp > 3 ~ "match",
                              Valence == 2 & EXP_consp > 3 ~ "mismatch",
                              Valence == 2 & EXP_consp < 3 ~ "match",
                              Valence == 0 ~ "neutral"))

CB$CB_EXP_covid2

CB = CB |> mutate(
  CB_EXP_consp4 = case_when(Valence == 1 & EXP_consp < 3 ~ "mismatch_negative",
                              Valence == 1 & EXP_consp > 3 ~ "match_negative",
                              Valence == 2 & EXP_consp > 3 ~ "mismatch_positive",
                              Valence == 2 & EXP_consp < 3 ~ "match_positive",
                              Valence == 0 ~ "neutral"))

CB$CB_EXP_consp2
cor(CB$IAT, CB$EXP_covid)

CB = CB |> filter(Valence != "0")

CB$CB_EXP_consp2

## создаем колонку со значением CB_EXP4 - создается по EXP_general
##Valence 0 = нейтральное, 1 = негативное, 2 = позитивное.

#CB = CB |> mutate(
#  CB_EXP4 = case_when(Valence == 1 & EXP_general < 0 ~ "match_negative",
#                      Valence == 1 & EXP_general > 0 ~ "mismatch_positive",
#                      Valence == 2 & EXP_general > 0 ~ "match_positive",
#                      Valence == 2 & EXP_general < 0 ~ "mismatch_negative",
#                      Valence == 0 ~ "neutral"))

### DATA SAVING
write_xlsx(CB, "C:/Users/cybergnom/Documents/eegcb/data/CB.xlsx")

