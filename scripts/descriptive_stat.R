
### PREPEARING WORKING SPACE

install.packages("psych")
install.packages("ggplot2")
install.packages("writexl")
install.packages("dplyr") 
install.packages("skimr")

library(psych)
library(lme4)
library(performance)
library(ordinal)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(writexl)
library(nlme)
library(skimr)


### Descriptive statistics
CB2 %>%
  count(Performance_Rate1, sort = TRUE)

# A tibble: 9 × 2
#Performance_Rate1      n
#               <dbl> <int>
#1                 1   253
#2                 2   205
#3                 5   171
#4                 4   145
#5                 6   139
#6                 3   128
#7                 8   102
#8                 0    85
#9                 7    68

hist(CB2$Performance_Rate1)


CB2 %>%
  count(IAT, sort = TRUE)

# A tibble: 9 × 2
#Performance_Rate1      n
#               <dbl> <int>
#1                 1   253
#2                 2   205
#3                 5   171
#4                 4   145
#5                 6   139
#6                 3   128
#7                 8   102
#8                 0    85
#9                 7    68

hist(CB2$Performance_Rate1)

CB2 %>%
  group_by(Valence) %>%
  summarise(describe(Performance_Rate1))

# A tibble: 2 × 14
#Valence  vars       n  mean    sd median trimmed   mad   min   max range  skew kurtosis     se
#<dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>  <dbl>
#1       1     1   640  3.38  2.36      3    3.20  2.97     0     8     8 0.472   -0.849 0.0933
#2       2     1   656  3.72  2.38      4    3.65  2.97     0     8     8 0.165   -1.17  0.0928

CB2 %>%
  group_by(CB_IAT2) %>%
  summarise(describe(Performance_Rate1))

# A tibble: 2 × 14
#Valence  vars       n  mean    sd median trimmed   mad   min   max range  skew kurtosis     se
#<dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>  <dbl>
#1       1     1   640  3.38  2.36      3    3.20  2.97     0     8     8 0.472   -0.849 0.0933
#2       2     1   656  3.72  2.38      4    3.65  2.97     0     8     8 0.165   -1.17  0.0928
