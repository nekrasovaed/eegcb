########### Main model ################

Model_A = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + as.factor(CB_EXP_consp2)
               + as.factor(CB_EXP_covid2)
               + Veracity
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

### PREPEARING WORKING SPACE
install.packages("ggplot2")
install.packages("writexl")
install.packages("dplyr") 
install.packages("car")
install.packages("gclus")
install.packages("GGally")
install.packages("psych")
install.packages("scatterPlotMatrix") 

library(lme4)
library(performance)
library(ordinal)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(writexl)
library(corrplot)
library(car)
library(gclus)
library(GGally)
library(corrplot)
library(scatterPlotMatrix)

### DATA LOADING AND PREPROCESSING
colnames(CB)

### Correlations #####

#visualize correlation matrix
cor(CB[7:10])
scatterplotMatrix(CB[7:10], diagonal = "histogram", smooth = FALSE)


Scatter_Matrix <- ggpairs(CB,columns = c(7:10), 
                          title = "Scatter Plot Matrix for Vaccination attitude", 
                          axisLabels = "show") 
Scatter_Matrix

library(ggplot2) 
library(psych) 
Scatter_Matrix2 <- pairs.panels(CB[7:10], main = "Scatter Plot Matrix for Vaccination attitude") 
ggsave("scatter plot matrix.png", Scatter_Matrix, width = 7, height = 7, units = "in") 

corrplot(cor(CB[7:10]))


########################################################
#################### CREDIBILITY #######################
########################################################

CB2 = CB |> filter(Valence != "0")

## Random factors testing
model = lmer(Performance_Rate1 ~ 1 + (1 | ID) + (1 | Row_num), data = CB) 
icc(model) 
model1 = lmer(Performance_Rate1 ~ 1 + (1 | ID), data = CB) 
icc(model1)
model2 = lmer(Performance_Rate1 ~ 1 + (1 | Row_num), data = CB)  ## This one
icc(model2) ## This one

check_heteroscedasticity(model2) 
check_autocorrelation(model2)
check_normality(model2)

colnames(CB)

summary(CB$EXP_general)

####### MODEL COMPAIRING ######################

Model_A = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + as.factor(CB_EXP_consp2)
               + as.factor(CB_EXP_covid2)
               + Veracity
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_B = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + as.factor(CB_EXP_consp2)
               + as.factor(CB_EXP_covid2)
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_C = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + as.factor(CB_EXP_consp2)
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_D = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_E = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))


AIC(Model_A, Model_B, Model_C, Model_D, Model_E)
BIC (Model_A, Model_B, Model_C, Model_D, Model_E)


summary(Model_A)
sjPlot::tab_model(Model_A)


#### Power analysis for present models
library(simr)
library(sjPlot)

Model_A = lmer(Performance_Rate1 ~ 1 
               + CB_IAT2
               + CB_EXP_general2
               + CB_EXP_consp2
               + CB_EXP_covid2
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_A1 = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
               + as.factor(CB_EXP_consp2)
               + as.factor(CB_EXP_covid2)
               + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_A1 = lmer(Performance_Rate1 ~ 1 
                + as.factor(CB_IAT2)
                + (1 | Row_num), data = CB2, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

sim_treat <- powerSim(Model_A, nsim=100, test = fcompare(Model_A1)) ##### 100

