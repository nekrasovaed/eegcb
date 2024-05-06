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
library(corrplot)

### DATA LOADING AND PREPROCESSING
colnames(CB)

### Correlations #####
install.packages("car")
install.packages("gclus")
install.packages("GGally")
install.packages("psych")
install.packages("scatterPlotMatrix") 
library(car)
library(gclus)
library(GGally)
library(corrplot)
library(scatterPlotMatrix)

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



Model_A = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               + as.factor(CB_EXP_general2)
              # + as.factor(CB_EXP_consp2)
              # + as.factor(CB_EXP_covid2)
               + (1 | Row_num), data = CB, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

summary(Model_A)
sjPlot::tab_model(Model_A)

Model_A1 = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT2)
               #+ as.factor(CB_EXP_general2)
               #+ as.factor(CB_EXP_consp2)
               + as.factor(CB_EXP_covid2)
               + (1 | Row_num), data = CB, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

summary(Model_A1)
sjPlot::tab_model(Model_A1)

Model_A2 = lmer(Performance_Rate1 ~ 1 
                + as.factor(CB_IAT4)
                + as.factor(CB_EXP_general4)
                + as.factor(CB_EXP_consp4)
                + as.factor(CB_EXP_covid4)
                + (1 | Row_num), data = CB, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

summary(Model_A2)
sjPlot::tab_model(Model_A2)


AIC(Model_A, Model_A1, Model_A2)

Model_B = lmer(Performance_Rate1 ~ 1 
               + as.factor(CB_IAT4)
               + (1 | Row_num), data = CB, REML = FALSE, control = lmerControl(calc.derivs = FALSE))


summary(Model_B)
sjPlot::tab_model(Model_B)

Model_C = lmer(Performance_Rate1 ~ 1 
               + IAT*Valence
               + EXP_general*Valence
               + EXP_covid*Valence
               + EXP_consp*Valence
               + (1 | Row_num), data = CB, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

summary(Model_C)
sjPlot::tab_model(Model_C)


Model_B = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + as.factor(place)
               + scale(ML)
               + scale(CRT)
               + scale(attitude)*as.factor(news_valence)*as.factor(comment_seen)
               + news_issue
               + news_truth
               +(1 | ID)
               +(1 | id_news), data = EM,  REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_C = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + as.factor(place)
               + scale(ML)
               + scale(CRT)
               + scale(attitude)*as.factor(news_valence)*as.factor(comment_seen)*news_issue
               + news_truth
               +(1 | ID)
               +(1 | id_news), data = EM,  REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_D = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + as.factor(place)
               + scale(ML)
               + scale(CRT)
               + scale(attitude)*as.factor(news_valence)*as.factor(comment_seen_precense)*news_issue
               + news_truth
               +(1 | ID)
               +(1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

AIC(Model_A, Model_B, Model_C, Model_D)
BIC(Model_A, Model_B, Model_C, Model_D)

summary(Model_D)
sjPlot::tab_model(Model_D)

#### Power analysis for present models
library(simr)
library(sjPlot)

#current model
Model_D = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + as.factor(place)
               + scale(ML)
               + scale(CRT)
               + scale(attitude)*as.factor(news_valence)*as.factor(comment_seen_precense)*news_issue
               + news_truth
               +(1 | ID)
               +(1 | id_news), data = EM,  REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_D1 = lmer(credibility ~ 1 
                + scale(age)
                + as.factor(gender)
                + as.factor(place)
                + scale(ML)
                + scale(CRT)
                + scale(attitude)*as.factor(news_valence)*as.factor(comment_seen_precense)
                + news_issue
                + news_truth
                +(1 | ID)
                +(1 | id_news), data = EM,  REML = FALSE, control = lmerControl(calc.derivs = FALSE))

#power of current model
power <- powerSim(Model_D,test = fcompare(Model_D1), nsim = 1000) 
power #99.8

##################### PLOTS ##########################
###### Attitude * News_valence #####################
stats_two_interactions_text = EM |>
  group_by(attitude, news_valence) |>
  summarise(
    N = n(),
    credibility_mean = mean(credibility),
    credibility_se = 1.96 * sd(credibility)/sqrt(N),
    .groups = "drop")

g1 <- ggplot(data = stats_two_interactions_text, aes(x = attitude, y = credibility_mean, group = news_valence, color = news_valence)) +
  geom_smooth(method = 'lm',  size = 2) +
  ylab("Credibility") + xlab("Attitude toward topics") +
  labs(color = "News\nvalence")

###### Attitude * News_valence * Comment_presence #####################
stats_three_interactions3 = EM |>
  group_by(attitude, news_valence, COMM_binary2) |>
  summarise(
    N = n(),
    credibility_mean = mean(credibility),
    credibility_se = 1.96 * sd(credibility)/sqrt(N),
    .groups = "drop")

ggplot(data = stats_three_interactions3, aes(x = attitude, y = credibility_mean, group = news_valence, color = news_valence)) +
  geom_point() + geom_smooth(se = F) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  ylab("Credibility") + xlab("Attitude") +
  labs(color = "News valence") +
  geom_errorbar(aes(ymin = credibility_mean - credibility_se, ymax = credibility_mean + credibility_se), width = .2) +
  facet_grid(cols = vars(COMM_binary2), labeller = label_both)

########################################################
#################### ACCURACY BINARY #######################
########################################################

model = lmer(ACC ~ 1 + (1 | ID) + (1 | id_news), data = EM) 
icc(model)
model1 = lmer(ACC ~ 1 + (1 | ID), data = EM) 
icc(model1) 
model2 = lmer(ACC ~ 1 + (1 | id_news), data = EM) 
icc(model2) 

check_heteroscedasticity(model) 
check_autocorrelation(model) 
check_normality(model) 

colnames(EM)
Model_ACC_A = lmer(ACC ~ 1 
                   + scale(age)
                   + as.factor(gender)
                   + as.factor(place)
                   + scale(ML)
                   + scale(CRT)
                   + scale(attitude_centered)*as.factor(news_valence)
                   + scale(attitude_centered)*as.factor(comment_seen)
                   + news_issue
                   + news_truth
                   #+ (1 | ID)
                   + (1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACC_B = lmer(ACC ~ 1 
                   + scale(age)
                   + as.factor(gender)
                   + as.factor(place)
                   + scale(ML)
                   + scale(CRT)
                   + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)
                   + news_issue
                   + news_truth
                   #+ (1 | ID)
                   + (1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACC_C = lmer(ACC ~ 1 
                   + scale(age)
                   + as.factor(gender)
                   + as.factor(place)
                   + scale(ML)
                   + scale(CRT)
                   + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)*news_issue
                   + news_truth
                   + delta_sec
                   + text_reread
                   #+ (1 | ID)
                   + (1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACC_D = lmer(ACC ~ 1 
                   + scale(age)
                   + as.factor(gender)
                   + as.factor(place)
                   + scale(ML)
                   + scale(CRT)
                   + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen_precense)*news_issue
                   + news_truth
                   # + (1 | ID)
                   + (1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

AIC(Model_ACC_A, Model_ACC_B, Model_ACC_C, Model_ACC_D)
BIC(Model_ACC_A, Model_ACC_B, Model_ACC_C, Model_ACC_D)

summary(Model_ACC_A)
r2(Model_ACC_A)
sjPlot::tab_model(Model_ACC_B)

#power of model for ACCURACY BINARY
power <- powerSim(Model_ACC_C,test = fcompare(Model_ACC_B), nsim = 1000) 
power #lack of power

##########ACC FULL SCALE###########
model <- lmer(ACC_full_scale ~ 1 + (1 | id_news) + (1 | ID), data = EM)
icc(model)
model <- lmer(ACC_full_scale ~ 1 + (1 | ID), data = EM)
icc(model)
model <- lmer(ACC_full_scale ~ 1 + (1 | id_news), data = EM)
icc(model)

Model_ACCF_A = lmer(ACC_full_scale ~ 1 
                    + scale(age)
                    + as.factor(gender)
                    + as.factor(place)
                    + scale(ML)
                    + scale(CRT)
                    + scale(attitude_centered)*as.factor(news_valence)
                    + scale(attitude_centered)*as.factor(comment_seen)
                    + news_issue
                    + news_truth
                    + (1 | ID)
                    +(1 | id_news), data = EM_lgbt, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACCF_B = lmer(ACC_full_scale ~ 1 
                    + scale(age)
                    + as.factor(gender)
                    + as.factor(place)
                    + scale(ML)
                    + scale(CRT)
                    + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)
                    + news_issue
                    + news_truth
                    + (1 | ID)
                    +(1 | id_news), data = EM,  REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACCF_C = lmer(scale(ACC_full_scale) ~ 1 
                    + scale(age)
                    + as.factor(gender)
                    + as.factor(place)
                    + scale(ML)
                    + scale(CRT)
                    + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)*news_issue
                    + news_truth
                    + (1 | ID)
                    +(1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

Model_ACCF_D = lmer(ACC_full_scale ~ 1 
                    + scale(age)
                    + as.factor(gender)
                    + as.factor(place)
                    + scale(ML)
                    + scale(CRT)
                    + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen_precense)*news_issue
                    + news_truth
                    + (1 | ID)
                    +(1 | id_news), data = EM, REML = FALSE, control = lmerControl(calc.derivs = FALSE))

AIC(Model_ACCF_A, Model_ACCF_B, Model_ACCF_C, Model_ACCF_D)
BIC(Model_ACCF_A, Model_ACCF_B, Model_ACCF_C, Model_ACCF_D)

summary(Model_ACCF_C)
r2(Model_ACCF_C)
sjPlot::tab_model(Model_ACCF_B)

#### Power analysis for present models
library(simr)
library(sjPlot)

#power of model for ACCURACY FULL
power <- powerSim(Model_ACCF_C,test = fcompare(Model_ACCF_B), nsim = 1000) 
power #99.8

############# PLOTS FO ACCURACY ##################

stats_three_interactions_ACC = EM |>
  group_by(attitude_centered, news_valence, comment_valence) |>
  summarise(
    N = n(),
    ACC_mean = mean(ACC_full_scale),
    ACC_se = 1.96 * sd(ACC_full_scale)/sqrt(N),
    .groups = "drop")

ggplot(data = stats_three_interactions_ACC, aes(x = attitude_centered, y = ACC_mean, group = news_valence, color = news_valence)) +
  geom_point() + geom_smooth(se = F) +
  scale_x_continuous(breaks = c(0, 1, 2, 3)) +
  ylab("Detection accuracy") + xlab("Attitude") +
  labs(color = "News valence") +
  geom_errorbar(aes(ymin = ACC_mean - ACC_se, ymax = ACC_mean + ACC_se), width = .2) +
  facet_grid(cols = vars(comment_valence), labeller = label_both)

ggplot(data = stats_three_interactions_ACC, aes(x = attitude_centered, y = ACC_mean, group = comment_valence, color = comment_valence)) +
  geom_point() + geom_smooth(se = F) +
  scale_x_continuous(breaks = c(0, 1, 2, 3)) +
  ylab("Detection accuracy") + xlab("Attitude") +
  labs(color = "News valence") + 
  geom_errorbar(aes(ymin = ACC_mean - ACC_se, ymax = ACC_mean + ACC_se), width = .2) +
  facet_grid(cols = vars(news_valence), labeller = label_both)

################DURATION###############

EM2 <- EM_ANOVA_Duration
model1 = lmer(Duration ~ 1 + (1 | id_news), data = EM2) 
icc(model1) 
model2 = lmer(Duration ~ 1 + (1 | ID), data = EM2) 
icc(model2)

Model_Duration_A = lmer(Duration ~ 1 
                        + scale(age)
                        + as.factor(gender)
                        + as.factor(place)
                        + scale(ML)
                        + scale(CRT)
                        + scale(attitude_centered)*as.factor(news_valence)
                        + scale(attitude)*as.factor(comment_seen)
                        + news_issue
                        + news_truth
                        +(1 | ID), data = EM2)

Model_Duration_B = lmer(Duration ~ 1 
                        + scale(age)
                        + as.factor(gender)
                        + as.factor(place)
                        + scale(ML)
                        + scale(CRT)
                        + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)
                        + news_issue
                        + news_truth
                        +(1 | ID), data = EM2)

Model_Duration_C = lmer(Duration ~ 1 
                        + scale(age)
                        + as.factor(gender)
                        + as.factor(place)
                        + scale(ML)
                        + scale(CRT)
                        + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen)*news_issue
                        + news_truth
                        +(1 | ID), data = EM2)

Model_Duration_D = lmer(Duration ~ 1 
                        + scale(age)
                        + as.factor(gender)
                        + as.factor(place)
                        + scale(ML)
                        + scale(CRT)
                        + scale(attitude_centered)*as.factor(news_valence)*as.factor(comment_seen_precense)*news_issue
                        + news_truth
                        +(1 | ID), data = EM2)

AIC(Model_Duration_A, Model_Duration_B, Model_Duration_C, Model_Duration_D)
sjPlot::tab_model(Model_Duration_C)

power<-powerSim(Model_Duration_2,test = fcompare(Model_Duration_1), nsim = 1000) 
power #99.8