## READ IN DATA ####
source("scripts/lmem_analysis_cleaning.R")

## LOAD PACKAGES ####

library(tidyr)
library(broom)
library(data.table)
library(devtools)
library(ggplot2)
library(pwr)
library(agricolae)
library(lme4)
library(effects)
library(eeptools)
library(merTools)
library(lattice)
library(lmerTest)
library(sjstats)
library(MuMIn)

## ORGANIZE DATA ####

# Check within and between variables

xtabs(~participant_id + var_mood + var_tempo, df_flow)
# There are no zeroes for conditions low-low, low-hig and high-low, as expected, since all participants 
# experienced all these conditions. High-low have only zeroes because the experimental design exclude this 
# condition. Then, var_mood and var_tempo are within-subject variables.

xtabs(~participant_id + level_extreme, df_flow)
# There are some zeros because by the experimental design partipants that evaluated 'extreme' playlists
# didn't evaluate the 'not extreme' playlists and vice-versa. Then, level_extreme is between-subject variable.

## BUILDING MODELS ####

# The first model built consist of a model considering all the variables available, so we can have an overview of 
# the structure of the random effects.

model_full <- lmer(flow_score ~ (1 | participant_id) + var_tempo + var_mood + level_extreme +
                     age + gender + hours_spotify_coded_7_scale + SUM_msi + extraversion +agreeableness + 
                     conscientiousness + neuroticism + openness + companionship +investment +
                     var_tempo:age + var_tempo:gender +var_tempo:hours_spotify_coded_7_scale + 
                     var_tempo:SUM_msi + var_tempo:extraversion + var_tempo:agreeableness + 
                     var_tempo:conscientiousness + var_tempo:neuroticism + var_tempo:openness +
                     var_tempo:companionship +var_tempo:investment + 
                     var_mood:age + var_mood:gender + var_mood:hours_spotify_coded_7_scale +
                     var_mood:SUM_msi + var_mood:extraversion + var_mood:agreeableness +
                     var_mood:conscientiousness + var_mood:neuroticism + var_mood:openness +
                     var_mood:companionship + var_mood:investment + 
                     level_extreme:age + level_extreme:gender + level_extreme:hours_spotify_coded_7_scale + 
                     level_extreme:SUM_msi + level_extreme:extraversion + level_extreme:agreeableness +
                     level_extreme:conscientiousness + level_extreme:neuroticism + level_extreme:openness + 
                     level_extreme:companionship + level_extreme:investment, data=df_flow, REML=FALSE)

model_full_sum <- summary(model_full)
model_full_sum

# # to save in .csv - Only works not using lmerTest. Therefore will not include p-values
# 
# df_full = tidy(model_full)
# head(df_full)
# 
# # save in csv
# 
# write.csv(df_full, file = "data/max_random_effects_results_190719.csv", row.names=FALSE)

# Model to test hypothesis H1

# H1: People rate the flow of a 2-track transition to be better when the variation in mood between 
# these two tracks is low, no matter if the variation in tempo is high or low

m_h1 <- lmer(flow_score~(1|participant_id) + var_mood + var_tempo + var_mood:var_tempo, data=df_flow, REML=FALSE)
m_h1_sum <- summary(m_h1)

m_h1_sum

# df_h1 = tidy(m_h1)
# head(df_h1)
#  
# # save in csv
# 
# write.csv(df_h1, file = "data/m_h1_190719.csv", row.names=FALSE)

# Model to test hyphotesis H2

# H2: Different levels of high variation in tempo (i.e. extreme and not extreme) have no influence on how people 
# rate flow as long as the variation in mood is low.

m_h2 <- lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme, data=df_flow, REML=FALSE)
m_h2_sum <- summary(m_h2)
m_h2_sum

# df_h2 = tidy(m_h2)
# head(df_h2)
# 
# # save in csv
# 
# write.csv(df_h2, file = "data/m_h2_190719.csv", row.names=FALSE)

## CHANGING THE BASELINE OF LEVEL_EXTREME ####
# In order to look at other baseline comparisons we're going to change 
# the baseline of our model within the code for the model itself.

# We want to see if our previous analysis also holds for 'not extreme' level.

m_h2_not_extr <- lmer(flow_score~(1|participant_id) + var_mood * var_tempo * relevel(level_extreme,"not_extreme"), 
                      data=df_flow, REML=FALSE)
m_h2_not_extr_sum <- summary(m_h2_not_extr)
m_h2_not_extr_sum

# df_h2_not_extr = tidy(m_h2_not_extr)
# head(df_h2_not_extr)
# 
# # save in csv
# 
# write.csv(df_h2_not_extr, file = "data/m_h2_not_extr_230719.csv", row.names=FALSE)
# 
# Building a model adding var_mood:companionship to model m_h2 since the full model showed that this interaction
# has a significant effect on flow

m_h2_companionship = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme + 
                            var_mood:companionship, data=df_flow, REML=FALSE)
m_h2_companionship_sum <- summary(m_h2_companionship)
m_h2_companionship_sum

# df_h2_companionship = tidy(m_h2_companionship)
# head(df_h2_companionship)
# 
# # save in csv
# 
# write.csv(df_h2_companionship, file = "data/m_h2_companionship_190719.csv", row.names=FALSE)

# Building a model for m_h2_companionship changing the baseline of level extreme

m_h2_companionship_not_extr = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * relevel(level_extreme, "not_extreme")
                                   + var_mood:companionship, data=df_flow, REML=FALSE)
m_h2_companionship_not_extr_sum <- summary(m_h2_companionship_not_extr)
m_h2_companionship_not_extr_sum

# df_h2_companionship_not_extr = tidy(m_h2_companionship_not_extr)
# head(df_h2_companionship_not_extr)
# 
# # save in csv
# 
# write.csv(df_h2_companionship_not_extr, file = "data/m_h2_companionship_not_extr_230719.csv", row.names=FALSE)



# OTHER ANALYSIS THAT DIDN'T SHOW SIGNIFICANT EFFECT ####

# We want to see if our previous analysis also holds for 'not extreme' level.

m_h2_not_extr <- lmer(flow_score~(1|participant_id) + var_mood * var_tempo * relevel(level_extreme,"not_extreme"), 
                      data=df_flow, REML=FALSE)
m_h2_not_extr_sum <- summary(m_h2_not_extr)
m_h2_not_extr_sum

# df_h2_not_extr = tidy(m_h2_not_extr)
# head(df_h2_not_extr)
# 
# # save in csv
# 
# write.csv(df_h2_not_extr, file = "data/m_h2_not_extr_230719.csv", row.names=FALSE)

# Building a model adding var_mood:companionship to model m_h2 since the full model showed that this interaction
# has a significant effect on flow

m_h2_companionship = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme + 
                            var_mood:companionship, data=df_flow, REML=FALSE)
m_h2_companionship_sum <- summary(m_h2_companionship)
m_h2_companionship_sum

# df_h2_companionship = tidy(m_h2_companionship)
# head(df_h2_companionship)
# 
# # save in csv
# 
# write.csv(df_h2_companionship, file = "data/m_h2_companionship_190719.csv", row.names=FALSE)

# Building a model for m_h2_companionship changing the baseline of level extreme

m_h2_companionship_not_extr = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * relevel(level_extreme, "not_extreme")
                                   + var_mood:companionship, data=df_flow, REML=FALSE)
m_h2_companionship_not_extr_sum <- summary(m_h2_companionship_not_extr)
m_h2_companionship_not_extr_sum

# df_h2_companionship = tidy(m_h2_companionship)
# head(df_h2_companionship)
# 
# # save in csv
# 
# write.csv(df_h2_companionship, file = "data/m_h2_companionship_190719.csv", row.names=FALSE)



# OTHER ANALYSIS THAT DIDN'T SHOW SIGNIFICANT EFFECT ####


# Analysing the effect of age, gender, and hours listening spotify per week on flow.

m_h2_age = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme * age_coded, data=df_flow, REML=FALSE)
m_h2_age_sum <- summary(m_h2_age)
m_h2_age_sum

m_h2_gender = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme * gender, data=df_flow, REML=FALSE)
m_h2_gender_sum <- summary(m_h2_gender)
m_h2_gender_sum

m_h2_spotify = lmer(flow_score~(1|participant_id) + var_mood * var_tempo * level_extreme 
                    + level_extreme:hours_spotify_coded_7_scale, data=df_flow, REML=FALSE)
m_h2_spotify_sum <- summary(m_h2_spotify)
m_h2_spotify_sum


# There is no siginificant effect of age, gender or hours listening spotify on flow.


## VALIDATION OF THE FINAL MODEL

# Checking the residuals

# we will check whether the residuals of the model are normally distributed (at both levels). 
# In addition to residuals being normally distributed, a multilevel model assume that variance of the residuals 
# is equal across groups (classes) for the different random effects. Statistical tests of normality and equality 
# of variance across groups do exists, but this tutorial is limited to visual inspections.

df = augment(m_h2_companionship)
head(df)

# save in csv

# write.csv(df, file = "data/residuals_mh_companionship_190719.csv", row.names=FALSE)

## COMPARING MODELS ####

# Comparing model with all variables with reduced models

# Full model x m_h1 (var_mood and var_tempo)

comp_full_m_h1 = anova(model_full, m_h1) 
comp_full_m_h1

# AIC is lower for the reduced model (m_h1) which means that the model including only var_mood and var_tempo 
# improves the fit of the model. However, this difference between models is not significant.

comp_full_m_h2 = anova(model_full, m_h2)
comp_full_m_h2

comp_full_m_h2_companionship = anova(model_full, m_h2_companionship)
comp_full_m_h2_companionship

# For the other two reduced models, i.e., model m_h2 that includes var_mood, var_tempo, and level_extreme 
# and model and m_h2_companionship which includs in addition of these three variables the interaction 
# var_mood:companionship the same is observed. The reduced models improve the fit of the model, but the difference
# between models is not significant.

# Comparing hypotheses models


comp_m_h1_m_h2 = anova(m_h1, m_h2)
comp_m_h1_m_h2

# AIC is lower for m_h2 which is m_h1 including level_extreme. This suggests that including level_extreme siginificantly
# improves the fit of the model. 

comp_m_h1_m_h2_companionship = anova(m_h1,m_h2_companionship)
comp_m_h1_m_h2_companionship

# The model that includes var_mood, var_tempo, level_extreme and the interaction var_mood:companionship, i.e., our final
# model has lower AIC than the model that uses only var_mood, var_tempo (m_h1). Therefore, including level extreme and 
# var_mood:companionship improves the model significantly.

comp_m_h2_m_h2_companionship = comp_m_h2_m_h2_companionship = anova(m_h2,m_h2_companionship)
comp_m_h2_m_h2_companionship

# When comparing the two last models, i.e., the model with var_mood, var_tempo, level_extreme and the interaction var_mood:companionship
# and the model without the interaction var_mood:companionship. The model including var_mood:companionship has lower AIC 
# suggesting that including the interaction improves the model significantly (p = 0.0158).

