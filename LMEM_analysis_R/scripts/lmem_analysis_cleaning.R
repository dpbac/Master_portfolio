## LOAD PACKAGES ####
library(dplyr)

## READ IN DATA ####
df_flow = read.csv("data/data_for_lmem_2019-07-08.csv")
glimpse(df_flow)

## ORGANIZE DATA ####

# Our main variables of interest are var_energy, var_valence, var_tempo,and level_extreme. Variable var_energy 
# and var_valence receive always the same value, therefore they cannot be together in the same model. 
# In addition, both energy and valence are proxy for mood. In addition, var_energy and var_valence receive always the same values,
# so I'll use in the analysis feature `var_mood' to make it clear that I'm considering the analysis of variation of mood
# on flow. 




# R choose the baseline alphabeticaly, and since I want low to be the baseline I'll change the position of the levels

df_flow <- df_flow %>%
          # changing the order of levels of var_mood
          mutate(var_mood = factor(var_mood, levels = c("low","high"))) %>%
          # changing the order of levels of var_tempo
          mutate(var_tempo = factor(var_tempo, levels = c("low","high")))


# Checking if the re-order of levels worked
levels(df_flow$var_mood)
levels(df_flow$var_tempo)

