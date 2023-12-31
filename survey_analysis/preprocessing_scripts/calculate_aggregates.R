#THIS SCRIPT CALCULATES COMPOSITE FOR THE VARIOUS MEASURED CONCEPTS AND INTERNAL VALIDITY SCORES FOR THE SURVEYS.
#A KNOWLEDGE SCORE IS CALCULATED BY GIVING ONE POINT FOR A CORRECT ORDER OF TWO CHOICES (PARTICIPANTS NEED TO ORDER 8 (1ST QUIZ) AND 9 (2ND QUIZ) ITEMS AND CAN GET 9! OR 8! POINTS FOR THE WHOLE QUIZ)
#A CRONBACH ALPHA IS CALCULATED FOR EACH OF THE COMPOSITE SCORES

#TODO: NEED TO CHECK WHETHER SURVEYT1T2_AGGR HAS THE RIGHT DATA

library(tidyverse)
library(magrittr)
library(psych)
library(here)

#not running this line; we assume that the dataset is already loaded. otherwise we need to convert to factors
# surveyT2 <- read_csv(here("datasets","processed","surveyT2.csv")) 
#load pre-processed survey data (where factors are ordered appropriately)
source(here("survey_analysis","preprocessing_scripts","preprocess_survey_T1.R"))
source(here("survey_analysis","preprocessing_scripts","preprocess_survey_T2.R"))


# calculate knowledge scores for the quizes -----------------------------

# scores are calculated as percentages of max (e.g., 76 means 76% of the maximal achievable score)

## score quizzes

# Answers on the knowledge quizzes are: 
#   
#   Q1: What do you believe are the best ways you can limit your foodprint?
#   
#   These should be at the top:
#   - Reduce certain types of products, no matter the season or origin
# - Avoid airfreighted products
# - Avoid products grown in hothouses (heated greenhouses)
# 
# These should be at the bottom:
#   - Reduce packaging
# - Buy local produce
# - Be considerate in choosing your grocer
# - Buy organic
# 
# Q2: Which items do you think have the highest foodprint (on average)?
#   
#   1: 100g tiger prawns
# 2: 100g lamb (UK)
# 3/4: 100g fresh blueberries, bought in January (Chile) 
# 3/4/8: 100g Strawberries (Scotland)?
#   5: 100g of Brittish cheddar (UK)
# 6: 100g chicken (UK)
# 7: 100g Strawberries (Spain)
# 9: 100g of banana (Peru)

#-->determine position each choice, 1 point each time it is correctly higher than another
#e.g. position (where) 'tiger prawns' < position 'lamb' --> T/F
#e.g. position (where) 'tiger prawns' < position 'chicken' --> T/F
#etc.
#count number of T
#calculate max score. #check that max score is equal for the various positions of strawberries sc.

surveyT2 %<>% mutate(
  knowledge_1_score = 
    as.numeric(knowledge_1_Organic > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Organic > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Organic > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Local > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Local > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Local > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Packeging > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Packeging > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Packeging > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Store > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Store > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Store > knowledge_1_Hothouses) +
    as.numeric(knowledge_1_Product_type < knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Product_type < knowledge_1_Hothouses)
) #max score = 14

surveyT2 %<>% mutate(
  knowledge_1_score = round(knowledge_1_score/14*100,2)
)

surveyT2 %<>% mutate(
  knowledge_2_score = 
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Banana) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_BerriesChile) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Lamb) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Strawb_Scott) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Chicken) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Banana) +
    as.numeric(knowledge_2_Lamb < knowledge_2_BerriesChile) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Strawb_Scott) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Chicken) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Banana) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Chicken) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Banana) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Chicken) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Chicken < knowledge_2_Banana) +
    as.numeric(knowledge_2_Chicken < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Strawb_Spain < knowledge_2_Banana) +
    as.numeric(knowledge_2_Strawb_Scott < knowledge_2_Banana)
)
#max score = 24
#there is an irregularity in the score for participant 30. This participant scores only 2/24 points, yet evidences good understanding of foodprint in the interview and scores maximal on knowledge question 1. It is reasonable to assume this participant misread the question and ordered from lowest to highest foodprint. In that case, this person scores 22 points!
surveyT2 %<>% mutate(knowledge_2_score = if_else(user_ID == 30, 24 - knowledge_2_score, knowledge_2_score))

surveyT2 %<>% mutate(
  knowledge_2_score = round(knowledge_2_score/24*100,2)
)

# repeat this for survey 1

surveyT1 %<>% mutate(
  knowledge_1_score = 
    as.numeric(knowledge_1_Organic > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Organic > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Organic > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Local > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Local > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Local > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Packeging > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Packeging > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Packeging > knowledge_1_Hothouses) + 
    as.numeric(knowledge_1_Store > knowledge_1_Product_type) +
    as.numeric(knowledge_1_Store > knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Store > knowledge_1_Hothouses) +
    as.numeric(knowledge_1_Product_type < knowledge_1_Airfreight) + 
    as.numeric(knowledge_1_Product_type < knowledge_1_Hothouses)
) #max score = 14

surveyT1 %<>% mutate(
  knowledge_1_score = round(knowledge_1_score/14*100,2)
)

surveyT1 %<>% mutate(
  knowledge_2_score = 
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Banana) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_BerriesChile) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Lamb) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Strawb_Scott) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Chicken) +
    as.numeric(knowledge_2_TigerPrawn < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Banana) +
    as.numeric(knowledge_2_Lamb < knowledge_2_BerriesChile) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Strawb_Scott) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Chicken) +
    as.numeric(knowledge_2_Lamb < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Banana) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Cheddar) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Chicken) +
    as.numeric(knowledge_2_BerriesChile < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Banana) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Chicken) +
    as.numeric(knowledge_2_Cheddar < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Chicken < knowledge_2_Banana) +
    as.numeric(knowledge_2_Chicken < knowledge_2_Strawb_Spain) +
    as.numeric(knowledge_2_Strawb_Spain < knowledge_2_Banana) +
    as.numeric(knowledge_2_Strawb_Scott < knowledge_2_Banana)
)
#max score = 24

surveyT1 %<>% mutate(
  knowledge_2_score = round(knowledge_2_score/24*100,2)
)


# calculate aggregated concept scores -------------------------------------

# for each participant, calculate the composite concept score by taking the average of the concept items
surveyT2_aggr <- surveyT2 %>% 
  mutate(across(where(is.factor), as.integer)) 
surveyT2_aggr %<>% 
  mutate(
    env_attitude_3 = 8-env_attitude_3_inv,
    # .keep = "unused"
  ) %>%
  mutate(
    env_attitude = (env_attitude_1 + env_attitude_2 + env_attitude_3 + env_attitude_4)/4,
    SelfEff = (SelfEff_1 + SelfEff_2 + SelfEff_3)/3,
    intention = (intention_1 + intention_2) /2,
    spillover = (spillover_1 + spillover_2) /2,
    knowledge_score = (knowledge_1_score + knowledge_2_score)/2,
    UX8 = rowSums(across(UX_1_supportive:UX_8_leading))/8,
    UX9 = rowSums(across(UX_1_supportive:state_accept_alt))/9,
    helpful = rowSums(across((contains("state_helped"))))/3, 
    # .keep = "unused"
  ) 

surveyT1_aggr <- surveyT1 %>% 
  mutate(across(where(is.factor), as.integer)) 
surveyT1_aggr %<>% 
  mutate(
    env_attitude_3 = 8-env_attitude_3_inv,
    # .keep = "unused"
  ) %>%
  mutate(
    env_attitude = (env_attitude_1 + env_attitude_2 + env_attitude_3 + env_attitude_4)/4,
    SelfEff = (SelfEff_1 + SelfEff_2 + SelfEff_3)/3,
    knowledge_score = (knowledge_1_score + knowledge_2_score)/2, 
    # .keep = "unused"
  ) 

#also remove raw knowledge scores ... should I
#what I want: a table with average scores for all participants (or households -- excluding the second participant) for all items as well as the composite score. for T1 and T2 separate. And SD? No not for composite scores. An SD and mean or median and IQR for each participant average/mean score. Also need graph showing the distributions (desity plots or box plots -- box plots with crosses for density). Cronbach alpha's should be added. 
#check how effect size for paired T-test is calculated, and how it is calculated for mixed methods.
#check how cronbach's alpha should be calculated. 
#check assumptions paired-T
#so what is the dataframe that I need? 
# TO BE DONE: CALCULATE EFFECT SIZES. CONSIDER TRANSFORMING DATA TO HELP THE MODEL FIT

# https://stats.stackexchange.com/questions/257985/how-can-i-derive-effect-sizes-in-lme4-and-describe-the-magnitude-of-fixed-effect


#TRY BINDING ROWS OF THE AGGREGATES

surveyT1T2_aggr <- bind_rows(surveyT1_aggr,surveyT2_aggr, .id = "survey_id")
#rename 1 to T1 and 2 t T2
surveyT1T2_aggr %<>% mutate(across(survey_id, ~ case_match(.,"1" ~ "T1 (start)","2" ~ "T2 (end)")))

# add concept scores to the survey data dataframe -------------------------

#add this data to the original dataframe
# surveyT2 %<>% 
#   add_column(env_attitude = surveyT2_aggr$env_attitude, .after = env_attitude_4) %>%
#   add_column(SelfEff = surveyT2_aggr$SelfEff, .after = SelfEff_3) %>%
#   add_column(intention = surveyT2_aggr$intention, .after = intention_2) %>%
#   add_column(spillover = surveyT2_aggr$spillover, .after = spillover_2) %>%
#   add_column(knowledge_score = surveyT2_aggr$knowledge_score, .after = knowledge_2_Strawb_Spain) %>%
#   add_column(UX8 = surveyT2_aggr$UX8, .after = UX_8_leading) %>%
#   add_column(UX9 = surveyT2_aggr$UX9, .after = state_accept_alt) %>%
#   add_column(UX9 = surveyT2_aggr$helpful, .after = state_helped_change)

surveyT2 %<>% 
  add_column(env_attitude = surveyT2_aggr$env_attitude) %>%
  add_column(SelfEff = surveyT2_aggr$SelfEff) %>%
  add_column(intention = surveyT2_aggr$intention) %>%
  add_column(spillover = surveyT2_aggr$spillover) %>%
  add_column(knowledge_score = surveyT2_aggr$knowledge_score) %>%
  add_column(UX8 = surveyT2_aggr$UX8) %>%
  add_column(UX9 = surveyT2_aggr$UX9) %>%
  add_column(helpful = surveyT2_aggr$helpful)

rm(surveyT2_aggr)  

surveyT2 %<>% select(
  StartDate:user_ID, 
  env_attitude:knowledge_score,  
  state_helped_goals:state_helped_change,
  helpful,
  UX_1_supportive:state_accept_alt,
  UX8:UX9,
  state_use_future
)


surveyT1 %<>% 
  add_column(env_attitude = surveyT1_aggr$env_attitude) %>%
  add_column(SelfEff = surveyT1_aggr$SelfEff) %>%
  add_column(knowledge_score = surveyT1_aggr$knowledge_score)

rm(surveyT1_aggr)  

surveyT1 %<>% select(
  StartDate:apps_name, 
  barriers_desc,
  env_attitude:knowledge_score
)



# calculate internal validity (cronbacher's alpha) ------------------------------------

cron_alphas_ <-
  tibble(
    alpha_attitude = 
      (surveyT1T2_aggr %>% 
         select(env_attitude_1, env_attitude_2, env_attitude_3, env_attitude_4) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_self_eff = 
      (surveyT1T2_aggr %>% 
         select(SelfEff_1:SelfEff_3) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_intent = 
      (surveyT1T2_aggr %>% 
         select(intention_1,intention_2) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_spillover = 
      (surveyT1T2_aggr %>% 
         select(spillover_1,spillover_2) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_knowledge = 
      (surveyT1T2_aggr %>% 
         select(knowledge_1_score,knowledge_2_score) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_UX8 = 
      (surveyT1T2_aggr %>% 
         select(UX_1_supportive:UX_8_leading) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,    
    alpha_UX9 = 
      (surveyT1T2_aggr %>% 
         select(UX_1_supportive:state_accept_alt) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,    
    alpha_helped = 
      (surveyT1T2_aggr %>% 
         select(contains("state_helped")) %>% 
         psych::alpha()) %>% .$total %>% .$raw_alpha,
  )
#note, the warning refers to knowledge, which doesn't have categories really

# #calculate cronbacher's alpha
# cron_alphas <-
#   tibble(
#     alpha_attitude = 
#       (surveyT2_aggr %>% 
#          select(env_attitude_1, env_attitude_2, env_attitude_3, env_attitude_4) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#     alpha_self_eff = 
#       (surveyT2_aggr %>% 
#          select(SelfEff_1:SelfEff_3) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#     alpha_intent = 
#       (surveyT2_aggr %>% 
#          select(intention_1,intention_2) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#     alpha_spillover = 
#       (surveyT2_aggr %>% 
#          select(spillover_1,spillover_2) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#     alpha_knowledge = 
#       (surveyT2_aggr %>% 
#          select(knowledge_1_score,knowledge_2_score) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#     alpha_UX8 = 
#       (surveyT2_aggr %>% 
#          select(UX_1_supportive:UX_8_leading) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,    
#     alpha_UX9 = 
#       (surveyT2_aggr %>% 
#          select(UX_1_supportive:state_accept_alt) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,    
#     alpha_helped = 
#       (surveyT2_aggr %>% 
#          select(contains("state_helped")) %>% 
#          psych::alpha()) %>% .$total %>% .$raw_alpha,
#   )


