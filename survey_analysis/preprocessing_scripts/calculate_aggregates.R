#THIS SCRIPT CALCULATES COMPOSITE FOR THE VARIOUS MEASURED CONCEPTS AND INTERNAL VALIDITY SCORES FOR THE SURVEYS.
#A KNOWLEDGE SCORE IS CALCULATED BY GIVING ONE POINT FOR A CORRECT ORDER OF TWO CHOICES (PARTICIPANTS NEED TO ORDER 8 (1ST QUIZ) AND 9 (2ND QUIZ) ITEMS AND CAN GET 9! OR 8! POINTS FOR THE WHOLE QUIZ)
#A CRONBACH ALPHA IS CALCULATED FOR EACH OF THE COMPOSITE SCORES

#TODO: NEED TO CHECK WHETHER SURVEYT1T2_AGGR HAS THE RIGHT DATA

library(tidyverse)
library(magrittr)
library(psych)
library(here)


# load data ---------------------------------------------------------------

# Option 1: read datasets and convert factors with separate script
surveyT1 <- read_csv(here("survey_analysis","datasets","processed","surveyT1.csv"))
surveyT2 <- read_csv(here("survey_analysis","datasets","processed","surveyT2.csv"))

# convert to factors
source(here("survey_analysis","preprocessing_scripts","convert_to_factor.R"))

# # Option 2: load pre-processed survey data (where factors are already ordered appropriately)
# source(here("survey_analysis","preprocessing_scripts","preprocess_survey_T1.R"))
# source(here("survey_analysis","preprocessing_scripts","preprocess_survey_T2.R"))

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
    as.numeric(knowledge_1_Product_type < knowledge_1_Hothouses),
  .keep = "unused"
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
    as.numeric(knowledge_2_Strawb_Scott < knowledge_2_Banana),
  .keep = "unused"
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
    as.numeric(knowledge_1_Product_type < knowledge_1_Hothouses),
  .keep = "unused"
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
    as.numeric(knowledge_2_Strawb_Scott < knowledge_2_Banana),
  .keep = "unused"
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
    .keep = "unused"
  ) %>%
  mutate(
    env_attitude = (env_attitude_1 + env_attitude_2 + env_attitude_3 + env_attitude_4)/4,
    SelfEff = (SelfEff_1 + SelfEff_2 + SelfEff_3)/3,
    intention = (intention_1 + intention_2) /2,
    spillover = (spillover_1 + spillover_2) /2,
    knowledge_score = (knowledge_1_score + knowledge_2_score)/2,
    UX8 = rowSums(across(UX_1_supportive:UX_8_leading))/8,
    UX10 = rowSums(across(UX_1_supportive:state_use_future))/10,
    helpful = rowSums(across((contains("state_helped"))))/3, 
    .keep = "unused"
  ) 

surveyT1_aggr <- surveyT1 %>% 
  mutate(across(where(is.factor), as.integer)) %>%
  select(!age:apps_name) #don't use these in aggregation
surveyT1_aggr %<>% 
  mutate(
    env_attitude_3 = 8-env_attitude_3_inv,
    .keep = "unused"
  ) %>%
  mutate(
    env_attitude = (env_attitude_1 + env_attitude_2 + env_attitude_3 + env_attitude_4)/4,
    SelfEff = (SelfEff_1 + SelfEff_2 + SelfEff_3)/3,
    knowledge_score = (knowledge_1_score + knowledge_2_score)/2, 
    .keep = "unused"
  ) 

## bind the aggregated data back to the original dataframes
surveyT1 %<>%
  left_join(.,surveyT1_aggr)

surveyT2 %<>%
  left_join(.,surveyT2_aggr)

#bind the two dataframes
survey_data_all <- bind_rows(surveyT1,surveyT2, .id = "survey_id")
#rename 1 to T1 and 2 t T2
survey_data_all %<>% mutate(across(survey_id, ~ case_match(.,"1" ~ "T1 (start)","2" ~ "T2 (end)")))

#what I want: a table with average scores for all participants (or households -- excluding the second participant) for all items as well as the composite score. for T1 and T2 separate. And SD? No not for composite scores. An SD and mean or median and IQR for each participant average/mean score. Also need graph showing the distributions (desity plots or box plots -- box plots with crosses for density). Cronbach alpha's should be added. 
#check how effect size for paired T-test is calculated, and how it is calculated for mixed methods.
#check how cronbach's alpha should be calculated. 
#check assumptions paired-T
# TO BE DONE: CALCULATE EFFECT SIZES. CONSIDER TRANSFORMING DATA TO HELP THE MODEL FIT

# https://stats.stackexchange.com/questions/257985/how-can-i-derive-effect-sizes-in-lme4-and-describe-the-magnitude-of-fixed-effect

# calculate internal validity (cronbacher's alpha) ------------------------------------

cron_alphas <-
  tibble(
    alpha_attitude = 
      (survey_data_all %>% 
         select(env_attitude_1, env_attitude_2, env_attitude_3_inv, env_attitude_4) %>%
         mutate(across(where(is.factor), as.integer)) %>%
         mutate(env_attitude_3 = 8 - env_attitude_3_inv, .keep = "unused") %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_self_eff = 
      (survey_data_all %>% 
         select(SelfEff_1:SelfEff_3) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_intent = 
      (survey_data_all %>% 
         select(intention_1,intention_2) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_spillover = 
      (survey_data_all %>% 
         select(spillover_1,spillover_2) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_knowledge =
      (survey_data_all %>%
         select(knowledge_1_score,knowledge_2_score) %>%
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
    alpha_UX8 = 
      (survey_data_all %>% 
         select(UX_1_supportive:UX_8_leading) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,    
    alpha_UX10 = 
      (survey_data_all %>% 
         select(UX_1_supportive:state_accept_alt) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,    
    alpha_helped = 
      (survey_data_all %>% 
         select(contains("state_helped")) %>% 
         mutate(across(where(is.factor), as.integer)) %>%
         psych::alpha()) %>% .$total %>% .$raw_alpha,
  )
#note, the warning refers to knowledge, which doesn't have categories really


# remove drop-out participants --------------------------------------------

survey_data_all %<>% filter(user_ID != 15)

# export ------------------------------------------------------------------

write_csv(survey_data_all,here("survey_analysis","datasets","processed","survey_data_all.csv"))
write_csv(cron_alphas,here("survey_analysis","datasets","processed","cron_alphas.csv"))


# experiment with a wide version of survey data all -----------------------

survey_data_all <- 
  left_join(surveyT1, surveyT2, by = c("user_ID"), suffix = c('_T1','_T2'))

#attempt to sort data by the way names appear in surveyT1
survey_data_all %<>%
  relocate(starts_with(names(surveyT1)))
#this works!

#participant ID in the front 
survey_data_all %<>% relocate(user_ID)

#update some names
names(survey_data_all) <- 
  names(survey_data_all) %>% 
  str_replace_all(.,"StartDate","survey_date") %>%
  str_replace_all(.,"duration_sec","survey_duration")

#include a column with information about study duration (days)
survey_data_all %<>%
  mutate(study_duration = survey_date_T2 - survey_date_T1, .after = survey_date_T2)

#convert duration to diff-time
survey_data_all %<>%
  mutate(
    across(
      starts_with('survey_duration'), 
      ~ as.difftime(./60, units = "mins")
      # .names = "{.col}_diff"
    )
  )


# ADD PARTICIPANT AND HOUSEHOLD INFORMATION -------------------------------

## bring in user data
user_data <- read_csv(here('common data','id_mapping_annonymous.csv')) 
## convert the start date to a date object, recorded in UTC (not BST)
user_data %<>% mutate(start_date = dmy_hm(start_date, tz="UTC"))
## change the display of the start date to BST
# attr(user_data$start_date, "tzone") <- "Europe/London"

#rename some variables in user_data
user_data %<>%
  rename(
    user_ID = ID
  )

# merge the two dataframes
survey_data_all %<>% 
  left_join(
    user_data %>% select(!mongo_id) %>% distinct(),
    .
  )

#verify time zone consistency
attr(survey_data_all$start_date, "tzone")
attr(survey_data_all$survey_date_T1, "tzone")
attr(survey_data_all$survey_date_T2, "tzone")

#all 3 in UTC


# export survey data in wide format ---------------------------------------

write_csv(survey_data_all,here("survey_analysis","datasets","processed","survey_data_all_wide.csv"))

