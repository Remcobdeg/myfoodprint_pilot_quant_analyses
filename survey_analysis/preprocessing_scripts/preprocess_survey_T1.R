#pure cleaning of T1 survey data

library(tidyverse)
library(magrittr)
library(here)


## reading data

#read the Timepoint-1 survey data. It has 3 lines of headers, so we remove the first two
surveyT1 <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t1_v2_23_factor_strings June 2023_12.21.csv"), skip = 2)
#there is another copy of the survey responses at T1 with numeric values for the questions, rather than the answer strings. This is helpful for organizing the responses into factors.
surveyT1numeric <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t1_v2_30_factor_numbers+December+2023_16.50.csv"), skip = 2)


# rename DF headers -------------------------------------------------------

#the first two lines of header data contained valuable information that we want to use to create new headers for the survey T1
surveyT1Descriptions <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t1_v2_23_factor_strings June 2023_12.21.csv")) %>% slice(1)

##we need to inspect the header data and come up with new descriptive names
#first pivot the data to make investigation easier
surveyT1Descriptions %<>% pivot_longer(cols = everything(), names_to = "header", values_to = "description")

surveyT1Descriptions %<>% add_column(new_header = NA)
surveyT1Descriptions$new_header[1:17] = surveyT1Descriptions$header[1:17]
surveyT1Descriptions$new_header[6] = "duration_sec"
surveyT1Descriptions$new_header[18] = "user_ID"
surveyT1Descriptions$new_header[19:22] = c("age","sex","sex_other","degree")
surveyT1Descriptions$new_header[23:27] =c("adults","adults_other","children","children_other","does_groceries")
surveyT1Descriptions$new_header[28] = "diet"
surveyT1Descriptions$new_header[29] = "diet_other"
surveyT1Descriptions$new_header[30:36] = paste("share",c("store","online","meal_box","veggie_box","farm_store","out-of-home","other"), sep = '_')
surveyT1Descriptions$new_header[37] = "other_def"
surveyT1Descriptions$new_header[38:39] = c("apps_used","apps_name")
surveyT1Descriptions$new_header[40:43] = c("env_attitude_1","env_attitude_2","env_attitude_3_inv","env_attitude_4")
surveyT1Descriptions$new_header[44:46] = c("SelfEff_1", "SelfEff_2", "SelfEff_3")
surveyT1Descriptions$new_header[47] = "barriers_desc"
levels_knowledge_1 <- c("Organic","Local","Product_type","Packeging","Store","Airfreight","Hothouses")
levels_knowledge_2 <- c("Banana","BerriesChile","Cheddar","Lamb","Strawb_Scott","TigerPrawn","Chicken","Strawb_Spain")
surveyT1Descriptions$new_header[48:54] = paste("knowledge_1",levels_knowledge_1,sep = "_")
surveyT1Descriptions$new_header[55:62] = paste("knowledge_2",levels_knowledge_2,sep = "_")

names(surveyT1) <- surveyT1Descriptions$new_header
names(surveyT1numeric) <- surveyT1Descriptions$new_header

# add level columns for factors --------------------------------------------------------------------

T1factors <-
  surveyT1 %>% 
  select(c(age,sex,degree,does_groceries,diet,share_store:`share_out-of-home`,env_attitude_1:SelfEff_3)) %>% 
  names(.)

surveyT1 <-
  left_join(
    surveyT1, 
    surveyT1numeric %>% select(c(StartDate,user_ID,any_of(T1factors))), 
    by = c("StartDate","user_ID"),
    suffix = c("","_level")
  )

# remove redundant rows and columns --------------------------------------------------------------------

##now remove test surveys
surveyT1 %<>% filter(EndDate > ymd("20230423"))

##remove columns that we're not interested in
surveyT1 %<>% 
  select(
    StartDate,duration_sec,user_ID:knowledge_2_Strawb_Spain,
    paste0(T1factors,"_level")
  )

#merge columns of 'other' descriptions
# surveyT1 %<>% mutate(sex = ifelse(is.na(sex_other),sex,paste(sex,sex_other)))
#there is only one 'sex other' variant, so we can use it to replace 'other'
surveyT1 %<>% mutate(sex = ifelse(is.na(sex_other),sex,"non-binary"))
surveyT1 %<>% select(!sex_other)
#there is only one adults_other variant, so we can safely use it to replace 'other'
surveyT1 %<>% mutate(adults = ifelse(is.na(adults_other),adults,adults_other))
surveyT1 %<>% mutate(adults = as.numeric(adults))
surveyT1 %<>% select(!adults_other)
# surveyT1 %<>% mutate(children = ifelse(is.na(children_other),children,children_other))
#there is no completed field for children other
surveyT1 %<>% select(!children_other)
#there is only one variant for diet_other so we can safely replace the 'other' 
surveyT1 %<>% mutate(diet = ifelse(is.na(diet_other),diet,diet_other))
surveyT1 %<>% select(!diet_other)

#adjust some misrecorded user IDs
surveyT1$user_ID[18] <- 39
surveyT1$user_ID[8] <- 20


# conduct factor tests ------------------------------------------------

#test factor creation process -- this needs to be repeated in subsequent files
for(var in T1factors){
  #make sure that the factor is organised
  surveyT1 %<>% arrange(get(paste0(var,'_level')))
  surveyT1[var] <- factor(surveyT1[var] %>% pull(.)) 
  # surveyT1[var] <- factor(surveyT1[var], levels = (surveyT1[[paste0(var,'_level')]] %>% unique(.) %>% sort(.)))
}

#retrospectively identify the factors in the df
names(surveyT1) %>% .[str_detect(.,'_level')] %>% str_replace_all(.,'_level','')

# ##convert likert scale answers to factors
# levels_statements_1 <- c("strongly disagree", "disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "agree", "strongly agree")
# 
# surveyT1 %<>% 
#   mutate(
#     across(c(env_attitude_1:SelfEff_3), ~ factor(str_to_lower(.x), levels = levels_statements_1))
#   )


# save file ---------------------------------------------------------------

write_csv(surveyT1,here("survey_analysis","datasets","processed","surveyT1.csv"))
