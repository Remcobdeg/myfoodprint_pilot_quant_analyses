#pure cleaning of T2 survey data

library(tidyverse)
library(magrittr)
library(here)


## reading data

#read the Timepoint-2 survey data. It has 3 lines of headers, so we remove the first two
surveyT2 <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t2_v3_12_factor_strings June 2023_10.52.csv"), skip = 2)
#there is another copy of the survey responses at T2 with numeric values for the questions, rather than the answer strings. This is helpful for organizing the responses into factors.
surveyT2numeric <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t2_v3_30_factor_numbers+December+2023_16.48.csv"), skip = 2)

#the first two lines of header data contained valuable information that we want to use to create new headers for the survey T2
surveyT2Descriptions <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t2_v3_12_factor_strings June 2023_10.52.csv")) %>% slice(1)

# rename DF headers -------------------------------------------------------

##we need to inspect the header data and come up with new descriptive names
#first pivot the data to make investigation easier
surveyT2Descriptions %<>% pivot_longer(cols = everything(), names_to = "header", values_to = "description")

surveyT2Descriptions %<>% add_column(new_header = NA)
surveyT2Descriptions$new_header[1:18] = surveyT2Descriptions$header[1:18]
surveyT2Descriptions$new_header[6] = "duration_sec"
surveyT2Descriptions$new_header[19] = "user_ID"
surveyT2Descriptions$new_header[20:23] = c("env_attitude_1","env_attitude_2","env_attitude_3_inv","env_attitude_4")
surveyT2Descriptions$new_header[24:26] = c("SelfEff_1", "SelfEff_2", "SelfEff_3")
surveyT2Descriptions$new_header[27] = "barriers_desc"
surveyT2Descriptions$new_header[28] = "state_helped_goals"
surveyT2Descriptions$new_header[29] = "state_helped_control"
surveyT2Descriptions$new_header[30] = "state_helped_change"
surveyT2Descriptions$new_header[31] = "state_helped_change_desc"
surveyT2Descriptions$new_header[32:33] = c("intention_1","intention_2")
surveyT2Descriptions$new_header[34:35] = c("spillover_1","spillover_2")
levels_knowledge_1 <- c("Organic","Local","Product_type","Packeging","Store","Airfreight","Hothouses")
levels_knowledge_2 <- c("Banana","BerriesChile","Cheddar","Lamb","Strawb_Scott","TigerPrawn","Chicken","Strawb_Spain")
surveyT2Descriptions$new_header[36:42] = paste("knowledge_1",levels_knowledge_1,sep = "_")
surveyT2Descriptions$new_header[43:50] = paste("knowledge_2",levels_knowledge_2,sep = "_")
surveyT2Descriptions$new_header[51:58] = c("UX_1_supportive","UX_2_easy","UX_3_efficient","UX_4_clear","UX_5_exciting","UX_6_interesting","UX_7_inventive","UX_8_leading")
surveyT2Descriptions$new_header[59] = "state_accept_alt" 
surveyT2Descriptions$new_header[60] = "state_use_future" 

names(surveyT2) <- surveyT2Descriptions$new_header
names(surveyT2numeric) <- surveyT2Descriptions$new_header


# add level columns for factors --------------------------------------------------------------------

T2factors <-
  surveyT2 %>% 
  select(c(env_attitude_1:SelfEff_3,state_helped_goals:state_helped_change,intention_1:spillover_2,UX_1_supportive:state_use_future)) %>% 
  names(.)

surveyT2 <-
  left_join(
    surveyT2, 
    surveyT2numeric %>% select(c(StartDate,user_ID,any_of(T2factors))), 
    by = c("StartDate","user_ID"),
    suffix = c("","_level")
  )

# remove redundant rows and columns --------------------------------------------------------------------

##now remove test surveys
surveyT2 %<>% filter(Status != "Survey Test" & Status != "Survey Preview")
surveyT2 %<>% filter(EndDate > ymd("20230516"))

##remove columns that we're not interested in
surveyT2 %<>% select(StartDate,duration_sec,user_ID:state_use_future,paste0(T2factors,"_level"))
surveyT2 %<>% select(!state_helped_change_desc) #no one answered

surveyT2 %<>% mutate(user_ID = as.numeric(user_ID))

# create factors (depricated) ---------------------------------------------

# ##convert likert scale answers to factors
# levels_statements_1 <- c("strongly disagree", "disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "agree", "strongly agree")
# levels_statements_2 <- c("highly unlikely","unlikely","somewhat unlikely","neither likely nor unlikely","somewhat likely","likely","very likely")
# levels_statements_3 <- c("Very little","Little","Somewhat little","Neither much nor little","Somewhat much","Much","Very much")
# levels_UX_1 <- c("Very obstructive","Obstructive","Slightly obstructive","Neither supportive nor obstructive","Slightly supportive","Supportive","Very supportive")
# levels_UX_2 <- c("Very complicated","Complicated","Slightly complicated","Neither easy nor complicated","Slightly easy","Easy","Very easy")
# levels_UX_3 <- c("Very inefficient","Inefficient","Slightly inefficient","Neither efficient nor inefficient","Slightly efficient","Efficient","Very efficient")
# levels_UX_4 <- c("Very confusing","Moderately confusing","Confusing","Neither clear nor confusing","Clear","Moderately clear","Very clear")
# levels_UX_5 <- c("Very boring","Boring","Slightly boring","Neither exciting nor boring","Slightly exciting","Exciting","Very exciting")
# levels_UX_6 <- c("Not interesting at all","Not interesting","Slightly not-interesting","Neither interesting nor not-interesting","Slightly interesting","Interesting","Very interesting")
# levels_UX_7 <- c("Very conventional","Conventional","Slightly conventional","Neither inventive nor conventional","Slightly inventive","Inventive","Very inventive")
# levels_UX_8 <- c("Very usual","Usual","Slightly usual","Neither leading edge nor usual","Slightly leading edge","Leading edge","Very leading edge")
# levels_statements_4 <- c("Very difficult","Difficult","Slightly difficult","Neither easy nor difficult","Slightly easy","Easy","Very easy")
# 
# surveyT2 %<>% 
#   mutate(
#     across(c(env_attitude_1:SelfEff_3, spillover_1:spillover_2, state_helped_goals:state_helped_change,state_use_future), ~ factor(str_to_lower(.x), levels = levels_statements_1))
#   )
# surveyT2 %<>% 
#   mutate(
#     across(intention_1, ~ factor(.x, levels = levels_statements_2))
#   )
# surveyT2 %<>% 
#   mutate(
#     across(intention_2, ~ factor(.x, levels = levels_statements_3)),
#     across(UX_1_supportive, ~ factor(.x, levels = levels_UX_1)),
#     across(UX_2_easy, ~ factor(.x, levels = levels_UX_2)),
#     across(UX_3_efficient, ~ factor(.x, levels = levels_UX_3)),
#     across(UX_4_clear, ~ factor(.x, levels = levels_UX_4)),
#     across(UX_5_exciting, ~ factor(.x, levels = levels_UX_5)),
#     across(UX_6_interesting, ~ factor(.x, levels = levels_UX_6)),
#     across(UX_7_inventive, ~ factor(.x, levels = levels_UX_7)),
#     across(UX_8_leading, ~ factor(.x, levels = levels_UX_8)),
#     across(state_accept_alt, ~ factor(.x, levels = levels_statements_4)),
#   )

# surveyT2 %<>% 
#   mutate(
#     across(knowledge_1_1:knowledge_1_7, ~ factor(.x, levels = 1:7, labels = levels_knowledge_1)),
#     across(knowledge_2_1:knowledge_2_8 , ~ factor(.x, levels = 1:8, labels = levels_knowledge_2))
#     
#   )

# conduct factor tests ------------------------------------------------

#test factor creation process -- this needs to be repeated in subsequent files
for(var in T2factors){
  surveyT2[var] <- factor(surveyT2[var], levels = surveyT2[paste0(var,'_level')])
}

#retrospectively identify the factors in the df
names(surveyT2) %>% .[str_detect(.,'_level')] %>% str_replace_all(.,'_level','')


# export file -------------------------------------------------------------



write_csv(surveyT2,here("survey_analysis","datasets","processed","surveyT2.csv"))
