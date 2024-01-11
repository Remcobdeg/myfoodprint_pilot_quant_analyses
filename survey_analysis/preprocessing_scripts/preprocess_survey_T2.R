#pure cleaning of T2 survey data

library(tidyverse)
library(magrittr)
library(here)


## reading data

#read the Timepoint-2 survey data. It has 3 lines of headers, so we remove the first two
surveyT2 <- read_csv(here("survey_analysis","datasets","original","myfoodprint_t2_v3_12_factor_strings June 2023_10.52.csv"), skip = 2)

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


# remove redundant rows and columns --------------------------------------------------------------------

##now remove test surveys
surveyT2 %<>% filter(Status != "Survey Test" & Status != "Survey Preview")
surveyT2 %<>% filter(EndDate > ymd("20230516"))

##remove columns that we're not interested in
surveyT2 %<>% select(StartDate,duration_sec,user_ID:state_use_future)
surveyT2 %<>% select(!state_helped_change_desc) #no one answered

surveyT2 %<>% mutate(user_ID = as.numeric(user_ID))
#id 16 registered as id 15 in the exit survey. This needs correction
surveyT2 %<>% mutate(user_ID = ifelse(user_ID == 15,16,user_ID))




# export file -------------------------------------------------------------



write_csv(surveyT2,here("survey_analysis","datasets","processed","surveyT2.csv"))
