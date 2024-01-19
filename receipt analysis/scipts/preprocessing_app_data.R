# DESCRIBING THE STEPS AS A PROCESS FOR THE WRITE-UP: the receipt data that was recorded in the application and stored in MongoDB is downloaded. User identifiers in MongoDB are mapped onto study IDs. Data on start dates is added such that we can distinguish receipts that were scanned by the researcher as part of the preloaded data and receipts that are added during the study period. 
# Aggregated values are calculated per receipt. Per receipt a weight-corrected average footprint is calculated, as well as the total weight, number of items and footprint of (recorded) purchases on the receipt. Receipts that we scanned by the researcher as part of pre-loaded data are combined into a single receipt

library(tidyverse)
library(magrittr)
library(jsonlite)
library(here)

#read-in the receipts
purchase_data <- fromJSON(here("receipt analysis","data","original","receipts_final.json"))

purchase_data %<>% mutate(date_recorded = force_tz(date_recorded, tzone = "Europe/London"))

## clean up the data
purchase_data %<>% unnest(user) %>% rename(mongo_id = `$oid`)
purchase_data %<>% unnest(`_id`) %>% rename(item_id = `$oid`)
#we only want processed items (otherwise we have duplicates, as well as non-grocery items)
purchase_data %<>% filter(is_checked_off == T)
#check if we have any duplicates remaining
purchase_data %<>% distinct() #--> nope, nothing changes
#make sure numeric data is read as numeric
purchase_data %<>% mutate(
    item_weight_g = as.numeric(item_weight_g),
    item_footprint_g_100g = as.numeric(item_footprint_g_100g),
    item_kcal_100g = as.numeric(item_kcal_100g)
  )  

#remove columns with only nas
purchase_data %<>% select(!store_branche, item_product_detail)

# #investigate whether any variable have problematic values
# for (col in 1:ncol(purchase_data)){
#   cat("\n\n",names(purchase_data[col]),"\n\n")
#   cat("number of NA's in variable:")
#   purchase_data[,col] %>% is.na() %>% sum() %>% print()
#   cat("\n\nunique values in variable:")
#   purchase_data[,col] %>% unique() %>% print()
# } 


# ADD PARTICIPANT AND HOUSEHOLD INFORMATION -------------------------------

## bring in user data
user_data <- read_csv(here('common data','id_mapping_annonymous.csv')) 
## convert the start date to a date object, recorded in UTC (not BST)
user_data %<>% mutate(start_date = dmy_hm(start_date, tz="UTC"))
## change the display of the start date to the same timezone as date_recorded
attr(user_data$start_date, "tzone") <- "Europe/London"

#rename some variables in user_data
user_data %<>%
  rename(
    user_start = start_date,
    user_ID = ID
  )

#check whether all users are accounted for
purchase_data$mongo_id[!(purchase_data$mongo_id %in% user_data$mongo_id)] %>% unique()
#"63ff3abb7a74defef8cf5afe" "6429db483071e41bedb00892" --> these are both admin id's. We can remove those
purchase_data %<>% filter(mongo_id %in% user_data$mongo_id)

# #connect the user ids
# id_match <- match(purchase_data$mongo_id,user_data$mongo_id)
# purchase_data %<>% mutate(
#   mongo_id = user_data$ID[id_match],
#   user_start = user_data$start_date[id_match]
# )

##when the household includes multiple persons, there are multiple start dates. We need the earliest of the two as a household start date
#add household start date
user_data %<>% left_join(
  .,
  user_data %>% 
    select(household_ID, user_start) %>%
    arrange(user_start) %>% 
    distinct(household_ID, .keep_all = T) %>% 
    rename(hh_start = user_start)
)

# merge the two dataframes
purchase_data %<>% left_join(.,user_data, by = c("mongo_id" = "mongo_id"))

#verify time zone consistency
attr(purchase_data$date_recorded, "tzone")
attr(purchase_data$user_start, "tzone")

# reset recorded_date to household start for receipt that were added as pre-loaded data
purchase_data %<>%
  mutate(date_recorded = if_else(date_recorded > hh_start,date_recorded,hh_start))

#determine start-date normalized receipt dates (days since household enrollment)
purchase_data %<>% 
  mutate(day_recorded = difftime(date_recorded, hh_start, units = "days"))

# # calculate receipt-level values ------------------------------------------
# 
# 
# ## calculate average and total footprint per receipt
# rcpt_data <- purchase_data %>% 
#   group_by(date_recorded, user_ID, user_start) %>% #user_id and user_start are redundant for grouping but make sure this data appears in the created dataframe
#   summarise(
#     rcpt_CO2_g_100g = sum(item_footprint_g_100g * item_weight_g * item_units) / sum(item_weight_g * item_units),
#     rcpt_CO2_kg = sum(item_footprint_g_100g * item_weight_g * item_units),
#     rcpt_weight = sum(item_weight_g * item_units),
#     rcpt_items_n = n()
#   )
# 
# ## add a sequence number for each receipt per person
# rcpt_data %<>% group_by(user_ID) %>% mutate(hh_receipt_id = row_number())
# 
# ## distinguish receipts that were used for pre-filling the app with purchase data, and those that were added after the particpant was exposed to the app
# rcpt_data %<>% mutate(pre_study_rcpt = eval(date_recorded <= user_start))
# 
# ## merge receipts that were added before study start
# #date added becomes the start date


# receipt level values with receipts for pre-fill data merged -------------

## calculate average and total footprint per receipt
rcpt_data_v2 <- purchase_data %>% 
  group_by(date_recorded, household_ID, hh_start) %>% #user_id and user_start are redundant for grouping but make sure this data appears in the created dataframe
  #note: because we want to keep more variables than the ones we group by, we use mutate, select, distinct
  mutate(
    rcpt_CO2_g_100g = sum(item_footprint_g_100g * item_weight_g * item_units) / sum(item_weight_g * item_units),
    rcpt_CO2_kg = sum(item_footprint_g_100g * item_weight_g/100 * item_units)/1000,
    rcpt_weight_kg = sum(item_weight_g/1000 * item_units),
    rcpt_items_n = n()
  ) %>%
  select(date_recorded, user_ID:rcpt_items_n) %>% 
  distinct(date_recorded, household_ID, hh_start, .keep_all = T) # Note: Because both participants of household 9 provided receipts for the pre-loaded data we have to specify the variables in `distinct()`. Otherwise we get two rows for the first receipt of household 9 which messes up our analyses.

## add a sequence number for each receipt per person
rcpt_data_v2 %<>% group_by(household_ID) %>% mutate(hh_receipt_id = row_number(), .after = hh_start)

# export
rcpt_data_v2 %>% write_csv(here("receipt analysis","data","processed","receipt_level_data.csv"))


# update user_data with receipt counts and export------------------------------------

#calculate number of receipts per household
rcpt_data_v2 %<>%
  group_by(household_ID) %>%
  filter(date_recorded > hh_start) %>% #remove pre-loaded receipts
  mutate(n_receipts_hh = n_distinct(day_recorded))

#calculate number of receipts per participant
rcpt_data_v2 %<>% 
  group_by(user_ID) %>%
  filter(date_recorded > hh_start) %>% #remove pre-loaded receipts
  mutate(n_receipts_user = n_distinct(day_recorded))

rcpt_data_v2 %<>% ungroup()

## update user data with receipt counts
#we have to add n_receipts_hh and n_receipts_user separately, as not all users uploaded receipts 
user_data %<>%
  full_join(
    .,
    rcpt_data_v2 %>% 
      select(household_ID,n_receipts_hh) %>%
      distinct()
  )
user_data %<>%
  full_join(
    .,
    rcpt_data_v2 %>% 
      select(user_ID,n_receipts_user) %>%
      distinct()
  )

#those that have no receipts and receive 'NA' we asign 0
user_data$n_receipts_user %<>% replace_na(0)

#Now let's define a primary holdhold person for the study. This is the most active person (i.e., has the most receipts scanned)
user_data %<>% 
  group_by(household_ID) %>%
  mutate(primary = ifelse(n_receipts_user == max(n_receipts_user),'primary','secondary'))
#note, inspecting the data shows that no household has both members as primary

user_data %>% write_csv(here("common data","processed","participant_data.csv"))

