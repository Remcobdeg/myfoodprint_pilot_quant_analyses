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
attr(rcpt_data_v2$date_recorded, "tzone")
attr(rcpt_data_v2$user_start, "tzone")

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
#     rcpt_C02_ave = sum(item_footprint_g_100g * item_weight_g * item_units) / sum(item_weight_g * item_units),
#     rcpt_C02_total = sum(item_footprint_g_100g * item_weight_g * item_units),
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
  group_by(day_recorded, household_ID, hh_start) %>% #user_id and user_start are redundant for grouping but make sure this data appears in the created dataframe
  #note: because we want to keep more variables than the ones we group by, we use mutate, select, distinct
  mutate(
    rcpt_C02_ave = sum(item_footprint_g_100g * item_weight_g * item_units) / sum(item_weight_g * item_units),
    rcpt_C02_total = sum(item_footprint_g_100g * item_weight_g * item_units),
    rcpt_weight = sum(item_weight_g * item_units),
    rcpt_items_n = n()
  ) %>%
  select(user_ID:rcpt_items_n) %>% 
  distinct(day_recorded, household_ID, hh_start, .keep_all = T) # Note: Because both participants of household 9 provided receipts for the pre-loaded data we have to specify the variables in `distinct()`. Otherwise we get two rows for the first receipt of household 9 which messes up our analyses.

## add a sequence number for each receipt per person
rcpt_data_v2 %<>% group_by(household_ID) %>% mutate(hh_receipt_id = row_number(), .after = hh_start)

# export
rcpt_data_v2 %>% write_csv(here("receipt analysis","data","processed","receipt_level_data.csv"))
