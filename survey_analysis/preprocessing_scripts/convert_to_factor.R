# convert to factors surveyT1 ------------------------------------------------

##convert likert scale answers to factors
levels_statements_1 <- c("strongly disagree", "disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "agree", "strongly agree")
levels_age <- c("18-24 years old", "25-34 years old", "35-44 years old", "55-64 years old")
levels_sex <- c("Man","Woman","non-binary")
levels_degree <- 
  c(
    "High school graduate (high school diploma or equivalent including GED)",
    "Associate degree in college (2-year)",
    "Bachelor's degree in college (4-year)",
    "Master's degree",
    "Doctoral degree",
    "Professional degree (JD, MD)"
  )
levels_does_groceries <- c("I do","Another person in my household","We do most groceries collectivily")
levels_diet <- c("Omnivorous (no restrictions)", "Flexitarian", "Pescaterian", "Vegetarian", "Dairy free")
levels_share <- c("none/very little","some","about half","most","almost all")

if(exists("surveyT1")){
  surveyT1 %<>%
    mutate(
      across(c(env_attitude_1:SelfEff_3), ~ factor(str_to_lower(.x), levels = levels_statements_1))
    )
  
  surveyT1 %<>%
    mutate(
      age = factor(age, levels = levels_age),
      sex = factor(sex, levels = levels_sex),
      degree = factor(degree, levels = levels_degree),
      does_groceries = factor(does_groceries, levels = levels_does_groceries),
      diet = factor(diet, levels = levels_diet),
      across(share_store:`share_out-of-home`, ~ factor(str_to_lower(.x), levels = levels_share))
    )
}



# create factors surveyT2---------------------------------------------

##convert likert scale answers to factors
levels_statements_1 <- c("strongly disagree", "disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "agree", "strongly agree")
levels_statements_2 <- c("highly unlikely","unlikely","somewhat unlikely","neither likely nor unlikely","somewhat likely","likely","very likely")
levels_statements_3 <- c("Very little","Little","Somewhat little","Neither much nor little","Somewhat much","Much","Very much")
levels_UX_1 <- c("Very obstructive","Obstructive","Slightly obstructive","Neither supportive nor obstructive","Slightly supportive","Supportive","Very supportive")
levels_UX_2 <- c("Very complicated","Complicated","Slightly complicated","Neither easy nor complicated","Slightly easy","Easy","Very easy")
levels_UX_3 <- c("Very inefficient","Inefficient","Slightly inefficient","Neither efficient nor inefficient","Slightly efficient","Efficient","Very efficient")
levels_UX_4 <- c("Very confusing","Moderately confusing","Confusing","Neither clear nor confusing","Clear","Moderately clear","Very clear")
levels_UX_5 <- c("Very boring","Boring","Slightly boring","Neither exciting nor boring","Slightly exciting","Exciting","Very exciting")
levels_UX_6 <- c("Not interesting at all","Not interesting","Slightly not-interesting","Neither interesting nor not-interesting","Slightly interesting","Interesting","Very interesting")
levels_UX_7 <- c("Very conventional","Conventional","Slightly conventional","Neither inventive nor conventional","Slightly inventive","Inventive","Very inventive")
levels_UX_8 <- c("Very usual","Usual","Slightly usual","Neither leading edge nor usual","Slightly leading edge","Leading edge","Very leading edge")
levels_statements_4 <- c("Very difficult","Difficult","Slightly difficult","Neither easy nor difficult","Slightly easy","Easy","Very easy")

if(exists("surveyT2")){
  surveyT2 %<>%
    mutate(
      across(c(env_attitude_1:SelfEff_3, spillover_1:spillover_2, state_helped_goals:state_helped_change,state_use_future), ~ factor(str_to_lower(.x), levels = levels_statements_1))
    )
  surveyT2 %<>%
    mutate(
      across(intention_1, ~ factor(.x, levels = levels_statements_2))
    )
  surveyT2 %<>%
    mutate(
      across(intention_2, ~ factor(.x, levels = levels_statements_3)),
      across(UX_1_supportive, ~ factor(.x, levels = levels_UX_1)),
      across(UX_2_easy, ~ factor(.x, levels = levels_UX_2)),
      across(UX_3_efficient, ~ factor(.x, levels = levels_UX_3)),
      across(UX_4_clear, ~ factor(.x, levels = levels_UX_4)),
      across(UX_5_exciting, ~ factor(.x, levels = levels_UX_5)),
      across(UX_6_interesting, ~ factor(.x, levels = levels_UX_6)),
      across(UX_7_inventive, ~ factor(.x, levels = levels_UX_7)),
      across(UX_8_leading, ~ factor(.x, levels = levels_UX_8)),
      across(state_accept_alt, ~ factor(.x, levels = levels_statements_4)),
    )
}

if(exists("survey_data_all")){

  survey_data_all %<>%
    mutate(
      age = factor(age, levels = levels_age),
      sex = factor(sex, levels = levels_sex),
      degree = factor(degree, levels = levels_degree),
      does_groceries = factor(does_groceries, levels = levels_does_groceries),
      diet = factor(diet, levels = levels_diet),
      across(share_store:`share_out-of-home`, ~ factor(str_to_lower(.x), levels = levels_share))
    )
  
  survey_data_all %<>%
    mutate(
      across(c(env_attitude_1:SelfEff_3, spillover_1:spillover_2, state_helped_goals:state_helped_change,state_use_future), ~ factor(str_to_lower(.x), levels = levels_statements_1))
    )
  
  survey_data_all %<>%
    mutate(
      across(intention_1, ~ factor(.x, levels = levels_statements_2))
    )
  
  survey_data_all %<>%
    mutate(
      across(intention_2, ~ factor(.x, levels = levels_statements_3)),
      across(UX_1_supportive, ~ factor(.x, levels = levels_UX_1)),
      across(UX_2_easy, ~ factor(.x, levels = levels_UX_2)),
      across(UX_3_efficient, ~ factor(.x, levels = levels_UX_3)),
      across(UX_4_clear, ~ factor(.x, levels = levels_UX_4)),
      across(UX_5_exciting, ~ factor(.x, levels = levels_UX_5)),
      across(UX_6_interesting, ~ factor(.x, levels = levels_UX_6)),
      across(UX_7_inventive, ~ factor(.x, levels = levels_UX_7)),
      across(UX_8_leading, ~ factor(.x, levels = levels_UX_8)),
      across(state_accept_alt, ~ factor(.x, levels = levels_statements_4)),
    )
}


if(exists("survey_data_all_wide")){
  
  survey_data_all_wide %<>%
    mutate(
      age = factor(age, levels = levels_age),
      sex = factor(sex, levels = levels_sex),
      degree = factor(degree, levels = levels_degree),
      does_groceries = factor(does_groceries, levels = levels_does_groceries),
      diet = factor(diet, levels = levels_diet),
      across(share_store:`share_out-of-home`, ~ factor(str_to_lower(.x), levels = levels_share))
    )
  
  survey_data_all_wide %<>%
    mutate(
      across(c(env_attitude_1_T1:SelfEff_3_T2, spillover_1:spillover_2, state_helped_goals:state_helped_change,state_use_future), ~ factor(str_to_lower(.x), levels = levels_statements_1))
    )
  
  survey_data_all_wide %<>%
    mutate(
      across(intention_1, ~ factor(.x, levels = levels_statements_2))
    )
  
  survey_data_all_wide %<>%
    mutate(
      across(intention_2, ~ factor(.x, levels = levels_statements_3)),
      across(UX_1_supportive, ~ factor(.x, levels = levels_UX_1)),
      across(UX_2_easy, ~ factor(.x, levels = levels_UX_2)),
      across(UX_3_efficient, ~ factor(.x, levels = levels_UX_3)),
      across(UX_4_clear, ~ factor(.x, levels = levels_UX_4)),
      across(UX_5_exciting, ~ factor(.x, levels = levels_UX_5)),
      across(UX_6_interesting, ~ factor(.x, levels = levels_UX_6)),
      across(UX_7_inventive, ~ factor(.x, levels = levels_UX_7)),
      across(UX_8_leading, ~ factor(.x, levels = levels_UX_8)),
      across(state_accept_alt, ~ factor(.x, levels = levels_statements_4)),
    )
}
