##### SAMPLED POSTS #####

clean_hand_coded_sample <- function(d) {
  
  d <- d %>% 
    janitor::clean_names() %>% 
    filter(str_detect(url, '^http')) %>%  # 2 rows had 'current totals'
    filter(!is.na(shared_post)) %>% # sample those posts that currently are coded
    select(
      url, ends_with('_highlight'),
      matches('first_names'), 
      matches('last_names'),
      matches('faces_in_image|faces_in_post'),
      matches('faces_connected')
    ) %>% 
    rename(
      faces_connected_students = how_many_names_faces_connected,
      faces_connected_staff = how_many_names_faces_connected_1,
      faces_connected_community = how_many_names_faces_connected_2,
      number_of_community_faces_in_image = number_of_community_faces_in_post
    )
    
  return(d)
}

select_subset <- function(d, sample) {
  
  subset <- d[d$post_url %in% sample$url,]
  
  res <- subset %>% 
    rename(url = post_url) %>% 
    left_join(sample, by = 'url')
  
  return(res)
}

##### MODELING #####

read_and_clean_names_txt <- function(f) {
  
  d <- read_csv(f, col_names = FALSE) %>% 
    (function(x){names(x)<-c('name');return(x)}) %>% 
    mutate(name=tolower(name))
  
  return(d)
}

create_text_features <- function(d, names_df) {
  
  res1 <- d %>% 
    unnest_tokens(word, description) %>% 
    filter(word %in% c(names_df$name)) %>% 
    count(post_url) %>% 
    rename(n_names = n) %>% 
    mutate(has_names = TRUE) %>%  # counts only if > 1, rest will be full join completed
    full_join(d %>% select(post_url), by='post_url') %>% 
    replace_na(list(n_names=0, has_names=FALSE))
  
  res2 <- d %>% 
    unnest_tokens(word, description) %>% 
    filter(word %in% c('student', 'students', 'kid', 'kids', 'child', 'children')) %>% 
    count(post_url) %>% 
    rename(n_student_words = n) %>% 
    mutate(has_student_words = TRUE) %>%  # counts only if > 1, rest will be full join completed
    full_join(d %>% select(post_url), by='post_url') %>% 
    replace_na(list(n_student_words=0, has_student_words=FALSE))
  
  return(
    res1 %>% 
      inner_join(res2, by='post_url')
  )
}

get_n_posts_by_account <- function(dat) {
  
  true_dist_posts <- dat %>% 
    filter(is_district) %>% 
    group_by(nces_id_district) %>% 
    summarise(true_district_posts = n()) %>% 
    ungroup()
  
  dist_posts <- dat %>% 
    group_by(nces_id_district) %>% 
    summarise(district_posts = n()) %>% 
    ungroup()
  
  res <- full_join(true_dist_posts, dist_posts, by='nces_id_district')
  
  return(res)
}

add_n_posts_hashmap <- function(d, n_posts_hashmap) {
  
  d <- d %>% 
    mutate(year_of_post = substr(date, 1, 4) %>% as.numeric())
  
  join_this <- n_posts_hashmap$account_map %>% 
    select(facebook_id, n_posts_account) 
  
  d <- d %>% left_join(join_this, by='facebook_id')
  
  return(d)
  
}

combine_clean_saipe <- function(clean, saipe) {
  
  # For multiple "district ID" cases, take average
  saipe <- saipe %>% 
    group_by(district_id) %>% 
    summarise(est_population_5_17_poverty_pct = 
                mean(est_population_5_17_poverty_pct)) %>% 
    ungroup() %>% 
    select(district_id, est_population_5_17_poverty_pct) %>% 
    rename(nces_id_district = district_id)
  
  #clean$case <- 1:nrow(clean)
  clean$nces_id_district_full <- clean$nces_id_district
  clean$nces_id_district <- clean$nces_id_district %>% 
    substrRight(5)
  
  d <- clean %>% 
    left_join(saipe, by='nces_id_district')
  
  return(d)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

preselect_vars <- function(d) {
  
  d <- d %>% 
    select(-matches('^schools')) %>% # remove NCES school variables
    #select(-matches('staff'), -matches('community')) %>%  # remove target vars pertaining to staff and community
    select(-matches('highlight')) %>% 
    select(-number_of_student_first_names_in_post, -number_of_student_last_names_in_post)#, -number_of_student_faces_in_image)
  
  return(d)
}

clean_data <- function(d, n_posts_hashmap) {
  
  d <- d %>% 
    preselect_vars() %>% 
    clean_target_variables() %>% 
    factor_variables_for_modeling() %>% 
    final_touches_cleaning()
  
  return(d)
}

final_touches_cleaning <- function(d) {
  
  d <- d %>%  
    rename(face_connected = n_faces_connected_students_binary) %>% 
    mutate_if(is.integer, as.numeric) %>% 
    rowwise() %>% 
    mutate(interaction_count = sum(like_count, share_count, love_count, wow_count, haha_count, care_count)) %>% 
    mutate(neg_interaction_count = sum(angry_count, sad_count)) %>% 
    ungroup() %>% 
    mutate(did_not_report_district_nces_data = is.na(districts_n_students))
  
  # Collapse d$districts_urban_centric_locale_district_2017_18
  d$districts_urban_centric_locale_district_2017_18 <- 
    d$districts_urban_centric_locale_district_2017_18 %>% 
    as.character() %>% 
    substr(1, 1)
  
  d <- d %>% 
    mutate(districts_urban_centric_locale_district_2017_18 = 
             case_when(
               districts_urban_centric_locale_district_2017_18 == '1' ~ 'City',
               districts_urban_centric_locale_district_2017_18 == '2' ~ 'Suburb',
               districts_urban_centric_locale_district_2017_18 == '3' ~ 'Town',
               districts_urban_centric_locale_district_2017_18 == '4' ~ 'Rural',
               TRUE ~ districts_urban_centric_locale_district_2017_18
             )
    )
  
  # Factor with reference Suburb, as it is the most common type among cases
  d$districts_urban_centric_locale_district_2017_18 <- 
    d$districts_urban_centric_locale_district_2017_18 %>% 
    factor() %>% 
    fct_infreq()
  
  # Collapse agency type d$districts_agency_type_district_2017_18
  d <- d %>%
    mutate(districts_agency_type_district_2017_18 = 
             as.character(districts_agency_type_district_2017_18)) %>% 
    mutate(districts_agency_type_district_2017_18 = 
             case_when(
               districts_agency_type_district_2017_18 == 
                 '1-Regular local school district that is NOT a component of a supervisory union' ~ 'Non-Charter',
               districts_agency_type_district_2017_18 == 
                 '2-Local school district that is a component of a supervisory union' ~ 'Non-Charter',
               districts_agency_type_district_2017_18 == 
                 '7-Independent Charter District' ~ 'Charter',
               TRUE ~ districts_agency_type_district_2017_18
             )
    )
  
  # Factor with reference Non-Charter, as it is the most common type among cases
  d$districts_agency_type_district_2017_18 <- 
    d$districts_agency_type_district_2017_18 %>% 
    factor() %>% 
    fct_infreq()
  
  # Spot checking NA for race variables indicates that districts either reported 0 or missings for ethnic groups, 0 will be imputed
  # targets::tar_read(final_cleaned_sample) %>% 
  #  select(nces_id_district_full, starts_with('districts_n_students')) %>% 
  #   arrange(districts_n_students) %>% 
  #  (function(d){names(d)<-names(d) %>% str_remove('districts_n_students');return(d)})
  
  d$districts_n_students_native[is.na(d$districts_n_students_native)] <- 0
  d$districts_n_students_asian[is.na(d$districts_n_students_asian)] <- 0
  d$districts_n_students_hispanic[is.na(d$districts_n_students_hispanic)] <- 0
  d$districts_n_students_black[is.na(d$districts_n_students_black)] <- 0
  d$districts_n_students_white[is.na(d$districts_n_students_white)] <- 0
  d$districts_n_students_hawaiian[is.na(d$districts_n_students_hawaiian)] <- 0
  d$districts_n_students_multirace[is.na(d$districts_n_students_multirace)] <- 0
  
  return(d)
}


select_features <- function(d, target_var='') {
  
  # targets::tar_read(sampled_allvars)
  # map(d, ~mean(is.na(.)))
  
  if (target_var != '') {
    d <- d %>% select(-face_connected)
    names(d)[names(d) == target_var] <- 'face_connected'
  }
  
  d <- d %>% 
    mutate(year_of_post = substr(date, 1, 4) %>% as.numeric())
  
  d2 <- d %>% 
    select(
      interaction_count,
      # like_count, # POST LEVEL vars, to be discussed
      # share_count, 
      # comment_count, 
      # love_count,
      # wow_count,
      # haha_count,
      # care_count,
      account_subscriber_count,
      #nces_id_district, # ID
      is_school, # whether it's a school rather than a district account
      districts_n_schools, 
      districts_n_students,
      #districts_n_students_white,
      #districts_n_students_black,
      #districts_n_students_hispanic,
      #districts_n_students_multirace,
      #districts_pupil_teacher_ratio,
      #districts_n_students_free_reduced_lunch, # drops an additional 24 cases due to missing values
      n_posts_account,
      #districts_agency_type_district_2017_18,
      #districts_urban_centric_locale_district_2017_18,
      year_of_post,
      #n_names,
      #has_names,
      #n_student_words,
      #has_student_words,
      #n_student_faces_binary, # target 1, renamed to face connected to recycle functions
      face_connected # target 2
    )
  
  return(d2)
}

select_complete_cases <- function(d) {
  
  # sum(complete.cases(d)) 
  # discuss: imputation
  
  d2 <- d[complete.cases(d),]
  
  return(d2)
}

scale_and_create_features <- function(d) {
  
  d <- d %>% 
    mutate(
      districts_students_per_school = districts_n_students / districts_n_schools#,
      #districts_ratio_white_students = districts_n_students_white / districts_n_students,
      #districts_ratio_black_students = districts_n_students_black / districts_n_students,
      #districts_ratio_hispanic_students = districts_n_students_hispanic / districts_n_students,
      #districts_ratio_multirace_students = districts_n_students_multirace / districts_n_students,
      #districts_posts_per_student = districts_n_students / districts_n_posts
     )# %>% 
    #select( # drop variables that are now redundant, but not size of district (i.e. n schools and n students)
    #  -districts_n_students_white, -districts_n_students_black
    #)
  
  return(d)
}

clean_target_variables <- function(d) {
  
   d <- d %>% 
    mutate(number_of_student_faces_in_image = case_when(
      number_of_student_faces_in_image == 'at least 1' ~ '1',
      number_of_student_faces_in_image == 'maybe 1?' ~ '1',
      number_of_student_faces_in_image == '?' ~ '0',
      number_of_student_faces_in_image == '40+' ~ '40',
      TRUE ~ number_of_student_faces_in_image
    )) %>% 
    mutate(number_of_student_faces_in_image = as.integer(number_of_student_faces_in_image))
  
   # binary split for classification task, 1 if there is at least 1, else 0
   
   d <- d %>% 
     mutate(n_student_faces_binary = case_when(
       number_of_student_faces_in_image > 0 ~ 1,
       TRUE ~ 0
     )) %>% 
     mutate(n_faces_connected_students_binary = case_when(
       faces_connected_students > 0 ~ 1,
       TRUE ~ 0
     ))
  
  return(d)
}

factor_variables_for_modeling <- function(d) {
  
  d <- d %>% 
    mutate(n_student_faces_binary = factor(n_student_faces_binary)) %>% 
    mutate(n_faces_connected_students_binary = factor(n_faces_connected_students_binary)) %>% 
    #mutate(districts_agency_type_district_2017_18 = factor(districts_agency_type_district_2017_18)) %>% 
    #mutate(districts_urban_centric_locale_district_2017_18 = factor(districts_urban_centric_locale_district_2017_18)) %>% 
    mutate(is_school = factor(is_school)) %>% 
    mutate(year_of_post = factor(year_of_post))
  
  return(d)
}

final_touches_preproc <- function(d) {
  
  d <- d %>%  
    mutate(year_of_post = year_of_post %>% as.character() %>% as.numeric() %>% scale() %>% c()) %>% 
    mutate_if(is.integer, as.numeric) #%>% # created interaction count in previous step
    # rowwise() %>% 
    # mutate(interaction_count = sum(like_count, share_count, comment_count, love_count, wow_count, haha_count, care_count)) %>% 
    # ungroup() %>% 
    # select(-like_count, -share_count, -comment_count, -love_count, -wow_count, -haha_count, -care_count)
  
  return(d)
}

preprocess_sample <- function(d) {
  
  d <- d %>% 
    select_features() %>% 
    select_complete_cases() %>% 
    scale_and_create_features() %>% 
    final_touches_preproc()
  
  return(d)
}

make_faces_connected_binary_recipe_lasso_baseline <- function(d, remove_vars_of_interest=TRUE, include_interactions=FALSE) {
  
  d2 <- d %>% 
    dplyr::select(-matches('face_in_image'), 
                  -matches('number_of_student_faces_in_image'), 
                  -matches('faces_connected_students')) # remove other outcome since it is not independent of the other 
    
  if (remove_vars_of_interest) 
    d2 <- d2 %>% dplyr::select(-interaction_count, -account_subscriber_count, -year_of_post) # remove variable of interest for baseline model
    
  set.seed(123)
  
  data_split <- initial_split(d2, prop = 3/4, strata = face_connected) # 25% into test set
  
  # Create data frames for the two sets:
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # recipe, start with predicting whether 1 image has student face in it or 0 if not
  if (!include_interactions) {
    rec <- recipe(face_connected ~ ., data = train_data) %>%  # interactions do not seems to improve grid search for maximizing AUC value
      #step_smote(is_school, over_ratio = 1) %>% 
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
      (function(x){return(x)}) # for commenting out lines
  } else {
    rec <- recipe(face_connected ~ ., data = train_data) %>%  # interactions do not seems to improve grid search for maximizing AUC value
      #step_smote(is_school, over_ratio = 1) %>% 
      step_interact(terms = ~ year_of_post:interaction_count) %>% 
      step_interact(terms = ~ year_of_post:account_subscriber_count) %>% 
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
      (function(x){return(x)}) # for commenting out lines    
  }
  
  wf <- workflow() %>%
    add_recipe(rec)
  
  # Hyperparameters
  set.seed(1234)
  boots <- bootstraps(train_data, strata = face_connected)
  
  tune_spec <- logistic_reg(mode = "classification", penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")
  
  lambda_grid <- grid_regular(penalty(), levels = 5000) # fitting takes not as long as XGB
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  lasso_grid <- tune_grid(
    wf %>% add_model(tune_spec),
    resamples = boots,
    grid = lambda_grid
  )
  
  best_hyperparameters <- lasso_grid %>%
    select_best("roc_auc")
  
  final_lasso <- finalize_workflow(
    wf %>% add_model(tune_spec),
    best_hyperparameters
  )
  
  mfit <- last_fit(
    final_lasso,
    data_split
  )
  
  metrics <- mfit %>% 
    collect_metrics()
  
  estimates <- finalize_workflow(
    wf %>% add_model(tune_spec),
    best_hyperparameters
  ) %>% 
    fit(train_data) %>% 
    pull_workflow_fit() %>% 
    tidy() %>% 
    arrange(desc(estimate))
  
  selected_predictors <- estimates %>% 
    filter(estimate > 0) %>% 
    pull(term)
  
  return(    
    list(
      model = mfit,
      hyperparameters = best_hyperparameters,
      accuracy = metrics,
      selected_predictors = selected_predictors
    )
  )
}


glm_faces_connected_incremental_testing_without_test_set <- function(d) {
  
  d2 <- d %>% 
    dplyr::select(-face_in_image, -number_of_student_faces_in_image, -faces_connected_students) # remove other outcome since it is not independent of the other 
  
  m0 <- glm(face_connected ~ account_subscriber_count+interaction_count+districts_n_schools+districts_n_posts+
            districts_n_students+districts_students_per_school+is_school, d2, family='binomial')
  # additive model
  m1 <- glm(face_connected ~ account_subscriber_count+interaction_count+districts_n_schools+districts_n_posts+
              districts_n_students+districts_students_per_school+is_school+
              year_of_post+interaction_count+account_subscriber_count, d2, family='binomial')
  
  m2 <- glm(face_connected ~ account_subscriber_count+interaction_count+districts_n_schools+districts_n_posts+
              districts_n_students+districts_students_per_school+is_school+
              year_of_post*interaction_count+year_of_post*account_subscriber_count, d2, family='binomial')
  
  model_table <- anova(m0, m1, m2, test = "Chisq")
  AIC_values <- c(summary(m0)$aic, summary(m1)$aic, summary(m2)$aic)
  
  return(list(model_table=model_table, AIC_values = AIC_values))
}

get_engagement_intercorrelations <- function(dat) {
  
  res <- dat %>% 
    select(ends_with('_count')) %>% 
    select(-account_subscriber_count) %>% 
    drop_na() %>% 
    cor() %>% 
    round(2)
  
  return(res)
}


fit_poisson_model_identifiable_students <- function(d) {
  
  #d$year <- d$year_of_post %>% as.character() %>% as.numeric() - 2014 # year in years since 2014, beginning of hand coded sample
  m <- glm(faces_connected_students ~ year_of_post*interaction_count + year_of_post*account_subscriber_count, family = 'poisson', d)
  
  return(m)
}

extrapolate_poisson_model_identifiable_students <- function(d, m) {
  
  d2 <- d %>% 
    mutate(year_of_post = substr(date, 1, 4) %>% as.numeric()) %>% 
    select(-date) %>% 
    mutate_if(is.integer, as.numeric) %>% 
    rowwise() %>% 
    mutate(interaction_count = sum(like_count, share_count, comment_count, love_count, wow_count, haha_count, care_count)) %>% 
    ungroup() %>% 
    select(-like_count, -share_count, -comment_count, -love_count, -wow_count, -haha_count, -care_count)
  
  d2 <- d2 %>% mutate(year_of_post = factor(year_of_post))
  
  # Reference https://stackoverflow.com/questions/40985366/prediction-of-poisson-regression
  # EXAMPLE
  # newdat <- tribble( # year 0 is 2014, year 1 is 2015, ... and so on
  #   ~year, ~account_subscriber_count, ~interaction_count,
  #   0, 200, 31,
  #   3, 9000, 1111,
  #   6, 7, 1,
  #   6, 2, 1,
  #   5, 333, 11231,
  #   2, 1, 2
  # )
  # 
  # set.seed(1000)
  # 
  # n <- 1000
  # newdat <- tibble(
  #   year = sample(0:6, n, replace=TRUE),
  #   account_subscriber_count = sample(0:10000, n, replace=TRUE),
  #   interaction_count = sample(0:1000, n, replace=TRUE)
  #   )
  # 
  # ref <- newdat
  
  # drop new factor levels, drop around 7.4%
  #sum(d2$year_of_post %in% c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) / nrow(d2)
  
  d3 <- d2[!(d2$year_of_post %in% c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)),]
  
  ginv <- m$family$linkinv  ## inverse link function
  prs <- predict(m, newdata = d3, type = "link", se.fit=TRUE)
  
  newdat <- data.frame(NULL)
  
  newdat$pred <- ginv(prs[[1]]) # is the same as specifying type='response' in predict()
  newdat$lo <- ginv(prs[[1]] - 1.96 * prs[[2]])
  newdat$up <- ginv(prs[[1]] + 1.96 * prs[[2]])
  
  return(newdat)
}

preprocess_sample_gridsearched_any_student_face <- function(d, include_frl=FALSE) {
  
  d <- d %>% 
    select_gridsearched_predictors_any_student_face(target_var = 'n_student_faces_binary', include_frl=include_frl) %>% 
    select_complete_cases() %>% 
    scale_numeric_features() %>% 
    final_touches_preproc()
  
  return(d)
}

scale_numeric_features <- function(d) {
  
  d <- d %>% 
    mutate_if(is.numeric, function(x){x %>% scale() %>% c()})
  
  return(d)
}

select_gridsearched_predictors_any_student_face <- function(d, target_var = 'n_student_faces_binary', include_frl=FALSE) {
  
  if (target_var != '') {
    d <- d %>% select(-face_connected)
    names(d)[names(d) == target_var] <- 'face_connected'
  }
  
  # GRIDSEARCH RESULTS 
  #$selected_predictors
  #[1] "n_posts_account"                                      "districts_urban_centric_locale_district_2017_18_Town"
  #[3] "est_population_5_17_poverty_pct"    
  
  d <- d %>% 
    select(
      districts_urban_centric_locale_district_2017_18,
      est_population_5_17_poverty_pct,
      n_posts_account,
      # Engagement Vars and Year of Post, Variables of Interest
      interaction_count,
      comment_count,
      account_subscriber_count,
      year_of_post,
      # Target variable
      face_connected
    ) %>%
    rename(
      locale = districts_urban_centric_locale_district_2017_18,
      saipe_poverty_population_percent = est_population_5_17_poverty_pct
    )
  
  if (!include_frl){
    d <- d %>% select(-matches('districts_n_students_free_reduced_lunch'))
  }
  
  return(d)
}

preprocess_sample_gridsearched <- function(d) {
  
  d <- d %>% 
    select_gridsearched_predictors() %>% 
    select_complete_cases() %>% 
    scale_numeric_features() %>% 
    final_touches_preproc()
  
  return(d)
}

select_gridsearched_predictors <- function(d, target_var = '') {
  
  if (target_var != '') {
    d <- d %>% select(-face_connected)
    names(d)[names(d) == target_var] <- 'face_connected'
  }
  
  d <- d %>% 
    select(
      districts_urban_centric_locale_district_2017_18,
      districts_agency_type_district_2017_18,
      # Engagement Vars and Year of Post, Variables of Interest
      interaction_count,
      comment_count,
      account_subscriber_count,
      year_of_post,
      # Target variable
      face_connected
    ) %>% 
    rename(
      locale = districts_urban_centric_locale_district_2017_18,
      agency_type = districts_agency_type_district_2017_18
    )# %>% 
  # mutate(is_large_suburb = case_when(
  #   districts_urban_centric_locale_district_2017_18 == '21-Suburb: Large' ~ TRUE,
  #   is.na(districts_urban_centric_locale_district_2017_18) ~ NA,
  #   TRUE ~ FALSE
  # )) %>% 
  # mutate(is_large_suburb = factor(is_large_suburb)) %>% 
  # select(-districts_urban_centric_locale_district_2017_18)
  
  return(d)
}

preprocess_sample_any_student_face <- function(d) {
  
  d <- d %>% 
    select_features(target_var = 'n_student_faces_binary') %>% 
    select_complete_cases() %>% 
    scale_numeric_features() %>% 
    final_touches_preproc()
  
  return(d)
}

export_glm_models_for_inference <- function(d, reference = 'main_analysis') {
  
  if (reference == 'main_analysis') {
    
    mnull <- glm(face_connected ~ 1, d, family='binomial')
    
    m0 <- glm(face_connected ~ locale + agency_type, d, family='binomial')
    
    m1 <- glm(face_connected ~ year_of_post+locale + agency_type, d, family='binomial')
    
    m2 <- glm(face_connected ~ account_subscriber_count+interaction_count+year_of_post+locale + agency_type, d, family='binomial')
    
    m3 <- glm(face_connected ~ account_subscriber_count+interaction_count+comment_count+year_of_post+locale + agency_type, d, family='binomial')
    
    m4 <- glm(face_connected ~ account_subscriber_count*year_of_post+
                interaction_count*year_of_post+
                comment_count*year_of_post+
                locale + agency_type, d, family='binomial')
  }
  else if (reference == 'any_connected_face') {
    
    mnull <- glm(face_connected ~ 1, d, family='binomial')
    
    m0 <- glm(face_connected ~ 1, d, family='binomial')
    
    m1 <- glm(face_connected ~ year_of_post, d, family='binomial')
    
    m2 <- glm(face_connected ~ account_subscriber_count+interaction_count+year_of_post, d, family='binomial')
    
    m3 <- glm(face_connected ~ account_subscriber_count+interaction_count+comment_count+year_of_post, d, family='binomial')
    
    m4 <- glm(face_connected ~ account_subscriber_count*year_of_post+
                interaction_count*year_of_post+
                comment_count*year_of_post, d, family='binomial')
  }
  
  else if (reference == 'any_student_face') {
    
    mnull <- glm(face_connected ~ 1, d, family='binomial')
    
    m0 <- glm(face_connected ~ 
                locale +
                n_posts_account + 
                saipe_poverty_population_percent, d, family='binomial')
    
    m1 <- glm(face_connected ~ locale +
                n_posts_account + 
                saipe_poverty_population_percent+ 
                year_of_post, d, family='binomial')
    
    m2 <- glm(face_connected ~ locale +
                n_posts_account + 
                saipe_poverty_population_percent+ 
                account_subscriber_count+interaction_count+year_of_post, d, family='binomial')
    
    m3 <- glm(face_connected ~ locale +
                n_posts_account + 
                saipe_poverty_population_percent + 
                account_subscriber_count+interaction_count+comment_count+year_of_post, d, family='binomial')
    
    m4 <- glm(face_connected ~ locale +
                n_posts_account + 
                saipe_poverty_population_percent + 
                account_subscriber_count*year_of_post+
                interaction_count*year_of_post+
                comment_count*year_of_post, d, family='binomial')
  }
  
  else if (reference == 'any_student_face_frl') {
    
    # is_rural_remote
    # is_town_remote
    # is_city_midsize
    # is_rural_fringe
    # is_town_distant
    # is_independent_charter_district
    # n_posts_account
    # is_school
    # districts_n_students_hispanic
    # districts_n_students_multirace
    # districts_n_schools
    # districts_n_students_free_reduced_lunch
    
    mnull <- glm(face_connected ~ 1, d, family='binomial')
    
    m0 <- glm(face_connected ~ is_rural_remote +
                is_town_remote + 
                is_city_midsize + 
                is_rural_fringe + 
                is_town_distant + 
                is_independent_charter_district + 
                n_posts_account + 
                is_school + 
                districts_n_students_hispanic + 
                districts_n_students_multirace + 
                districts_n_students_free_reduced_lunch + 
                districts_n_schools, d, family='binomial')
    
    m1 <- glm(face_connected ~ is_rural_remote +
                is_town_remote + 
                is_city_midsize + 
                is_rural_fringe + 
                is_town_distant + 
                is_independent_charter_district + 
                n_posts_account + 
                is_school + 
                districts_n_students_hispanic + 
                districts_n_students_multirace + 
                districts_n_schools + 
                districts_n_students_free_reduced_lunch + 
                year_of_post, d, family='binomial')
    
    m2 <- glm(face_connected ~ is_rural_remote +
                is_town_remote + 
                is_city_midsize + 
                is_rural_fringe + 
                is_town_distant + 
                is_independent_charter_district + 
                n_posts_account + 
                is_school + 
                districts_n_students_hispanic + 
                districts_n_students_multirace + 
                districts_n_schools + 
                districts_n_students_free_reduced_lunch +
                account_subscriber_count+interaction_count+year_of_post, d, family='binomial')
    
    m3 <- glm(face_connected ~ is_rural_remote +
                is_town_remote + 
                is_city_midsize + 
                is_rural_fringe + 
                is_town_distant + 
                is_independent_charter_district + 
                n_posts_account + 
                is_school + 
                districts_n_students_hispanic + 
                districts_n_students_multirace + 
                districts_n_schools +  
                districts_n_students_free_reduced_lunch + 
                account_subscriber_count+interaction_count+comment_count+year_of_post, d, family='binomial')
    
    m4 <- glm(face_connected ~ is_rural_remote +
                is_town_remote + 
                is_city_midsize + 
                is_rural_fringe + 
                is_town_distant + 
                is_independent_charter_district + 
                n_posts_account + 
                is_school + 
                districts_n_students_hispanic + 
                districts_n_students_multirace + 
                districts_n_schools + 
                districts_n_students_free_reduced_lunch + 
                account_subscriber_count*year_of_post+
                interaction_count*year_of_post+
                comment_count*year_of_post, d, family='binomial')
  }
  
  
  else {
    return(FALSE)
  }
  
  return(
    list(
      mnull=mnull, m0=m0, m1=m1, m2=m2, m3=m3, m4=m4
    )
  )
}

compare_models <- function(l) {
  
  model_table <- anova(l$mnull, l$m0, l$m1, l$m2, l$m3, l$m4, test = "Chisq")
  perf_table <- performance::compare_performance(l$mnull, l$m0, l$m1, l$m2, l$m3, l$m4)
  AIC_values <- c(summary(l$mnull)$aic, summary(l$m0)$aic, summary(l$m1)$aic, summary(l$m2)$aic,
                  summary(l$m3)$aic, summary(l$m4)$aic)
  AIC_weights <- perf_table$AIC_wt
  BIC_values <- c(BIC(l$mnull), BIC(l$m0), BIC(l$m1), BIC(l$m2), BIC(l$m3), BIC(l$m4))
  BIC_weights <- perf_table$BIC_wt
  
  return(list(model_table=model_table, AIC_values = AIC_values, BIC_values = BIC_values,
              AIC_weights=AIC_weights, BIC_weights=BIC_weights))
}

compare_null_to_interaction_model <- function(l) {
  
  model_table <- anova(l$mnull, l$m4, test = "Chisq")
  perf_table <- performance::compare_performance(l$mnull, l$m4)
  
  AIC_values <- c(summary(l$mnull)$aic, summary(l$m4)$aic)
  AIC_weights <- perf_table$AIC_wt
  BIC_values <- c(BIC(l$mnull), BIC(l$m4))
  BIC_weights <- perf_table$BIC_wt
  
  return(list(model_table=model_table, AIC_values = AIC_values, BIC_values = BIC_values,
              AIC_weights=AIC_weights, BIC_weights=BIC_weights))
}

get_intercept_for_extrapolation <- function(l) {

  m <- l$mnull
  
  intercept_log_odds <- m$coefficients %>% as.numeric()
  conf_lower_log_odds <- m %>% confint() %>% (function(v){return(v[1])}) %>% as.numeric()
  conf_upper_log_odds <- m %>% confint() %>% (function(v){return(v[2])}) %>% as.numeric()
  
  log_odds_to_percent <- function(log_odds){
    odds <- log_odds %>% exp()
    p <- odds/(odds+1)
    p <- p*100  # decimal to percent
    return(p)
  }
  
  return(
    list(
      intercept_log_odds = intercept_log_odds %>% round(2) %>% format(nsmall=2),
      conf_lower_log_odds = conf_lower_log_odds %>% round(2) %>% format(nsmall=2),
      conf_upper_log_odds = conf_upper_log_odds %>% round(2) %>% format(nsmall=2),
      intercept_p = intercept_log_odds %>% log_odds_to_percent() %>% round(2) %>% format(nsmall=2),
      conf_lower_p = conf_lower_log_odds %>% log_odds_to_percent() %>% round(2) %>% format(nsmall=2),
      conf_upper_p = conf_upper_log_odds %>% log_odds_to_percent() %>% round(2) %>% format(nsmall=2)
    )
  )
}
