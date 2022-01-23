library(targets)
library(tarchetypes)

# Source R files
lapply(list.files("./R", full.names = TRUE), source)

# Set target-specific options such as packages.
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  "tidyverse", "data.table", "tidymodels", "xgboost", "tidytext"
))

# Define targets
list(
  ## RUN PIPELINE WITH AGGREGATED, HAND-CODED DATA SET, INCLUDING NCES AND FB VARIABLES
  tar_target(sampled_aggregated_posts_f, here::here('rds', '400-aggregated-posts.rds'), format='file'),
  tar_target(account_posts_hashmap_f, here::here('rds', 'accounts-n_posts-hashmap.rds'), format='file'), 

  tar_target(sampled_aggregated_posts, readRDS(sampled_aggregated_posts_f)),
  tar_target(account_posts_hashmap, readRDS(account_posts_hashmap_f)),
  
  tar_target(aggregated_handcoded_data, add_n_posts_hashmap(sampled_aggregated_posts, account_posts_hashmap)),
  tar_target(cleaned_handcoded_data, clean_data(aggregated_handcoded_data)),
  
  # https://educationdata.urban.org/documentation/index.html#summary_endpoints
  tar_target(saipe_2018, educationdata::get_education_data(level = 'school-districts', 
                                                           source = 'saipe', 
                                                           filters = list(year = 2018))),
  tar_target(final_cleaned_sample, combine_clean_saipe(cleaned_handcoded_data, saipe_2018)),
  
  # Inspection and pre-processing ----------------------------------------------------
  
  # # DESCRIBE DATA DESCRIPTIVELY THROUGH NCES VARIABLES
  tar_target(na_stats, final_cleaned_sample %>% map_dbl(~sum(is.na(.))/length(is.na(.)))),

  tar_target(binary_modeling_data, preprocess_sample(final_cleaned_sample)),
  # 
  # # Selecting Variables of Incremental Model Tests Through LASSO Grid Search ---------
  tar_target(baseline_model_gridsearched, make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data)),
  # 
  # # Sampling variables that tested useful after grid search, i.e. urban centric locale and agency type
  tar_target(binary_modeling_data_gridsearched, preprocess_sample_gridsearched(final_cleaned_sample)),
  # 
  # # AUC on reduced variable set for additive and interaction model
  tar_target(additive_model_gridsearched, make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data_gridsearched, remove_vars_of_interest=FALSE)),
  tar_target(interaction_model_gridsearched, make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data_gridsearched, remove_vars_of_interest=FALSE,  include_interaction=TRUE)),
  # 
  # # Modeling for Inference and Model Comparisons --------------------------------------------------------
  tar_target(exported_models, export_glm_models_for_inference(binary_modeling_data_gridsearched)),
  tar_target(conventional_model_comparison, compare_models(exported_models)),
  # 
  # #### Modeling of whether ANY student was depicted in the image (identifiable or not) ------------------------------
  tar_target(binary_modeling_data_any_student_face, preprocess_sample_any_student_face(final_cleaned_sample)),
  # 
  # # Selecting Variables of Incremental Model Tests Through LASSO Grid Search ---------
  tar_target(baseline_model_gridsearched_any_student_face, 
              make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data_any_student_face)),
  # 
  # # Sampling variables that tested useful after grid search
  tar_target(binary_modeling_data_gridsearched_any_student_face, 
             preprocess_sample_gridsearched_any_student_face(final_cleaned_sample)),
  # 
  # # AUC on reduced variable set for additive and interaction model
  tar_target(additive_model_gridsearched_any_student_face, make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data_gridsearched_any_student_face, remove_vars_of_interest=FALSE)),
  tar_target(interaction_model_gridsearched_any_student_face, make_faces_connected_binary_recipe_lasso_baseline(binary_modeling_data_gridsearched_any_student_face, remove_vars_of_interest=FALSE,  include_interaction=TRUE)),
  # 
  # # Modeling for Inference and Model Comparisons --------------------------------------------------------
  tar_target(exported_models_any_student_face, export_glm_models_for_inference(binary_modeling_data_gridsearched_any_student_face, reference='any_student_face')),
  tar_target(conventional_model_comparison_any_student_face, compare_models(exported_models_any_student_face)),
  
  # Addendum for RQ1 -------------------------------------------------------------------------------------------------
   tar_target(get_model_intercept_face_connected, get_intercept_for_extrapolation(exported_models)),
  tar_target(get_model_intercept_face_in_image, get_intercept_for_extrapolation(exported_models_any_student_face)),
  
  # Addendum for RQ2 -------------------------------------------------------------------------------------------------
  tar_target(direct_model_comparison_identifiable_students, compare_null_to_interaction_model(exported_models))
  
  # EQUATIOMATIC
  #equatiomatic::extract_eq(targets::tar_read(exported_models)$m4)
  #equatiomatic::extract_eq(targets::tar_read(exported_models_any_student_face)$m4)
  
  # Average marginal effects
  # library(margins)
  # ms <- targets::tar_read(exported_models_any_student_face)
  # margins(ms$m0) %>% round(2)
  
  # Log Odds
  #coef(ms$m0) %>% round(2)
  
)

