#devtools::install_github("tntp/surveymonkey")
library(tidyverse)
library(surveymonkey)

exec_dir <- paste0(dirname(getwd()), '/EB')
tokens <- read.delim(paste0(exec_dir, "/tokens.txt"), header=F)
token <- str_split(tokens$V1, pattern=" ")[[2]][2] #token stuff for authentication

usethis::edit_r_profile()
options(sm_oauth_token = token)
#getOption("sm_oauth_token")

surveys <- browse_surveys(200)  # see your most recent 200 surveys

post_workshop <- surveys %>%
  filter(grepl('post', title)) %>%
  filter(!grepl('Copy of', title)) %>%
  select(title, id) #all post-workshop surveys and ids

post_workshop_ratings <- data.frame(id = NA,
             instr_answers = NA,
             instr_enthusiasm = NA,
             instr_interact_comfort = NA,
             instr_knowledge = NA,
             learningenv_comfort = NA,
             lesson_apply_immediately = NA,
             practical_setup = NA,
             practical_target_audience = NA,
             lesson_authentic_examples = NA,
             practical_org = NA)

pre_workshop <- surveys %>%
  filter(grepl('pre-workshop', title)) %>%
  filter(!grepl('Copy of', title)) %>%
  select(title, id) #all pre-workshop surveys and ids

post_survey_summ <- data.frame(
  id = post_workshop$id[i],
  instr_answers = NA,
  instr_enthusiasm = NA,
  instr_interact_comfort = NA,
  instr_knowledge = NA,
  learningenv_comfort = NA,
  lesson_apply_immediately = NA,
  practical_setup = NA,
  practical_target_audience = NA,
  lesson_authentic_examples = NA,
  practical_org = NA)

for (i in 1:dim(post_workshop)[1]) { #don't run this loop willy nilly, it spends surveymonkey requests
  post_survey_df <- post_workshop$id[i] %>%
    fetch_survey_obj %>%
    parse_survey

  result = tryCatch({
    post_survey_summ <- fetch_post_data(post_survey_df) %>%
      add_column(id = post_workshop$id[i], .before = "instr_answers")
  }, error = function(e) {
    print(paste("survey ", post_workshop$title[post_workshop$id==post_workshop$id[i]], "does not have the expected questions. Assigning NAs"))
    post_survey_summ <- data.frame(
      id = post_workshop$id[i],
      instr_answers = NA,
      instr_enthusiasm = NA,
      instr_interact_comfort = NA,
      instr_knowledge = NA,
      learningenv_comfort = NA,
      lesson_apply_immediately = NA,
      practical_setup = NA,
      practical_target_audience = NA,
      lesson_authentic_examples = NA,
      practical_org = NA)
  }, finally = {

  })
  post_workshop_ratings <- rbind(post_survey_summ, post_workshop_ratings)
}

post_workshop_ratings<-merge(post_workshop_ratings, post_workshop, by="id") %>%
  distinct()

colMeans(post_workshop_ratings[,2:10])
rowMeans(post_workshop_ratings[,2:10])



fetch_post_data <- function(post_survey_df) {

  post_survey_play <- post_survey_df %>%
    select(starts_with("How much")) %>%
    rename_with(~ gsub('[[:punct:]]', '', .x)) %>%
    set_names(~stringr::str_replace_all(., "How much do you agree with following statements", "")) %>%
    clean_names() %>%
    rename(instr_answers              = i_was_able_to_get_clear_answers_to_my_questions_from_the_instructors) %>%
    rename(instr_enthusiasm           = the_instructors_were_enthusiastic_about_the_workshop) %>%
    rename(instr_interact_comfort     = i_felt_comfortable_interacting_with_the_instructors) %>%
    rename(instr_knowledge            = the_instructors_were_knowledgeable_about_the_material_being_taught) %>%
    rename(learningenv_comfort        = i_felt_comfortable_learning_in_this_workshop_environment) %>%
    rename(lesson_apply_immediately   = i_can_immediately_apply_what_i_learned_at_this_workshop) %>%
    rename(practical_setup            = the_setup_and_installation_instructions_for_the_lesson_were_complete_and_easy_to_follow) %>%
    rename(practical_target_audience  = i_belong_to_the_target_audience_for_this_lesson) %>%
    rename(lesson_authentic_examples  = examples_and_tasks_in_the_lesson_were_relevant_and_authentic) %>%
    rename(practical_org              = the_practical_organization_of_the_workshop_was_good) %>%
    mutate(across(where(is.factor), ~ recode(.x, "Strongly disagree" = 1,
                                             "Disagree" = 2,
                                             "Undecided" = 3,
                                             "Agree" = 4,
                                             "Strongly Agree" = 5))) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = TRUE))
}

pre_survey_list <- vector(mode = "list")

for (i in 1:dim(pre_workshop)[1]) { #don't run this loop willy nilly, it spends surveymonkey requests
  pre_survey <- pre_workshop$id[i] %>%
      fetch_survey_obj %>%
      parse_survey

  pre_survey_list[[i]] <- pre_survey
  }



pre_survey_list <- pre_workshop$id[1] %>%
  fetch_survey_obj %>%
  parse_survey


all_pre_data<-bind_rows(pre_survey_list, .id = "column_label")
