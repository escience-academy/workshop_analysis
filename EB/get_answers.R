get_answers <- function(ppt_all, ppt_info) {
  #' Take information from an EventBrite JSON structure and returns the answers to the costum questions in the webpage it was taken from
  #' 
  #' Input: 
  #'         ppt_all: a list of all the attendees for a given workshop
  #'         ppt_info: a df with id, event_id, order_id, ticket_class_name, created, name, email
  #'         
  #' Ouput:
  #'         A dataframe with simplified answer column names and answers for each ppt
  
  ans_idx         <- unlist(lapply(ppt_all$answers, function(el) is.element("answer",(names(el)))))
  cols            <- c(ppt_info$id[ans_idx])
  answers_raw     <- data.frame(ppt_all$answers)
  
  if (is_empty(answers_raw)) {
    
    answers <- ppt_info
    
  } else {
    
    answers     <- answers_raw %>% 
      rename_at(vars(matches("^question$")), function(x) "answer_question") %>% 
      #mutate(answer_question = question) %>% 
      select(starts_with("answer")) %>% 
      #rename(question=answer_question) %>% 
      rename_at(vars(matches("answer_question")), function(x) "question") %>% 
      mutate_if(grepl('affiliation',.), ~replace(., grepl('affiliation.*', .), "affiliation")) %>% 
      mutate_if(grepl('Organization',.), ~replace(., grepl('Organization.*', .), "affiliation")) %>% 
      mutate_if(grepl('collaboration',.), ~replace(., grepl('collaboration.*', .), "eSc_collab")) %>% 
      mutate_if(grepl('disciplines',.), ~replace(., grepl('disciplines.*', .), "disciplines")) %>% 
      mutate_if(grepl('scientific domain',.), ~replace(., grepl('scientific domain.*', .), "disciplines")) %>% 
      mutate_if(grepl('Discipline',.), ~replace(., grepl('Discipline.*', .), "disciplines")) %>% 
      mutate_if(grepl('career stage',.), ~replace(., grepl('career stage.*', .), "career_stage")) %>% 
      mutate_if(grepl('Git Quiz',.), ~replace(., grepl('Git Quiz.*', .), "git_quiz")) %>% 
      remove_rownames() %>% 
      column_to_rownames("question") %>%  
      rownames_to_column() %>%
      pivot_longer(-rowname, 'id', 'value') %>%
      pivot_wider(id, rowname) %>% 
      mutate(id=cols) %>% 
      left_join(ppt_info, by="id")
    
    if (is.element("disciplines", names(answers))) {
      answers <- answers %>% 
        separate(disciplines,sep=" [|] ",c("dis1", "dis2", "dis3", "dis4", "dis5"), 
                 extra="drop", fill="right")
    }
  }
  return(answers)
}