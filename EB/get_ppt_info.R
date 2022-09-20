get_ppt_info <- function(EBurl, req_names, token) {
  #' Take an EventBrite URL and return a df with the information about participants and the event that they attended
  #' 
  #' Input: 
  #'         EBurl: An EB URL (string), 
  #'         event_info: a df with the information about the events
  #'         req_names: a vector of names that we want the dataframe to have
  #'         
  #' Ouput:
  #'         A dataframe with the column names in req_names that corresponds to the information in the url provided
  
  Q_A         <- GET(paste0(EBurl, 'attendees/', token))
  ppt_all     <- fromJSON(rawToChar(Q_A$content))$attendees 
  
  if (is_empty(ppt_all)) {
    
    answers <- data.frame(id=character(),
                          affiliation=character(), dis1=character(), dis2=character(), dis3=character(), dis4=character(),
                          dis5=character(), event_id=character(), order_id=character(), ticket_type=character(), created=character(),
                          name=character(), email=character(), ERCdis=character(), NLeScdis=character(), eSc_collab=character(),
                          car1=character(), car2=character(), git_quiz=character(),
                          stringsAsFactors=FALSE)
    
  } else {
    
    if ("name" %in% colnames(ppt_all$profile)) {   
      ppt_pers    <- ppt_all$profile %>% 
        select(name, email) 
      } else if ("first_name" %in% colnames(ppt_all$profile)) {
      ppt_pers    <- ppt_all$profile %>% 
        select(first_name, last_name, email) %>% 
        mutate(name = paste(first_name, last_name)) 
      } else {   
      ppt_pers    <- ppt_all$profile %>% 
        select(email)
    }
  
  ppt_info <- ppt_all %>% 
    select(id, event_id, order_id, ticket_class_name, created) %>%
    mutate(name=ppt_pers$name, email=ppt_pers$email) %>% 
    rename(ticket_type=ticket_class_name) %>% 
    mutate(id=ppt_all$id)
  
  answers <- get_answers(ppt_all, ppt_info)
  
  add_after <- req_names[!is.element(req_names,names(answers))]
  answers[,add_after]=NA
  }
  return(answers) #change back to return(answers)
}

#for (ev in 39:39){#length(event_info$uri)) {



#}

#fromJSON(rawToChar(Q_A$content))$attendees

