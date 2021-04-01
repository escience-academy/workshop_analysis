eSc_ppts_EB <- function(EBurl, req_names, token) {
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
  
  ppt_pers    <- ppt_all$profile %>% 
    select(name, email) 
  
  ppt_info <- ppt_all %>% 
    select(id, event_id, order_id, ticket_class_name, created) %>%
    mutate(name=ppt_pers$name, email=ppt_pers$email) %>% 
    rename(ticket_type=ticket_class_name) %>% 
    mutate(id=ppt_all$id)
     
  answers <- get_answers(ppt_all, ppt_info)
  
  add_after <- req_names[!is.element(req_names,names(answers))]
  answers[,add_after]=NA
  
  return(answers)
}

#for (ev in 39:39){#length(event_info$uri)) {



#}

#fromJSON(rawToChar(Q_A$content))$attendees

