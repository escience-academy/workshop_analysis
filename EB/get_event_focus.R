get_event_focus <- function(event_data) {
  
  un_events <- data.frame(event_focus = event_data) %>% 
    mutate_if(grepl('*Software Carpentry.*',., ignore.case=T), ~replace(., grepl('Software Carpentry.*', ., ignore.case=T), "Open & Reproducible Research Software")) %>% 
    mutate_if(grepl('Data Carpentry',., ignore.case=T), ~replace(., grepl('Data Carpentry.*', ., ignore.case=T), "Domain Specific")) %>% 
    mutate_if(grepl('NL-RSE',., ignore.case=T), ~replace(., grepl('NL-RSE.*', ., ignore.case=T), "Other")) %>% 
    mutate_if(grepl('CodeRefinery',., ignore.case=T),  ~replace(., grepl('CodeRefinery.*', .), "Open & Reproducible Research Software")) %>% 
    mutate_if(grepl('Parallel Programming in Python',., ignore.case=T), ~replace(., grepl('Parallel Programming in Python.*', ., ignore.case=T), "Technology Specific")) %>% 
    mutate_if(grepl('Containers',., ignore.case=T), ~replace(., grepl('Containers.*', ., ignore.case=T), "Open & Reproducible Research Software")) %>% 
    mutate_if(grepl('eScience Symposium',., ignore.case=T), ~replace(., grepl('eScience Symposium.*', ., ignore.case=T), "Other")) %>% 
    mutate_if(grepl('Unix',., ignore.case=T), ~replace(., grepl('Unix.*', ., ignore.case=T), "Open & Reproducible Research Software")) %>% 
    mutate_if(grepl('GitHub',., ignore.case=T), ~replace(., grepl('GitHub.*', ., ignore.case=T), "Open & Reproducible Research Software")) 
    # mutate_if(grepl('Introduction',., ignore.case=T), ~replace(., grepl('Introduction.*', ., ignore.case=T), "Essential")) 
    
  un_events$event_focus <- if_else((un_events$event_focus != "Open & Reproducible Research Software") & (un_events$event_focus != "Domain Specific") & (un_events$event_focus != "Technology Specific") &
                                   (un_events$event_focus != "Other"), "Other" , un_events$event_focus)  
  # mutate(level = mutate_if(grepl()))
  
  return(un_events$event_focus)
  
}