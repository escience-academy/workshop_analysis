get_event_level <- function(event_data) {
  
  un_events <- data.frame(event_level = event_data) %>% 
    mutate_if(grepl('*Software Carpentry.*',., ignore.case=T), ~replace(., grepl('Software Carpentry.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('Data Carpentry',., ignore.case=T), ~replace(., grepl('Data Carpentry.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('NL-RSE',., ignore.case=T), ~replace(., grepl('NL-RSE.*', ., ignore.case=T), "Other")) %>% 
    mutate_if(grepl('CodeRefinery',., ignore.case=T),  ~replace(., grepl('CodeRefinery.*', .), "Advanced")) %>% 
    mutate_if(grepl('Parallel Programming in Python',., ignore.case=T), ~replace(., grepl('Parallel Programming in Python.*', ., ignore.case=T), "Advanced")) %>% 
    mutate_if(grepl('Containers',., ignore.case=T), ~replace(., grepl('Containers.*', ., ignore.case=T), "Advanced")) %>% 
    mutate_if(grepl('eScience Symposium',., ignore.case=T), ~replace(., grepl('eScience Symposium.*', ., ignore.case=T), "Other")) %>% 
    mutate_if(grepl('Introduction',., ignore.case=T), ~replace(., grepl('Introduction.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('Good practices',., ignore.case=T), ~replace(., grepl('Good practices.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('FAIR software workshop',., ignore.case=T), ~replace(., grepl('FAIR Software Workshop*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('Version Control and Collaboration with Git and GitHub',., ignore.case=T), ~replace(., grepl('Version.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('Research data handling',., ignore.case=T), ~replace(., grepl('Research data.*', ., ignore.case=T), "Essential")) %>% 
    mutate_if(grepl('Computational skills for the humanities',., ignore.case=T), ~replace(., grepl('Workshop.*', ., ignore.case=T), "Essential"))

      
  
  un_events$event_level <- if_else((un_events$event_level != "Essential") & (un_events$event_level != "Intermediate") & (un_events$event_level != "Advanced") , 
            "Other" , un_events$event_level)  
  # mutate(level = mutate_if(grepl()))
  
  return(un_events$event_level)
  
}