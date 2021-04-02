get_unique_ev <- function(event_data) {
  
  un_events <- data.frame(event_type = unique(event_data$event)) %>% 
    mutate_if(grepl('*Software Carpentry.*',., ignore.case=T), ~replace(., grepl('Software Carpentry.*', .), "Software Carpentry")) %>% 
    mutate_if(grepl('Data Carpentry',., ignore.case=T), ~replace(., grepl('Data Carpentry.*', .), "Data Carpentry")) %>% 
    mutate_if(grepl('NL-RSE',., ignore.case=T), ~replace(., grepl('NL-RSE.*', .), "NL-RSE")) %>% 
    mutate_if(grepl('CodeRefinery',., ignore.case=T),  ~replace(., grepl('CodeRefinery.*', .), "CodeRefinery")) %>% 
    mutate_if(grepl('Parallel Programming in Python',., ignore.case=T), ~replace(., grepl('Parallel Programming in Python.*', .), "Parallel Programming in Python")) %>% 
    mutate_if(grepl('eScience Symposium',., ignore.case=T), ~replace(., grepl('eScience Symposium.*', .), "eScience Symposium")) %>% 
    mutate(event = unique(event_data$event)) %>% 
    mutate(level = )

}