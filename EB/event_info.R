event_info <- function(evURL) {
  
  evJSON <- GET(evURL)
  event_info <- data.frame(fromJSON(rawToChar(evJSON$content))) %>% 
    select(events.organizer_id, events.venue_id, events.id, events.resource_uri, events.name, events.start) %>% 
    rename(organizer_id=events.organizer_id, venue_id = events.venue_id,
           event_id = events.id, uri = events.resource_uri, event = events.name, eventdate=events.start) %>% 
    mutate(event=event$text) %>% 
    mutate(event_date = eventdate$local) %>% 
    mutate(org_id =case_when(organizer_id == "18671463704" ~ "ePlan",
                             organizer_id == "27675087963" ~ "LCRDM",
                             organizer_id == "8536296706" ~ "NLeSc",
                             organizer_id == "11806205959" ~ "NLeSc+LeidenHistoryInst",
                             organizer_id == "29275724533" ~ "NL-RSE")) %>%  #make sure IDs make sense to us (had to convert manually from EB)
    select(event_id, uri, org_id, venue_id, event, event_date) %>% 
    mutate(event_type = get_unique_events(event)) %>% 
    mutate(event_level = get_event_level(event_type))
    
 # event_info$event_level <- get_event_level(event_info)
  
  return(event_info)
  
}