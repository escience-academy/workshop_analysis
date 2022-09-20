event_info <- function(evURL) {

  evJSON <- GET(paste0(evURL, "&page=1"))
  num_pages <- data.frame(fromJSON(rawToChar(evJSON$content)))$pagination.page_count[1] 
  rel_columns <- c("events.organizer_id", "events.venue_id", "events.id", 
                   "events.resource_uri", "events.name", "events.start", "timezone",
                   "utc", "event_date", "event")
  num_columns <- length(rel_columns)
  mat = matrix(ncol = num_columns, nrow = 0)
  event_info <- data.frame(mat)
  
  for (i in 1:num_pages) {
    cur_evURL <- paste0(evURL, "&page=", as.character(i))
    evJSON <- GET(cur_evURL)
    page_event_info <- data.frame(fromJSON(rawToChar(evJSON$content))) %>%
      select(events.organizer_id, events.venue_id, events.id, events.resource_uri, events.name, events.start) %>%
      rename(organizer_id=events.organizer_id, venue_id = events.venue_id,
             event_id = events.id, uri = events.resource_uri, event = events.name, eventdate=events.start) %>% 
      mutate(timezone = eventdate$timezone, utc=eventdate$utc, event_date=eventdate$local,
             event = event$text) %>% 
      select(-eventdate)
    rownames(page_event_info) <- seq(dim(event_info)[1]+1, dim(page_event_info)[1]+dim(event_info)[1])
    event_info <- rbind(event_info, page_event_info)
  }
  
  event_info <- event_info %>% 
    mutate(org_id =case_when(organizer_id == "18671463704" ~ "ePlan",
                             organizer_id == "27675087963" ~ "LCRDM",
                             organizer_id == "8536296706" ~ "DigitalSkills",
                             organizer_id == "11806205959" ~ "NLeSc+LeidenHistoryInst",
                             organizer_id == "29275724533" ~ "NL-RSE",
                             organizer_id == "34021342943" ~ "NLeSc")) %>%  #make sure IDs make sense to us (had to convert manually from EB)
    select(event_id, uri, org_id, venue_id, event, event_date) %>%
    mutate(event_type = get_unique_events(event)) %>%
    mutate(event_level = get_event_level(event_type)) %>%
    mutate(event_focus = get_event_focus(event_type))

  return(event_info)
}
