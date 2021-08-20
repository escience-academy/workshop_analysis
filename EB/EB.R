#-------------------------------------------
#Script by Lieke de Boer, March 2021
#Calls eSc_ppts which scrapes participant information off of Eventbrite (EB), returns a dataframe with participant information (if available):
# event name
# event date
# organiser name (NLeSc, NL-RSE or other)
# event ppt name
# event ppt email address
# event ppt affiliation
# event ppt career stage
# event ppt collaborator with NLeSc?
# up to 5 disciplines (dis1-dis5)
# did ppt fill out git quiz?
# some EB inforamtion about ticket
#-------------------------------------------
# To Do:
# - take out test events
#-------------------------------------------
# searches eventbrite for my event IDs and returns data about workshops participants
library(jsonlite)
library(httr)
library(tidyverse)

exec_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #the dir this script is in
setwd(exec_dir)

source("get_EB_functions.R")
get_EB_functions()



tokens <- read.delim("tokens.txt", header=F)
token <- str_split(tokens$V1, pattern=" ")[[1]][2]

institutes <- read_delim(paste0(dirname(exec_dir),'/data/unique_aff.csv'), ";") # manually updated list of affiliations
pats       <- c("gmail|hotmail|yahoo|msn|icloud|live|outlook") # most common non-affiliation email addresses
req_names  <- c("id","affiliation","eSc_collab","ERCdis","NLeScdis", "dis1","dis2","dis3","dis4","dis5","car1", "car2",
                "git_quiz","order_id","ticket_type","created","name", "email")      

evURL <- paste0("https://www.eventbriteapi.com/v3/organizations/91980504819/events/", token) #figure out events we have

event_info <- event_info(evURL)

#all_events <- lapply(event_info$uri[c(1,3:10,12:17,20:22,25:39)], function(el) get_ppt_info(el, req_names, token)) # eSc_ppts_EB extracts info 
all_events <- lapply(event_info$uri, function(el) get_ppt_info(el, req_names, token)) # eSc_ppts_EB extracts info 
#all_events <- lapply(event_info$uri[5], function(el) get_ppt_info(el, req_names, token)) # eSc_ppts_EB extracts info 

# from each events' page
# numbers in between throw an error I haven't had time to check yet (2,11,18,19,23,24)

all_together <- do.call("bind_rows", all_events) %>% 
  mutate(affiliation=toupper(affiliation))

# all_together <- left_join(all_together, unique(institutes)) #manually adapted affiliations

all_together$email_aff<-sapply(strsplit(all_together$email, "@"), "[[", 2)
all_together$email_aff<-str_replace_all(all_together$email_aff, pats, NA_character_)

event_data <- merge(all_together, event_info, by="event_id")
event_data <- merge(event_data, unique(institutes), all.x = T) %>% 
  mutate(affiliation = aff_corrected) %>% 
  mutate(affiliation = coalesce(affiliation, email_aff)) %>% 
  mutate(affiliation = toupper(affiliation)) %>% 
  mutate(year = format(as.Date(event_date, format="%Y-%m-%d"), "%Y")) %>% 
  select(-aff_corrected) %>% 
  arrange(.,event)

event_data <- merge(event_data, unique(institutes), by="affiliation", all.x=T) %>% 
  mutate(affiliation=toupper(aff_corrected), 
         Affiliation_type=Affiliation_type.x) %>% 
  select(-aff_corrected, -Affiliation_type.y) %>% 
  select(event, event_date, year, org_id,name,email,affiliation,Affiliation_type,car1,car2,eSc_collab,ERCdis, NLeScdis, dis1,dis2,dis3,dis4,dis5,
        created,event_type,event_level,event_focus, ticket_type,order_id,id,event_id,venue_id,uri) %>% 
  arrange(.,event)


 event_data <- left_join(event_data, unique(institutes), by="affiliation", all.x=T) %>%# do this again so the ones who filled out something like "PhD student" 
   #in the affiliation field, get the affiliation from their email address
   select(event, event_date, year, org_id,name,email,affiliation, Affiliation_type.y, car1,car2,eSc_collab,ERCdis, NLeScdis, dis1,dis2,dis3,dis4,dis5,
          aff_country, RI_type,created,event_type,event_level,event_focus, ticket_type,order_id,id,event_id,venue_id,uri) %>% 
   mutate(affiliation_type = Affiliation_type.y) %>% 
   mutate(order_id = as.character(order_id)) %>%  #so compatible with Momice
   select(-Affiliation_type.y) %>% 
   slice(c(which(year>2015))) %>% #there is only one event in 2015 and five Lodes attended
   arrange(.,year)

  
write_csv(event_data, paste0(dirname(exec_dir),'/data/eventbrite.csv'))

