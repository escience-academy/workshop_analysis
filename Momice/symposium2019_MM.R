
Momice <- function(write_file) {
  
  exec_dir <- paste0(getwd(), '/Momice')
  
  institutes <- read_delim(paste0(dirname(exec_dir),'/data/unique_aff.csv'), ";") # manually updated list of affiliations
  pats       <- c("gmail|hotmail|yahoo|msn|icloud|live|outlook") # most common non-affiliation email addresses
  
  Momice_disc <- c("Agricultural or Environmental Sciences", "Biomedical or Health Sciences", "Chemistry", "Civil, Mechanical, Chemical, or Nuclear Engineering", 
                   "Computer Science or Electrical Engineering","Earth Sciences", "Genetics, Genomics or Bioinformatics", "High Performance Computing", 
                   "Humanities", "Planetary Sciences (Geology, Climatology, Oceanography, etc.)","Economics and business", "Education", 
                   "Library and information sciences", "Life Sciences", "Mathematics or Statistics", "Medicine","Physical Sciences", 
                   "Psychology or Neuroscience", "Organismal biology (Ecology, Botany, Zoology, Microbiology, etc.)", "Social Sciences",
                   "Space Sciences", "Not Applicable")
  
  symposium2019        <- rio::import(paste0(exec_dir, "/2019-11-26_escience_symposium_Registrations.xlsx"))
  
  symposium2019$event <- "eScience Symposium 2019"
  symposium2019$event_date <- "2019-11-26"
 
  
  event_data_mm <-symposium2019 %>% 
    mutate(eSc_collab = (Ticket == "Free ticket" & List =="Lijst medewerke"), 
           eSc_collab = eSc_collab %in% TRUE,
           year = year(`Registration date`),
           event_date = format(as.Date(event_date)),
           event_id = NA,
           venue_id = NA,
           id=NA,
           uri=`Confirmation URL`,
           email = Email,
           name = paste(Name, Surname),
           ticket_type = Ticket,
           id=`Registration id`,
           order_id=NA,
           created=`Registration date`,
           affiliation = Organization,
           car1 = NA,
           org_id = NA,
           car2 = NA,
           dis1 = NA,
           dis2 = NA,
           dis3 = NA,
           dis4 = NA,
           dis5 = NA,
           NLeScdis = NA,
           ERCdis = NA,
           event_type = "eScience Symposium",
           event_level = NA,
           event_focus = NA)
  
 
  event_data_mm$email_aff<-sapply(strsplit(event_data_mm$email, "@"), "[[", 2)
  event_data_mm$email_aff<-str_replace_all(event_data_mm$email_aff, pats, NA_character_)
  
  event_data <- merge(event_data_mm, unique(institutes), all.x = T) %>% 
    mutate(affiliation = aff_corrected) %>% 
    mutate(affiliation = coalesce(affiliation, email_aff)) %>% 
    mutate(affiliation = toupper(affiliation)) %>% 
    mutate(year = format(as.Date(event_date, format="%Y-%m-%d"), "%Y")) %>% 
    select(-aff_corrected) %>% 
    arrange(.,event)
  
  event_data <- merge(event_data, unique(institutes), by="affiliation", all.x=T) %>% 
    mutate(affiliation=toupper(aff_corrected), 
           Affiliation_type=Affiliation_type.x, 
           id=id) %>% 
    select(-aff_corrected, -Affiliation_type.y) %>% 
    select(event, event_date, year, org_id,name,email,affiliation,Affiliation_type,car1,car2,eSc_collab,ERCdis, NLeScdis, dis1,dis2,dis3,dis4,dis5,
           created,event_type,event_level,event_focus, ticket_type,order_id,id,event_id,venue_id,uri) %>% 
    arrange(.,event)
  
  event_data <- left_join(event_data, unique(institutes), by="affiliation", all.x=T) %>%# do this again so the ones who filled out something like "PhD student" 
    #in the affiliation field, get the affiliation from their email address
    select(event, event_date, year, org_id,name,email,affiliation, Affiliation_type.y, car1,car2,eSc_collab,ERCdis, NLeScdis, dis1,dis2,dis3,dis4,dis5,
           aff_country, RI_type,created,event_type,event_level,event_focus, ticket_type,order_id,id,event_id,venue_id,uri) %>% 
    mutate(affiliation_type = Affiliation_type.y) %>% 
    mutate(car1=tolower(car1)) %>% 
    mutate(car1=toTitleCase(car1)) %>% 
    mutate(car1 = str_replace_all(car1, 
                                  c("Phd Candidate" = "Graduate Student",
                                    "Master Student" = "Graduate Student",
                                    "Research Programmer" = "Research Software Engineer", 
                                    "Including" = "including"))) %>% 
    select(-Affiliation_type.y) %>% 
    slice(c(which(year>2015))) %>% #there is only one event in 2015 and five Lodes attended
    arrange(.,year)
  
  
  if (write_file) {
    write_csv(event_data, paste0(dirname(exec_dir),'/data/momice_symposium2019.csv'))
  } 
  return(event_data)
}
