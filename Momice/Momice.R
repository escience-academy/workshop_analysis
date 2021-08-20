#Script by Lieke de Boer, August 2021
# This is an ugly script that just needs to be run once, in theory. We will not receive new Momice data so only rerun/save 
# this script if something is wrong with the variables

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
  
  swc.R.20210517        <- rio::import(paste0(exec_dir, "/2021-05-17-swc-R-nlesc-Registrations.xlsx"))
  ds.parallel.20210412  <- rio::import(paste0(exec_dir, "/2021-04-12-parallel-python-attendance.xlsx"))
  dc.py.20210419        <- rio::import(paste0(exec_dir, "/2021-04-19-dc-py-attendance.xlsx"))
  
  names(swc.R.20210517)[7:28] <- Momice_disc 
  names(ds.parallel.20210412)[7:28] <- Momice_disc
  names(dc.py.20210419)[7:28] <- Momice_disc 
  
  swc.R.20210517$event <- "Software Carpentry with R"
  swc.R.20210517$event_date <- "2021-05-17"
  ds.parallel.20210412$event <- "Parallel Programming with Python"
  ds.parallel.20210412$event_date <- "2021-04-12"
  dc.py.20210419$event <- "Data Carpentry with Python"
  dc.py.20210419$event_date <- "2021-04-19"
  
  Momice_events<-rbind(swc.R.20210517, ds.parallel.20210412, dc.py.20210419) %>% 
    mutate(eSc_collab = `Fill in the code you received from the eScience Center here:`=="eScience2021", 
           eSc_collab = eSc_collab %in% TRUE,
           year = 2021,
           event_date = format(as.Date(event_date)),
           year = format(as.Date(event_date, format="%Y-%m-%d"), "%Y"),
           event_id = NA,
           venue_id = NA,
           uri=`Confirmation URL`,
           email = Email,
           name = Name,
           ticket_type = Ticket,
           id=`Registration id`,
           order_id=id,
           created=`Registration date`,
           affiliation = `What's your affiliation? (e.g. Leiden University)`,
           car1 = `What is your career stage? Please choose the option you think describes your position best.`,
           org_id = NA,
           car2 = NA)
  
  indxs <- data.frame(which((Momice_events == "checked" | Momice_events == "aangevinkt"), arr.ind=T)) %>% 
    arrange(row) %>% 
    filter(col>6) %>% 
    filter(col<29) %>% 
    group_by(row) %>% 
    mutate(index=1:n()) %>% 
    mutate(disnum=paste0("dis", index)) %>% 
    select(-index) # split up disciplines into separate columns
  
  for (i in 1:max(indxs$row)) {
    indxs$col[indxs$col==i] <- names(Momice_events)[i] #fill out names of disciplines
  }
  
  indxs <- indxs %>%
    spread(disnum, col)
  
  indxs$name <- Momice_events$name[indxs$row] 
  indxs$id <- indxs$row
  
  indxs<-detect_discipline(indxs)
  
  event_data_mm <- merge(indxs, Momice_events, by="name")
  
  event_data_mm <-event_data_mm %>% 
    mutate(event_type = get_unique_events(event)) %>% 
    mutate(event_level = get_event_level(event_type)) %>% 
    mutate(event_focus = get_event_focus(event_type)) %>% 
    mutate(affiliation=toupper(affiliation))
  
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
           id=id.x) %>% 
    select(-aff_corrected, -Affiliation_type.y, -id.x) %>% 
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
    write_csv(event_data, paste0(dirname(exec_dir),'/data/momice.csv'))
  } 
  return(event_data)
}
