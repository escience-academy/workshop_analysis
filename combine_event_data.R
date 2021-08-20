library(tidyverse)

exec_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #the dir this script is in
setwd(exec_dir)

EB <- read_delim(paste0(exec_dir,'/data/eventbrite.csv'), ",")  %>% # manually updated list of affiliations
  mutate(eSc_collab = eSc_collab=="Yes", 
         eSc_collab = eSc_collab %in% TRUE) %>% 
  mutate(order_id=as.character(order_id))

MM <- read_delim(paste0(exec_dir,'/data/momice.csv'), ",") # manually updated list of affiliations

all_event_data<-rbind(EB, MM)

write_csv(all_event_data, paste0(exec_dir,'/data/all_event_data.csv'))

