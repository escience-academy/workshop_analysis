library(jsonlite)
library(httr)
library(tidyverse)
library(tools)
library(readr)

exec_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #the dir this script is in
setwd(exec_dir)

source("./EB/get_EB_functions.R")
get_EB_functions() #load all functions necessary

EB<-EventBrite(write_file=F) #if you do not want to write new files put in F here
MM<-Momice(write_file=F) # not necessary to change to T unless something needs to be changed about the data.
                         # No more new Momice data will come in.

EB <- read_delim(paste0(exec_dir,'/data/eventbrite.csv'), ",") #load the EventBrite event data
MM <- read_delim(paste0(exec_dir,'/data/momice.csv'), ",") #Mload the Momice event data
MMsymp <- read_delim(paste0(exec_dir,'/data/momice_symposium2019.csv'), ",") #Mload t)

all_event_data<-rbind(EB, MM, MMsymp) #merge EB and MM data

write_csv(all_event_data, paste0(exec_dir,'/data/all_event_data.csv')) #save

training_events2021<- all_event_data %>%
  filter(year==2021, event_type != "NL-RSE meetup") %>% 
  count(car1)


training_events2022<- all_event_data %>%
  filter(year==2022, event_type != "NL-RSE meetup") %>% 
  count(car1)
