library(jsonlite)
library(httr)
library(tidyverse)
library(tools)

exec_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #the dir this script is in
setwd(exec_dir)

source("./EB/get_EB_functions.R")
get_EB_functions() #load all functions necessary

EB<-EB(write_file=T) #if you do not want to write new files put in F here
MM<-Momice(write_file=T)

EB <- read_delim(paste0(exec_dir,'/data/eventbrite.csv'), ",") #load the EventBrite event data
MM <- read_delim(paste0(exec_dir,'/data/momice.csv'), ",") #Mload the omice event data
 
all_event_data<-rbind(EB, MM) #merge EB and MM data

write_csv(all_event_data, paste0(exec_dir,'/data/all_event_data.csv')) #save

