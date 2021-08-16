library(tidyverse)
library(rio)

exec_dir <- dirname(rstudioapi::getSourceEditorContext()$path) #the dir this script is in
setwd(exec_dir)


Momice_disc <- c("Agricultural or Environmental Sciences", "Biomedical or Health Sciences", "Chemistry", "Civil, Mechanical, Chemical, or Nuclear Engineering ", 
                 "Computer Science or Electrical Engineering ","Earth Sciences", "Genetics, Genomics or Bioinformatics", "High Performance Computing", 
                 "Humanities", "Planetary Sciences (Geology, Climatology, Oceanography, etc.)","Economics and business", "Education", 
                 "Library and information sciences", "Life Sciences", "Mathematics or Statistics", "Medicine","Physical Sciences", 
                 "Psychology or Neuroscience", "Organismal biology (Ecology, Botany, Zoology, Microbiology, etc.)", "Social Sciences",
                 "Space Sciences", "Not Applicable")

swc.R.20210517 <- rio::import(paste0(data_dir, "2021-05-17-swc-R-nlesc-Registrations.xlsx"))
names(swc.R.20210517)[7:28] <- Momice_disc %>% 
  mutate()


data_dir <- paste0(dirname(exec_dir),'/data/')




