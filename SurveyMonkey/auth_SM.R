#devtools::install_github("tntp/surveymonkey")
library(tidyverse)
library(surveymonkey)

exec_dir <- paste0(dirname(getwd()), '/EB')
tokens <- read.delim(paste0(exec_dir, "/tokens.txt"), header=F)
token <- str_split(tokens$V1, pattern=" ")[[2]][2] #token stuff for authentication

usethis::edit_r_profile()
options(sm_oauth_token = token)
getOption("sm_oauth_token")

surveys <- browse_surveys(200)  # see your most recent 200 surveys

post_workshop <- surveys %>%
  filter(grepl('post', title)) %>%
  filter(!grepl('Copy of', title)) %>%
  select(title, id) #all post-workshop surveys and ids

pre_workshop <- surveys %>%
  filter(grepl('pre-workshop', title)) %>%
  filter(!grepl('Copy of', title)) %>%
  select(title, id) #all pre-workshop surveys and ids


