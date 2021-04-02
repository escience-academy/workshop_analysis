library(jsonlite)
library(httr)
library(tidyverse)


evURL <- paste0("https://api.surveymonkey.com/oauth/authorize", "?response_type=code", 
                "&redirect_uri=https://www.surveymonkey.com", 
                "&client_id=SHPc8aGJR1-lu_LofL_gOA")
evJSON <- GET(evURL)
event_info <- fromJSON(rawToChar(evJSON$content))