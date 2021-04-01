event_data<-read_csv("eventbrite.csv")
institutes <- unique(toupper(event_data$email_aff))
write_csv(data.frame(institutes), "unique_aff_email.csv")
