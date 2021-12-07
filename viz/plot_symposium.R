symp_inst <- all_event_data %>% 
  filter(event_type=="eScience Symposium") %>% 
  mutate(aff_fac=as.factor(affiliation_type)) %>% 
  ggplot(aes(x=aff_fac)) +
  geom_bar(aes(fill=aff_fac)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  theme(legend.position = "none") +
  labs(x = "Affiliation type") +
  labs(title = "number of attendees at eScience Symposium")
  

symp_inst

symp_insp <- all_event_data %>% 
  filter(event_type=="eScience Symposium") 
