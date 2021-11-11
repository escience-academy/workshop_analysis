all_event_data %>% 
  filter(org_id=="NLeSc") %>% 
  filter(!is.na(NLeScdis)) %>% 
  select(event_date, car1, NLeScdis, event_type) %>% 
  group_by(NLeScdis) %>% 
  count() %>% 
  ggplot(aes(x=NLeScdis, fill=NLeScdis, y=n)) +
  geom_bar(position="stack", stat="identity")

all_event_data %>% 
  filter(org_id=="NLeSc") %>% 
  filter(!is.na(NLeScdis)) %>% 
  filter(event_level!="Other") %>% 
  select(event_date, car1, NLeScdis, event_type, event_level) %>% 
  group_by(NLeScdis, event_level) %>% 
  count() %>% 
  ggplot(aes(fill=event_level, x=NLeScdis, y=n)) +
  geom_bar(position="fill", stat="identity")

all_event_data %>% 
  filter(org_id=="NLeSc") %>% 
  filter(!is.na(NLeScdis)) %>% 
  filter(event_level!="Other") %>% 
  select(event_date, car1, NLeScdis, event_type, event_level) %>% 
  group_by(NLeScdis, event_level) %>% 
  count() %>% 
  ggplot(aes(fill=NLeScdis, x=event_level, y=n)) +
  geom_bar(position="fill", stat="identity") 

## Advanced includes:
# - Parallel Programming in Python
# - CodeRefinery
# - Containers
# - 
