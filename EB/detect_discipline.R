detect_discipline <- function(answers) {
  if (is.element("dis1", names(answers))) {
    
    otherdis <- read.csv(paste0(getwd(),'/data/ERC_cat.csv'), sep = ";") %>% 
      separate(ERC,sep=" [|] ",c("ERCdis1", "ERCdis2")) %>% 
      separate(NLeSc,sep=" [|] ",c("NLeScdis1", "NLeScdis2"))

    sepdis <- answers %>% 
      pivot_longer(starts_with("dis"),names_to = c("disnum")) %>% 
      rename(dis=value) %>% 
      left_join(otherdis) %>% 
      mutate(as.factor(dis)) %>% 
      pivot_wider(id_cols=id, values_from = c(ERCdis1,ERCdis2,NLeScdis1,NLeScdis2), names_from = disnum) 
    
    ERCdis <- sepdis %>% 
      select(starts_with("ERC"))
    
    NLeScdis <- sepdis %>% 
      select(starts_with("NLeSc"))
    
    bb<-rowMode(data.frame(NLeScdis), ties="random")
    cc<-rowMode(data.frame(ERCdis), ties="random")
    
    answers$ERCdis <- cc
    answers$NLeScdis <- bb
    
  }
  return(answers)
}
