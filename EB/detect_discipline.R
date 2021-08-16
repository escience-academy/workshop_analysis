detect_discipline <- function(answers) {
  if (is.element("dis1", names(answers))) {
    
    otherdis <- read.csv(paste0(dirname(getwd()),'/data/ERC_cat.csv'), sep = ";") %>% 
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
    
    bb<-apply(ERCdis,1,function(x) names(which.max(table(x))))
    cc<-apply(NLeScdis,1,function(x) names(which.max(table(x))))
    
    if (!is.null(bb)) {
      bb[sapply(bb, is.null)] <- NA
      answers$ERCdis <- unlist(bb)
        } 
    
    if (!is.null(cc)) {
      cc[sapply(cc, is.null)] <- NA
      answers$NLeScdis <- unlist(cc)
    }
  }
  return(answers)
}
