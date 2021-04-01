cut_disciplines <- function(answers) {
  if (is.element("disciplines", names(answers))) {
    answers <- answers %>% 
      separate(disciplines,sep=" [|] ",c("dis1", "dis2", "dis3", "dis4", "dis5"), 
               extra="drop", fill="right")
  }
  return(answers)
}