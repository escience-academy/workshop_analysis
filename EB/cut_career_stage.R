cut_career_stage <- function(answers) {
  if (is.element("career_stage", names(answers))) {
    answers <- answers %>% 
      separate(career_stage,sep=" [|] ",c("car1", "car2", "car3", "car4", "car5"), 
               extra="drop", fill="right")
  }
  return(answers)
}