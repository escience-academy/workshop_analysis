instr_ratings <- post_workshop_ratings %>%
  select(starts_with("instr")) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = TRUE))

lesson_ratings <- post_workshop_ratings %>%
  select(starts_with("lesson")) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = TRUE))

practical_ratings <- post_workshop_ratings %>%
  select(starts_with("practical")) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = TRUE))

workshop_ratings <- post_workshop_ratings %>%
  group_by(title) %>%
  summarize(mean_instr = mean(instr_answers, instr_enthusiasm, instr_interact_comfort, instr_knowledge),
            mean_lesson = mean(lesson_apply_immediately, lesson_authentic_examples),
            mean_practical = mean(practical_setup, practical_target_audience, practical_org)) %>%
  pivot_longer(c(mean_instr, mean_lesson, mean_practical), names_to = "workshop")

pirateplot(workshop_ratings, formula = workshop_ratings$value ~ workshop_ratings$workshop)

detailed_ratings <- post_workshop_ratings %>%
  select(-id) %>%
  pivot_longer(c(instr_answers, instr_enthusiasm, instr_interact_comfort, instr_knowledge,
                 learningenv_comfort, lesson_apply_immediately, lesson_authentic_examples,
                 practical_setup, practical_target_audience, practical_org), names_to = "evaluation") %>%
  mutate(evaluation_type = as.factor(word(evaluation, start = 1, sep = "_")),
         evaluation = as.factor(evaluation))



pirateplot(formula = value ~ evaluation,
          data = subset(detailed_ratings,
                        evaluation_type == "instr"),
          pal = "eternal",theme = 3,
          cex.lab = .8,
          inf.disp = "rect", ylim=c(1,5),
          inf.f.col = rgb(r=0, g=157, b=221, maxColorValue = 255),
          inf.f.o = .7,
          bean.f.o = 0,
          bean.b.o = 0,
          point.cex = 1,
          point.o = .8,
          main = "",
          xaxt = "n",
          xlab = "",
          ylab = "")

axis(side = 1, 1:4, c("answers", "enthusiasm", "interaction", "knowledge"),
     srt=90, tck = -0.0001, cex.axis = 1.3, mgp = c(1, 1, 0))

title(main = "instructors", cex.main = 2)

pirateplot(formula = value ~ evaluation,
           data = subset(detailed_ratings,
                         evaluation_type == "lesson"),
           pal = "eternal",theme = 3,
           cex.lab = .8,
           inf.disp = "rect", ylim=c(1,5),
           inf.f.col = rgb(r=56, g=3, b=57, maxColorValue = 255),
           inf.f.o = .7,
           bean.f.o = 0,
           bean.b.o = 0,
           point.cex = 1,
           point.o = .8,
           main = "",
           xaxt = "n",
           xlab = "",
           ylab = "")

axis(side = 1, 1:2, c("immediate \n applicability", "authentic \n examples"),
     srt=90, tck = -0.0001, cex.axis = 1.3, mgp = c(1, 2, 0))

title(main = "lessons", cex.main = 2)

pirateplot(formula = value ~ evaluation,
           data = subset(detailed_ratings,
                         evaluation_type == "practical"),
           pal = "eternal",theme = 3,
           cex.lab = .8,
           inf.disp = "rect", ylim=c(1,5),
           inf.f.col = rgb(r=255, g=178, b=19, maxColorValue = 255),
           inf.f.o = .7,
           bean.f.o = 0,
           bean.b.o = 0,
           point.cex = 1,
           point.o = .8,
           main = "",
           xaxt = "n",
           xlab = "",
           ylab = "")

axis(side = 1, 1:3, c("general \n organization", "setup instructions", "clear target \n audience"),
     srt=90, tck = -0.0001, cex.axis = 1.3, mgp = c(1, 2, 0))

title(main = "practical organization", cex.main = 2)



workshop_ratings_mean <- workshop_ratings %>%
  summarize(mean_instr_ratings = mean(mean_instr),
            mean_lesson_ratings = mean(mean_lesson),
            mean_practical_ratings = mean(mean_practical))

workshop_ratings_mean_perworkshop <- workshop_ratings
