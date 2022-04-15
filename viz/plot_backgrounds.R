library(janitor)
library(tidyverse)

pre_survey_clean <- all_pre_data %>%
  clean_names() %>%
  mutate(OS=as.factor(what_operating_system_is_on_the_computer_you_are_using_at_the_workshop)) %>%
  mutate(click_and_point=ordered(as.factor(a_specialized_software_with_a_point_and_click_graphical_user_interface_e_g_for_statistical_analysis_spss_sas_for_geospatial_analysis_arc_gis_qgis_for_genomics_analysis_geneious), levels = rev(c("Daily", "Weekly", "Monthly", "Several times per year", "Less than once per year", "Never")))) %>%
  mutate(programming = ordered(as.factor(programming_languages_r_python_etc), levels = rev(c("Daily", "Weekly", "Monthly", "Several times per year", "Less than once per year", "Never")))) %>%
  mutate(databases = ordered(as.factor(databases_sql_access_etc), levels = rev(c("Daily", "Weekly", "Monthly", "Several times per year", "Less than once per year", "Never")))) %>%
  mutate(git = ordered(as.factor(git_hub_or_gitlab), levels = rev(c("Daily", "Weekly", "Monthly", "Several times per year", "Less than once per year", "Never")))) %>%
  mutate(shell = ordered(as.factor(a_command_shell_usually_accessed_through_terminal_on_mac_os_or_power_shell_on_windows), levels = rev(c("Daily", "Weekly", "Monthly", "Several times per year", "Less than once per year", "Never")))) %>%
  mutate(workflow_satisfaction = ordered(as.factor(please_rate_your_level_of_satisfaction_with_your_current_data_management_and_analysis_workflow_i_e_how_you_collect_organize_store_and_analyze_your_data), levels = c("Not applicable", "Never thought about this", "Very unsatisfied", "Unsatisfied", "Neutral", "Satisfied", "Very Satisfied"))) %>%
  select(OS, click_and_point, programming, databases, git, shell, workflow_satisfaction)

OS <- pre_survey_clean %>%
  ggplot(aes(x=OS)) +
  geom_bar(aes(fill=OS)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  theme(legend.position = "none") +
  labs(x = "") +
  labs(title = "Operating System Digital Skills workshop participants")


OS


software_use <- pre_survey_clean %>%
  drop_na() %>%
  select(-OS, -workflow_satisfaction) %>%
  pivot_longer(c(click_and_point, programming, databases, git, shell), names_to = "RStype") %>%
  group_by(RStype, value) %>%
  count() %>%
  ggplot(aes(fill=value, y=n, x=RStype)) +
  geom_bar(position="fill", stat="identity")



