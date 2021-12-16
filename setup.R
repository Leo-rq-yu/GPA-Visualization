gpa_o = read.csv("data/uiuc-gpa-dataset.csv")


g_sub = gpa_o %>%
  mutate(Course = paste(Subject, as.character(Number), sep = ""))


write.csv(g_sub, "data/gpa_sub.csv")
