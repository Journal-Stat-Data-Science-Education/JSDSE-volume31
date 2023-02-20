require(tidyverse)

BM110_0 <- read_csv("./Week 0 Evaluation/Week0_Introductory Survey_BM110.csv")
MM104_0 <- read_csv("./Week 0 Evaluation/Week0_IntroductorySurvey_MM104.csv")
MM106_0 <- read_csv("./Week 0 Evaluation/Week0_IntroductorySurvey_MM106.csv")

MM104_0 %>% bind_rows(MM106_0) -> MM104_0

BM110_5 <- read_csv("./Week 5 Evaluation/BM110 Midway Evaluation.csv")
MM104_5 <- read_csv("./Week 5 Evaluation/MM104 Midway Evaluation.csv")
MM106_5 <- read_csv("./Week 5 Evaluation/MM106 Midway Evaluation.csv")

MM104_5 %>% bind_rows(MM106_5) -> MM104_5

BM110_10 <- read_csv("./Week 10 Evaluation/BM110 End of Module Evaluation.csv")
MM104_10 <- read_csv("./Week 10 Evaluation/MM104 End of Module Evaluation.csv")
MM106_10 <- read_csv("./Week 10 Evaluation/MM106 End of Module Evaluation.csv")

MM104_10 %>% bind_rows(MM106_10) -> MM104_10


# Perceived difficulty - how hard did they think it was going to be
# Survey 1

MM104_0 %>% 
  group_by(`Are you worried/nervous about learning a new computing language ?`) %>% 
  summarise(n())

BM110_0 %>% 
  group_by(`Are you worried/nervous about learning a new computing language ?`) %>% 
  summarise(n())

MM104_0 %>% 
  mutate(Class = "MM104") -> MM104_0

BM110_0 %>% 
  mutate(Class = "BM110") -> BM110_0

MM104_0 %>% 
  bind_rows(BM110_0) %>% 
  group_by(Class, `Are you worried/nervous about learning a new computing language ?`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=`Are you worried/nervous about learning a new computing language ?`, values_from=count) %>% 
  ungroup() %>% 
  as.data.frame() ->
  for_test

rownames(for_test) <- for_test$Class
for_test <- for_test[,-1]
fisher.test(for_test)
chisq.test(for_test)$expected

## Week 5

MM104_5 %>% 
  group_by(`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(n())

BM110_5 %>% 
  group_by(`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(n())

MM104_5 %>% 
  mutate(Class = "MM104") -> MM104_5

BM110_5 %>% 
  mutate(Class = "BM110") -> BM110_5

MM104_5 %>% 
  bind_rows(BM110_5) %>% 
  group_by(Class, `At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`, values_from=count) %>% 
  ungroup() %>% 
  as.data.frame() ->
  for_test

rownames(for_test) <- for_test$Class
for_test <- for_test[,-1]
fisher.test(for_test)
chisq.test(for_test)$expected


## Week 10 

MM104_10 %>% 
  group_by(`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(n())

BM110_10 %>% 
  group_by(`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(n())

MM104_10 %>% 
  mutate(Class = "MM104") -> MM104_10

BM110_10 %>% 
  mutate(Class = "BM110") -> BM110_10

MM104_10 %>% 
  bind_rows(BM110_10) %>% 
  group_by(Class, `At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=`At this point in the course, how would you rate your anxiety levels around using R for statistical analysis?`, values_from=count, values_fill=0) %>% 
  ungroup() %>% 
  as.data.frame() ->
  for_test

rownames(for_test) <- for_test$Class
for_test <- for_test[,-1]
fisher.test(for_test)
chisq.test(for_test)$expected
# Enjoyment - track through the three surveys

MM104_5 %>% 
  group_by(`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(n())

BM110_5 %>% 
  group_by(`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(n())

MM104_5 %>% 
  mutate(Class = "MM104") -> MM104_5

BM110_5 %>% 
  mutate(Class = "BM110") -> BM110_5

MM104_5 %>% 
  bind_rows(BM110_5) %>% 
  group_by(Class, `How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`, values_from=count, values_fill=0) %>% 
  ungroup() %>% 
  as.data.frame() ->
  for_test

rownames(for_test) <- for_test$Class
for_test <- for_test[,-1]
fisher.test(for_test)$expected
chisq.test(for_test)$expected

MM104_10 %>% 
  group_by(`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(n())

BM110_10 %>% 
  group_by(`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(n())

MM104_10 %>% 
  mutate(Class = "MM104") -> MM104_10

BM110_10 %>% 
  mutate(Class = "BM110") -> BM110_10

MM104_10 %>% 
  bind_rows(BM110_10) %>% 
  group_by(Class, `How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from=`How would you rate your agreement with the following statement: I enjoyed learning R as part of this module.`, values_from=count, values_fill=0) %>% 
  ungroup() %>% 
  as.data.frame() ->
  for_test

rownames(for_test) <- for_test$Class
for_test <- for_test[,-1]
fisher.test(for_test)$expected # can't do fishers chi-sq highly signif


# Comparison between SIPBS and M&S
# Compare test 1 directly
# Compare test 2&3 (SIPBS) test 3 (M&S)

current_marks <- read_csv("Marks/Test 1 Marks.csv", na="-")[,1:2]

current_marks %>% 
  rename(Score=Test1, Cohort=Class) %>% 
  mutate(Cohort=case_when(Cohort=="BM110"~"Biomedical Sciences",
                         TRUE~"Mathematics & Statistics")) %>% 
  ggplot() +
  geom_histogram(aes(fill=Cohort, x=Score, y=after_stat(density)), colour="white") +
  facet_wrap(~Cohort, nrow=2) +
  theme_bw() +
  theme(legend.position = "none")

wilcox.test(Test1~Class, data=current_marks)

current_marks %>% 
  group_by(Class) %>% 
  summarise(median(Test1, na.rm=TRUE))

# Comparison with previous year
# Expect downward trend

Marks <- read_csv("BM110 Marks.csv")

Marks %>%
  rename(Score=Mark) %>% 
  ggplot() + 
  geom_histogram(aes(fill=Time, x=Score, y=after_stat(density)), colour="white") + 
  facet_wrap(~Time, nrow=2) +
  theme_bw() + 
  theme(legend.position = "none")

wilcox.test(Mark~Time, data=Marks)
