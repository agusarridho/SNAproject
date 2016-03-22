library(dplyr)
library(lubridate)

###*** Sports ***###

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
rm(temp)

Users = Users[-1,]

Questions = Posts %>% 
  filter(X_PostTypeId == 1) %>%
  select(X_Id, X_AcceptedAnswerId, X_OwnerUserId, X_CreationDate)
  
Answered_Questions = Questions %>% filter(!is.na(X_AcceptedAnswerId))

Answers = Posts %>% 
  filter(X_PostTypeId == 2) %>%
  select(X_Id, X_ParentId, X_OwnerUserId, X_CreationDate)

Answers$X_CreationDate = as.Date(Answers$X_CreationDate)
Answered_Questions$X_CreationDate = as.Date(Answered_Questions$X_CreationDate)
Comments$X_CreationDate = as.Date(Comments$X_CreationDate)

Answers$Year = year(Answers$X_CreationDate)
Answered_Questions$Year = year(Answered_Questions$X_CreationDate)
Comments$Year = year(Comments$X_CreationDate)

Freq.Ans_Quest = Answered_Questions %>% 
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2012 = Answered_Questions %>%
  filter(Year == 2012) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2013 = Answered_Questions %>%
  filter(Year == 2013) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2014 = Answered_Questions %>%
  filter(Year == 2014) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2015 = Answered_Questions %>%
  filter(Year == 2015) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2016 = Answered_Questions %>%
  filter(Year == 2016) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans = Answers %>% 
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2012 = Answers %>% 
  filter(Year == 2012) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2013 = Answers %>% 
  filter(Year == 2013) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2014 = Answers %>% 
  filter(Year == 2014) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2015 = Answers %>% 
  filter(Year == 2015) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2016 = Answers %>% 
  filter(Year == 2016) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm = Comments %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

Freq.Comm_2012 = Comments %>%
  filter(Year == 2012) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

Freq.Comm_2013 = Comments %>%
  filter(Year == 2013) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

Freq.Comm_2014 = Comments %>%
  filter(Year == 2014) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

Freq.Comm_2015 = Comments %>%
  filter(Year == 2015) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

Freq.Comm_2016 = Comments %>%
  filter(Year == 2016) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))

temp_Answers = Answers %>% 
  select(X_Id, X_OwnerUserId) %>%
  rename(AnswerId = X_Id, AnswerUserId = X_OwnerUserId)

temp_Answered_Questions = Answered_Questions %>% 
  select(X_AcceptedAnswerId, X_OwnerUserId) %>%
  rename(AnswerId = X_AcceptedAnswerId, QuestionUserId = X_OwnerUserId)

QA = merge(temp_Answers, temp_Answered_Questions, by='AnswerId')

write.csv(QA, file = 'QA.csv', row.names = F)
