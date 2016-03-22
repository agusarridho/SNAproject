library(dplyr)

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

Answered_Questions$Date = as.Date(Answered_Questions$X_CreationDate)

Freq.Ans_Quest = Answered_Questions %>% 
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_OwnerUserId)) %>%
  arrange(desc(Frequency))

Freq.Ans = Answers %>% 
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm = Comments %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  filter(!is.na(X_UserId)) %>%
  arrange(desc(Frequency))