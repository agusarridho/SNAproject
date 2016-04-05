library(dplyr)
library(lubridate)

###*** Sports ***###

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
rm(temp)

Users = Users[-1,]

Posts = Posts %>%
  filter(!is.na(X_OwnerUserId))

Comments = Comments %>%
  filter(!is.na(X_UserId))

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
  arrange(desc(Frequency))

Freq.Ans_Quest_2012 = Answered_Questions %>%
  filter(Year == 2012) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2013 = Answered_Questions %>%
  filter(Year == 2013) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2014 = Answered_Questions %>%
  filter(Year == 2014) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2015 = Answered_Questions %>%
  filter(Year == 2015) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2016 = Answered_Questions %>%
  filter(Year == 2016) %>%
  group_by(X_OwnerUserId) %>%
  summarise(Frequency=n()) %>%
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
  arrange(desc(Frequency))

Freq.Comm_2012 = Comments %>%
  filter(Year == 2012) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm_2013 = Comments %>%
  filter(Year == 2013) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm_2014 = Comments %>%
  filter(Year == 2014) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm_2015 = Comments %>%
  filter(Year == 2015) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Comm_2016 = Comments %>%
  filter(Year == 2016) %>%
  group_by(X_UserId) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

temp_Answers = Answers %>% 
  select(X_Id, X_OwnerUserId) %>%
  rename(AnswerId = X_Id, AnswerProvider = X_OwnerUserId)

temp_Answered_Questions = Answered_Questions %>% 
  select(X_AcceptedAnswerId, X_OwnerUserId) %>%
  rename(AnswerId = X_AcceptedAnswerId, AnswerSeeker = X_OwnerUserId)

QA = merge(temp_Answers, temp_Answered_Questions, by='AnswerId')
QA$AnswerId = NULL

library(igraph)
sports_g = graph.data.frame(QA, directed = T)

sports_vertices = get.data.frame(sports_g, what='vertices')
sports_vertices$betweenness = betweenness(sports_g)
sports_vertices$closeness = closeness(sports_g)
sports_vertices$in_degree = degree(sports_g, mode = 'in')
sports_vertices$out_degree = degree(sports_g, mode = 'out')
sports_vertices$eccentricity = eccentricity(sports_g)
colnames(sports_vertices)[1] = 'user_id'

sports_edges = get.data.frame(sports_g, what='edges')
sports_edges$betweenness = edge_betweenness(sports_g)
colnames(sports_edges)[1] = 'from_user_id'
colnames(sports_edges)[2] = 'to_user_id'

sports_users = merge(sports_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

write.csv(sports_users, file = 'sports_users.csv', row.names = F)
write.csv(sports_edges, file = 'sports_edges.csv', row.names = F)

# Check for NA
sapply(QA, function(x) sum(is.na(x)))

write.csv(QA, file = 'QA.csv', row.names = F)
