library(dplyr)
library(lubridate)

####*** Sports ***####

# set the directory equals to the location of orginal CSV files

# read all CSVs
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
rm(temp)

# cleanse tables
Users = Users[-1,]

Posts = Posts %>%
  filter(!is.na(X_OwnerUserId))

Comments = Comments %>%
  filter(!is.na(X_UserId))

# create table questions from table posts
Questions = Posts %>% 
  filter(X_PostTypeId == 1) %>%
  select(X_Id, X_AcceptedAnswerId, X_OwnerUserId, X_CreationDate)
  
# create table answers from table posts
Answers = Posts %>% 
  filter(X_PostTypeId == 2) %>%
  select(X_Id, X_ParentId, X_OwnerUserId, X_CreationDate)

# filter only answered questions from table questions
Answered_Questions = Questions %>% filter(!is.na(X_AcceptedAnswerId))

# format date columns
Answers$X_CreationDate = as.Date(Answers$X_CreationDate)
Answered_Questions$X_CreationDate = as.Date(Answered_Questions$X_CreationDate)
Comments$X_CreationDate = as.Date(Comments$X_CreationDate)

# extract year from date columns
Answers$Year = year(Answers$X_CreationDate)
Answered_Questions$Year = year(Answered_Questions$X_CreationDate)
Comments$Year = year(Comments$X_CreationDate)

# show total number of each user's answered questions (all dataset and yearly)
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

# show total number of each user's answers (all dataset and yearly)
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

# show total number of each user's comments (all dataset and yearly)
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

# Creating QA (answer provider -> answer seeker) tables (all dataset and yearly)
temp_Answers = Answers %>% 
  select(X_Id, X_OwnerUserId) %>%
  rename(AnswerId = X_Id, AnswerProvider = X_OwnerUserId)

temp_Answered_Questions = Answered_Questions %>% 
  select(X_AcceptedAnswerId, X_OwnerUserId, Year) %>%
  rename(AnswerId = X_AcceptedAnswerId, AnswerSeeker = X_OwnerUserId)

QA = merge(temp_Answers, temp_Answered_Questions, by='AnswerId')
QA_2012 = QA %>% filter(Year == 2012)
QA_2013 = QA %>% filter(Year == 2013)
QA_2014 = QA %>% filter(Year == 2014)
QA_2015 = QA %>% filter(Year == 2015)
QA_2016 = QA %>% filter(Year == 2016)
rm(temp_Answers, temp_Answered_Questions)

# Removing columns AnswerId and Year
QA$AnswerId = NULL
QA$Year = NULL
QA_2012$AnswerId = NULL
QA_2012$Year = NULL
QA_2013$AnswerId = NULL
QA_2013$Year = NULL
QA_2014$AnswerId = NULL
QA_2014$Year = NULL
QA_2015$AnswerId = NULL
QA_2015$Year = NULL
QA_2016$AnswerId = NULL
QA_2016$Year = NULL

###** Calculate Network's Scoring **###
library(igraph)

# all dataset
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

# 2012 dataset
sports_2012_g = graph.data.frame(QA_2012, directed = T)
sports_2012_vertices = get.data.frame(sports_2012_g, what='vertices')
sports_2012_vertices$betweenness = betweenness(sports_2012_g)
sports_2012_vertices$closeness = closeness(sports_2012_g)
sports_2012_vertices$in_degree = degree(sports_2012_g, mode = 'in')
sports_2012_vertices$out_degree = degree(sports_2012_g, mode = 'out')
sports_2012_vertices$eccentricity = eccentricity(sports_2012_g)
colnames(sports_2012_vertices)[1] = 'user_id'

sports_2012_edges = get.data.frame(sports_2012_g, what='edges')
sports_2012_edges$betweenness = edge_betweenness(sports_2012_g)
colnames(sports_2012_edges)[1] = 'from_user_id'
colnames(sports_2012_edges)[2] = 'to_user_id'
sports_2012_users = merge(sports_2012_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sports_2013_g = graph.data.frame(QA_2013, directed = T)
sports_2013_vertices = get.data.frame(sports_2013_g, what='vertices')
sports_2013_vertices$betweenness = betweenness(sports_2013_g)
sports_2013_vertices$closeness = closeness(sports_2013_g)
sports_2013_vertices$in_degree = degree(sports_2013_g, mode = 'in')
sports_2013_vertices$out_degree = degree(sports_2013_g, mode = 'out')
sports_2013_vertices$eccentricity = eccentricity(sports_2013_g)
colnames(sports_2013_vertices)[1] = 'user_id'

sports_2013_edges = get.data.frame(sports_2013_g, what='edges')
sports_2013_edges$betweenness = edge_betweenness(sports_2013_g)
colnames(sports_2013_edges)[1] = 'from_user_id'
colnames(sports_2013_edges)[2] = 'to_user_id'
sports_2013_users = merge(sports_2013_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sports_2014_g = graph.data.frame(QA_2014, directed = T)
sports_2014_vertices = get.data.frame(sports_2014_g, what='vertices')
sports_2014_vertices$betweenness = betweenness(sports_2014_g)
sports_2014_vertices$closeness = closeness(sports_2014_g)
sports_2014_vertices$in_degree = degree(sports_2014_g, mode = 'in')
sports_2014_vertices$out_degree = degree(sports_2014_g, mode = 'out')
sports_2014_vertices$eccentricity = eccentricity(sports_2014_g)
colnames(sports_2014_vertices)[1] = 'user_id'

sports_2014_edges = get.data.frame(sports_2014_g, what='edges')
sports_2014_edges$betweenness = edge_betweenness(sports_2014_g)
colnames(sports_2014_edges)[1] = 'from_user_id'
colnames(sports_2014_edges)[2] = 'to_user_id'
sports_2014_users = merge(sports_2014_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sports_2015_g = graph.data.frame(QA_2015, directed = T)
sports_2015_vertices = get.data.frame(sports_2015_g, what='vertices')
sports_2015_vertices$betweenness = betweenness(sports_2015_g)
sports_2015_vertices$closeness = closeness(sports_2015_g)
sports_2015_vertices$in_degree = degree(sports_2015_g, mode = 'in')
sports_2015_vertices$out_degree = degree(sports_2015_g, mode = 'out')
sports_2015_vertices$eccentricity = eccentricity(sports_2015_g)
colnames(sports_2015_vertices)[1] = 'user_id'

sports_2015_edges = get.data.frame(sports_2015_g, what='edges')
sports_2015_edges$betweenness = edge_betweenness(sports_2015_g)
colnames(sports_2015_edges)[1] = 'from_user_id'
colnames(sports_2015_edges)[2] = 'to_user_id'
sports_2015_users = merge(sports_2015_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sports_2016_g = graph.data.frame(QA_2016, directed = T)
sports_2016_vertices = get.data.frame(sports_2016_g, what='vertices')
sports_2016_vertices$betweenness = betweenness(sports_2016_g)
sports_2016_vertices$closeness = closeness(sports_2016_g)
sports_2016_vertices$in_degree = degree(sports_2016_g, mode = 'in')
sports_2016_vertices$out_degree = degree(sports_2016_g, mode = 'out')
sports_2016_vertices$eccentricity = eccentricity(sports_2016_g)
colnames(sports_2016_vertices)[1] = 'user_id'

sports_2016_edges = get.data.frame(sports_2016_g, what='edges')
sports_2016_edges$betweenness = edge_betweenness(sports_2016_g)
colnames(sports_2016_edges)[1] = 'from_user_id'
colnames(sports_2016_edges)[2] = 'to_user_id'
sports_2016_users = merge(sports_2016_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

###** Export to CSVs **###

# all dataset
write.csv(sports_users, file = 'sports_users.csv', row.names = F)
write.csv(sports_edges, file = 'sports_edges.csv', row.names = F)
write.csv(QA, file = 'QA.csv', row.names = F)

# 2012
write.csv(sports_2012_users, file = 'sports_2012_users.csv', row.names = F)
write.csv(sports_2012_edges, file = 'sports_2012_edges.csv', row.names = F)
write.csv(QA_2012, file = 'QA_2012.csv', row.names = F)

# 2013
write.csv(sports_2013_users, file = 'sports_2013_users.csv', row.names = F)
write.csv(sports_2013_edges, file = 'sports_2013_edges.csv', row.names = F)
write.csv(QA_2013, file = 'QA_2013.csv', row.names = F)

# 2014
write.csv(sports_2014_users, file = 'sports_2014_users.csv', row.names = F)
write.csv(sports_2014_edges, file = 'sports_2014_edges.csv', row.names = F)
write.csv(QA_2014, file = 'QA_2014.csv', row.names = F)

# 2015
write.csv(sports_2015_users, file = 'sports_2015_users.csv', row.names = F)
write.csv(sports_2015_edges, file = 'sports_2015_edges.csv', row.names = F)
write.csv(QA_2015, file = 'QA_2015.csv', row.names = F)

# 2016
write.csv(sports_2016_users, file = 'sports_2016_users.csv', row.names = F)
write.csv(sports_2016_edges, file = 'sports_2016_edges.csv', row.names = F)
write.csv(QA_2016, file = 'QA_2016.csv', row.names = F)


