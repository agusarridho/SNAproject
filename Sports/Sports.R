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
  select(X_Id, X_AcceptedAnswerId, X_OwnerUserId, X_CreationDate) %>%
  rename(Question = X_Id, AcceptedAnswer = X_AcceptedAnswerId, 
         AnswerSeeker = X_OwnerUserId, QuestionDate = X_CreationDate)
Questions$QuestionDate = as.Date(Questions$QuestionDate)
Questions$Year = year(Questions$QuestionDate)

# create table answers from table posts
Answers = Posts %>% 
  filter(X_PostTypeId == 2) %>%
  select(X_Id, X_ParentId, X_OwnerUserId, X_CreationDate) %>%
  rename(Answer = X_Id, Question = X_ParentId, 
         AnswerProvider = X_OwnerUserId, AnswerDate = X_CreationDate)
Answers$AnswerDate = as.Date(Answers$AnswerDate)
Answers$Year = year(Answers$AnswerDate)

# filter only answered questions from table questions
Answered_Questions = Questions %>% filter(!is.na(AcceptedAnswer))

# format date columns
Comments$X_CreationDate = as.Date(Comments$X_CreationDate)
# extract year from date columns
Comments$Year = year(Comments$X_CreationDate)

# merge comments and questions
temp_comments = Comments %>% 
  select(X_PostId, X_UserId) %>%
  rename(Question = X_PostId, Commentator = X_UserId)
temp_questions = Questions %>% 
  select(Question, AnswerSeeker, Year)
Comments_Questions = merge(temp_comments, temp_questions, by = 'Question')
Comments_Questions$Question_Id = NULL
Comments_Questions_2012 = Comments_Questions %>% filter(Year == 2012)
Comments_Questions_2013 = Comments_Questions %>% filter(Year == 2013)
Comments_Questions_2014 = Comments_Questions %>% filter(Year == 2014)
Comments_Questions_2015 = Comments_Questions %>% filter(Year == 2015)
Comments_Questions_2016 = Comments_Questions %>% filter(Year == 2016)
Comments_Questions$Year = NULL
Comments_Questions_2012$Year = NULL
Comments_Questions_2013$Year = NULL
Comments_Questions_2014$Year = NULL
Comments_Questions_2015$Year = NULL
Comments_Questions_2016$Year = NULL
rm(temp_questions, temp_comments)

# merge comments and answers
temp_comments = Comments %>% 
  select(X_PostId, X_UserId) %>%
  rename(Answer = X_PostId, Commentator = X_UserId)
temp_answers = Answers %>% 
  select(Answer, AnswerProvider, Year)
Comments_Answers = merge(temp_comments, temp_answers, by = 'Answer')
Comments_Answers$Answer = NULL
Comments_Answers_2012 = Comments_Answers %>% filter(Year == 2012)
Comments_Answers_2013 = Comments_Answers %>% filter(Year == 2013)
Comments_Answers_2014 = Comments_Answers %>% filter(Year == 2014)
Comments_Answers_2015 = Comments_Answers %>% filter(Year == 2015)
Comments_Answers_2016 = Comments_Answers %>% filter(Year == 2016)
Comments_Answers$Year = NULL
Comments_Answers_2012$Year = NULL
Comments_Answers_2013$Year = NULL
Comments_Answers_2014$Year = NULL
Comments_Answers_2015$Year = NULL
Comments_Answers_2016$Year = NULL
rm(temp_answers, temp_comments)

# show total number of each user's answered questions (all dataset and yearly)
Freq.Ans_Quest = Answered_Questions %>% 
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2012 = Answered_Questions %>%
  filter(Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2013 = Answered_Questions %>%
  filter(Year == 2013) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2014 = Answered_Questions %>%
  filter(Year == 2014) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2015 = Answered_Questions %>%
  filter(Year == 2015) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2016 = Answered_Questions %>%
  filter(Year == 2016) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))


# show total number of each user's answers (all dataset and yearly)
Freq.Ans = Answers %>% 
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2012 = Answers %>% 
  filter(Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2013 = Answers %>% 
  filter(Year == 2013) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2014 = Answers %>% 
  filter(Year == 2014) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2015 = Answers %>% 
  filter(Year == 2015) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2016 = Answers %>% 
  filter(Year == 2016) %>%
  group_by(AnswerProvider) %>%
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

# Creating QA_All (answer provider -> answer seeker) tables (all dataset and yearly)
temp_Answers = Answers %>% 
  select(Question, Answer, AnswerProvider, Year)

temp_Questions = Questions %>% 
  select(Question, AnswerSeeker, AcceptedAnswer)

QA_All = merge(temp_Answers, temp_Questions, by='Question', all.x = T)
QA_All = QA_All %>% filter(!is.na(AnswerSeeker))

QA_All$Answer = NULL
QA_All$Question = NULL
QA_All_2012 = QA_All %>% filter(Year == 2012)
QA_All_2013 = QA_All %>% filter(Year == 2013)
QA_All_2014 = QA_All %>% filter(Year == 2014)
QA_All_2015 = QA_All %>% filter(Year == 2015)
QA_All_2016 = QA_All %>% filter(Year == 2016)

# Removing columns AnswerId and Year
QA_All$Year = NULL
QA_All_2012$Year = NULL
QA_All_2013$Year = NULL
QA_All_2014$Year = NULL
QA_All_2015$Year = NULL
QA_All_2016$Year = NULL
rm(temp_Answers, temp_Questions)

# Creating QA_Accepted (accepted answer provider only -> answer seeker) tables (all dataset and yearly)
temp_Answers = Answers %>% 
  select(Answer, AnswerProvider, Year)

temp_Answered_Questions = Answered_Questions %>% 
  select(AcceptedAnswer, AnswerSeeker) %>%
  rename(Answer = AcceptedAnswer)

QA_Accepted = merge(temp_Answers, temp_Answered_Questions, by = 'Answer')

QA_Accepted$Answer = NULL
QA_Accepted_2012 = QA_Accepted %>% filter(Year == 2012)
QA_Accepted_2013 = QA_Accepted %>% filter(Year == 2013)
QA_Accepted_2014 = QA_Accepted %>% filter(Year == 2014)
QA_Accepted_2015 = QA_Accepted %>% filter(Year == 2015)
QA_Accepted_2016 = QA_Accepted %>% filter(Year == 2016)

# Removing columns AnswerId and Year
QA_Accepted$Year = NULL
QA_Accepted_2012$Year = NULL
QA_Accepted_2013$Year = NULL
QA_Accepted_2014$Year = NULL
QA_Accepted_2015$Year = NULL
QA_Accepted_2016$Year = NULL
rm(temp_Answers, temp_Answered_Questions)

###** Calculate Network's Scoring **###
library(igraph)

# For QA_All
# all dataset
sports_QAAll_g = graph.data.frame(QA_All, directed = T)
sports_QAAll_vertices = get.data.frame(sports_QAAll_g, what='vertices')
sports_QAAll_vertices$betweenness = betweenness(sports_QAAll_g)
sports_QAAll_vertices$closeness = closeness(sports_QAAll_g)
sports_QAAll_vertices$in_degree = degree(sports_QAAll_g, mode = 'in')
sports_QAAll_vertices$out_degree = degree(sports_QAAll_g, mode = 'out')
sports_QAAll_vertices$eccentricity = eccentricity(sports_QAAll_g)
colnames(sports_QAAll_vertices)[1] = 'user_id'
sports_QAAll_users = merge(sports_QAAll_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2012 dataset
sports_QAAll_2012_g = graph.data.frame(QA_All_2012, directed = T)
sports_QAAll_2012_vertices = get.data.frame(sports_QAAll_2012_g, what='vertices')
sports_QAAll_2012_vertices$betweenness = betweenness(sports_QAAll_2012_g)
sports_QAAll_2012_vertices$closeness = closeness(sports_QAAll_2012_g)
sports_QAAll_2012_vertices$in_degree = degree(sports_QAAll_2012_g, mode = 'in')
sports_QAAll_2012_vertices$out_degree = degree(sports_QAAll_2012_g, mode = 'out')
sports_QAAll_2012_vertices$eccentricity = eccentricity(sports_QAAll_2012_g)
colnames(sports_QAAll_2012_vertices)[1] = 'user_id'
sports_QAAll_2012_users = merge(sports_QAAll_2012_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sports_QAAll_2013_g = graph.data.frame(QA_All_2013, directed = T)
sports_QAAll_2013_vertices = get.data.frame(sports_QAAll_2013_g, what='vertices')
sports_QAAll_2013_vertices$betweenness = betweenness(sports_QAAll_2013_g)
sports_QAAll_2013_vertices$closeness = closeness(sports_QAAll_2013_g)
sports_QAAll_2013_vertices$in_degree = degree(sports_QAAll_2013_g, mode = 'in')
sports_QAAll_2013_vertices$out_degree = degree(sports_QAAll_2013_g, mode = 'out')
sports_QAAll_2013_vertices$eccentricity = eccentricity(sports_QAAll_2013_g)
colnames(sports_QAAll_2013_vertices)[1] = 'user_id'
sports_QAAll_2013_users = merge(sports_QAAll_2013_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sports_QAAll_2014_g = graph.data.frame(QA_All_2014, directed = T)
sports_QAAll_2014_vertices = get.data.frame(sports_QAAll_2014_g, what='vertices')
sports_QAAll_2014_vertices$betweenness = betweenness(sports_QAAll_2014_g)
sports_QAAll_2014_vertices$closeness = closeness(sports_QAAll_2014_g)
sports_QAAll_2014_vertices$in_degree = degree(sports_QAAll_2014_g, mode = 'in')
sports_QAAll_2014_vertices$out_degree = degree(sports_QAAll_2014_g, mode = 'out')
sports_QAAll_2014_vertices$eccentricity = eccentricity(sports_QAAll_2014_g)
colnames(sports_QAAll_2014_vertices)[1] = 'user_id'
sports_QAAll_2014_users = merge(sports_QAAll_2014_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sports_QAAll_2015_g = graph.data.frame(QA_All_2015, directed = T)
sports_QAAll_2015_vertices = get.data.frame(sports_QAAll_2015_g, what='vertices')
sports_QAAll_2015_vertices$betweenness = betweenness(sports_QAAll_2015_g)
sports_QAAll_2015_vertices$closeness = closeness(sports_QAAll_2015_g)
sports_QAAll_2015_vertices$in_degree = degree(sports_QAAll_2015_g, mode = 'in')
sports_QAAll_2015_vertices$out_degree = degree(sports_QAAll_2015_g, mode = 'out')
sports_QAAll_2015_vertices$eccentricity = eccentricity(sports_QAAll_2015_g)
colnames(sports_QAAll_2015_vertices)[1] = 'user_id'
sports_QAAll_2015_users = merge(sports_QAAll_2015_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sports_QAAll_2016_g = graph.data.frame(QA_All_2016, directed = T)
sports_QAAll_2016_vertices = get.data.frame(sports_QAAll_2016_g, what='vertices')
sports_QAAll_2016_vertices$betweenness = betweenness(sports_QAAll_2016_g)
sports_QAAll_2016_vertices$closeness = closeness(sports_QAAll_2016_g)
sports_QAAll_2016_vertices$in_degree = degree(sports_QAAll_2016_g, mode = 'in')
sports_QAAll_2016_vertices$out_degree = degree(sports_QAAll_2016_g, mode = 'out')
sports_QAAll_2016_vertices$eccentricity = eccentricity(sports_QAAll_2016_g)
colnames(sports_QAAll_2016_vertices)[1] = 'user_id'
sports_QAAll_2016_users = merge(sports_QAAll_2016_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# For QA_Accepted
# all dataset
sports_QAAcc_g = graph.data.frame(QA_Accepted, directed = T)
sports_QAAcc_vertices = get.data.frame(sports_QAAcc_g, what='vertices')
sports_QAAcc_vertices$betweenness = betweenness(sports_QAAcc_g)
sports_QAAcc_vertices$closeness = closeness(sports_QAAcc_g)
sports_QAAcc_vertices$in_degree = degree(sports_QAAcc_g, mode = 'in')
sports_QAAcc_vertices$out_degree = degree(sports_QAAcc_g, mode = 'out')
sports_QAAcc_vertices$eccentricity = eccentricity(sports_QAAcc_g)
colnames(sports_QAAcc_vertices)[1] = 'user_id'
sports_QAAcc_users = merge(sports_QAAcc_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2012 dataset
sports_QAAcc_2012_g = graph.data.frame(QA_Accepted_2012, directed = T)
sports_QAAcc_2012_vertices = get.data.frame(sports_QAAcc_2012_g, what='vertices')
sports_QAAcc_2012_vertices$betweenness = betweenness(sports_QAAcc_2012_g)
sports_QAAcc_2012_vertices$closeness = closeness(sports_QAAcc_2012_g)
sports_QAAcc_2012_vertices$in_degree = degree(sports_QAAcc_2012_g, mode = 'in')
sports_QAAcc_2012_vertices$out_degree = degree(sports_QAAcc_2012_g, mode = 'out')
sports_QAAcc_2012_vertices$eccentricity = eccentricity(sports_QAAcc_2012_g)
colnames(sports_QAAcc_2012_vertices)[1] = 'user_id'
sports_QAAcc_2012_users = merge(sports_QAAcc_2012_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sports_QAAcc_2013_g = graph.data.frame(QA_Accepted_2013, directed = T)
sports_QAAcc_2013_vertices = get.data.frame(sports_QAAcc_2013_g, what='vertices')
sports_QAAcc_2013_vertices$betweenness = betweenness(sports_QAAcc_2013_g)
sports_QAAcc_2013_vertices$closeness = closeness(sports_QAAcc_2013_g)
sports_QAAcc_2013_vertices$in_degree = degree(sports_QAAcc_2013_g, mode = 'in')
sports_QAAcc_2013_vertices$out_degree = degree(sports_QAAcc_2013_g, mode = 'out')
sports_QAAcc_2013_vertices$eccentricity = eccentricity(sports_QAAcc_2013_g)
colnames(sports_QAAcc_2013_vertices)[1] = 'user_id'
sports_QAAcc_2013_users = merge(sports_QAAcc_2013_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sports_QAAcc_2014_g = graph.data.frame(QA_Accepted_2014, directed = T)
sports_QAAcc_2014_vertices = get.data.frame(sports_QAAcc_2014_g, what='vertices')
sports_QAAcc_2014_vertices$betweenness = betweenness(sports_QAAcc_2014_g)
sports_QAAcc_2014_vertices$closeness = closeness(sports_QAAcc_2014_g)
sports_QAAcc_2014_vertices$in_degree = degree(sports_QAAcc_2014_g, mode = 'in')
sports_QAAcc_2014_vertices$out_degree = degree(sports_QAAcc_2014_g, mode = 'out')
sports_QAAcc_2014_vertices$eccentricity = eccentricity(sports_QAAcc_2014_g)
colnames(sports_QAAcc_2014_vertices)[1] = 'user_id'
sports_QAAcc_2014_users = merge(sports_QAAcc_2014_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sports_QAAcc_2015_g = graph.data.frame(QA_Accepted_2015, directed = T)
sports_QAAcc_2015_vertices = get.data.frame(sports_QAAcc_2015_g, what='vertices')
sports_QAAcc_2015_vertices$betweenness = betweenness(sports_QAAcc_2015_g)
sports_QAAcc_2015_vertices$closeness = closeness(sports_QAAcc_2015_g)
sports_QAAcc_2015_vertices$in_degree = degree(sports_QAAcc_2015_g, mode = 'in')
sports_QAAcc_2015_vertices$out_degree = degree(sports_QAAcc_2015_g, mode = 'out')
sports_QAAcc_2015_vertices$eccentricity = eccentricity(sports_QAAcc_2015_g)
colnames(sports_QAAcc_2015_vertices)[1] = 'user_id'
sports_QAAcc_2015_users = merge(sports_QAAcc_2015_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sports_QAAcc_2016_g = graph.data.frame(QA_Accepted_2016, directed = T)
sports_QAAcc_2016_vertices = get.data.frame(sports_QAAcc_2016_g, what='vertices')
sports_QAAcc_2016_vertices$betweenness = betweenness(sports_QAAcc_2016_g)
sports_QAAcc_2016_vertices$closeness = closeness(sports_QAAcc_2016_g)
sports_QAAcc_2016_vertices$in_degree = degree(sports_QAAcc_2016_g, mode = 'in')
sports_QAAcc_2016_vertices$out_degree = degree(sports_QAAcc_2016_g, mode = 'out')
sports_QAAcc_2016_vertices$eccentricity = eccentricity(sports_QAAcc_2016_g)
colnames(sports_QAAcc_2016_vertices)[1] = 'user_id'
sports_QAAcc_2016_users = merge(sports_QAAcc_2016_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# for Comments_Questions
# all dataset
sportsCQ_g = graph.data.frame(Comments_Questions, directed = T)
sportsCQ_vertices = get.data.frame(sportsCQ_g, what='vertices')
sportsCQ_vertices$betweenness = betweenness(sportsCQ_g)
sportsCQ_vertices$closeness = closeness(sportsCQ_g)
sportsCQ_vertices$in_degree = degree(sportsCQ_g, mode = 'in')
sportsCQ_vertices$out_degree = degree(sportsCQ_g, mode = 'out')
sportsCQ_vertices$eccentricity = eccentricity(sportsCQ_g)
colnames(sportsCQ_vertices)[1] = 'user_id'
sportsCQ_users = merge(sportsCQ_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2012 dataset
sportsCQ_2012_g = graph.data.frame(Comments_Questions_2012, directed = T)
sportsCQ_2012_vertices = get.data.frame(sportsCQ_2012_g, what='vertices')
sportsCQ_2012_vertices$betweenness = betweenness(sportsCQ_2012_g)
sportsCQ_2012_vertices$closeness = closeness(sportsCQ_2012_g)
sportsCQ_2012_vertices$in_degree = degree(sportsCQ_2012_g, mode = 'in')
sportsCQ_2012_vertices$out_degree = degree(sportsCQ_2012_g, mode = 'out')
sportsCQ_2012_vertices$eccentricity = eccentricity(sportsCQ_2012_g)
colnames(sportsCQ_2012_vertices)[1] = 'user_id'
sportsCQ_2012_users = merge(sportsCQ_2012_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sportsCQ_2013_g = graph.data.frame(Comments_Questions_2013, directed = T)
sportsCQ_2013_vertices = get.data.frame(sportsCQ_2013_g, what='vertices')
sportsCQ_2013_vertices$betweenness = betweenness(sportsCQ_2013_g)
sportsCQ_2013_vertices$closeness = closeness(sportsCQ_2013_g)
sportsCQ_2013_vertices$in_degree = degree(sportsCQ_2013_g, mode = 'in')
sportsCQ_2013_vertices$out_degree = degree(sportsCQ_2013_g, mode = 'out')
sportsCQ_2013_vertices$eccentricity = eccentricity(sportsCQ_2013_g)
colnames(sportsCQ_2013_vertices)[1] = 'user_id'
sportsCQ_2013_users = merge(sportsCQ_2013_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sportsCQ_2014_g = graph.data.frame(Comments_Questions_2014, directed = T)
sportsCQ_2014_vertices = get.data.frame(sportsCQ_2014_g, what='vertices')
sportsCQ_2014_vertices$betweenness = betweenness(sportsCQ_2014_g)
sportsCQ_2014_vertices$closeness = closeness(sportsCQ_2014_g)
sportsCQ_2014_vertices$in_degree = degree(sportsCQ_2014_g, mode = 'in')
sportsCQ_2014_vertices$out_degree = degree(sportsCQ_2014_g, mode = 'out')
sportsCQ_2014_vertices$eccentricity = eccentricity(sportsCQ_2014_g)
colnames(sportsCQ_2014_vertices)[1] = 'user_id'
sportsCQ_2014_users = merge(sportsCQ_2014_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sportsCQ_2015_g = graph.data.frame(Comments_Questions_2015, directed = T)
sportsCQ_2015_vertices = get.data.frame(sportsCQ_2015_g, what='vertices')
sportsCQ_2015_vertices$betweenness = betweenness(sportsCQ_2015_g)
sportsCQ_2015_vertices$closeness = closeness(sportsCQ_2015_g)
sportsCQ_2015_vertices$in_degree = degree(sportsCQ_2015_g, mode = 'in')
sportsCQ_2015_vertices$out_degree = degree(sportsCQ_2015_g, mode = 'out')
sportsCQ_2015_vertices$eccentricity = eccentricity(sportsCQ_2015_g)
colnames(sportsCQ_2015_vertices)[1] = 'user_id'
sportsCQ_2015_users = merge(sportsCQ_2015_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sportsCQ_2016_g = graph.data.frame(Comments_Questions_2016, directed = T)
sportsCQ_2016_vertices = get.data.frame(sportsCQ_2016_g, what='vertices')
sportsCQ_2016_vertices$betweenness = betweenness(sportsCQ_2016_g)
sportsCQ_2016_vertices$closeness = closeness(sportsCQ_2016_g)
sportsCQ_2016_vertices$in_degree = degree(sportsCQ_2016_g, mode = 'in')
sportsCQ_2016_vertices$out_degree = degree(sportsCQ_2016_g, mode = 'out')
sportsCQ_2016_vertices$eccentricity = eccentricity(sportsCQ_2016_g)
colnames(sportsCQ_2016_vertices)[1] = 'user_id'
sportsCQ_2016_users = merge(sportsCQ_2016_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# for Comments_Answers
# all dataset
sportsCA_g = graph.data.frame(Comments_Answers, directed = T)
sportsCA_vertices = get.data.frame(sportsCA_g, what='vertices')
sportsCA_vertices$betweenness = betweenness(sportsCA_g)
sportsCA_vertices$closeness = closeness(sportsCA_g)
sportsCA_vertices$in_degree = degree(sportsCA_g, mode = 'in')
sportsCA_vertices$out_degree = degree(sportsCA_g, mode = 'out')
sportsCA_vertices$eccentricity = eccentricity(sportsCA_g)
colnames(sportsCA_vertices)[1] = 'user_id'
sportsCA_users = merge(sportsCA_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2012 dataset
sportsCA_2012_g = graph.data.frame(Comments_Answers_2012, directed = T)
sportsCA_2012_vertices = get.data.frame(sportsCA_2012_g, what='vertices')
sportsCA_2012_vertices$betweenness = betweenness(sportsCA_2012_g)
sportsCA_2012_vertices$closeness = closeness(sportsCA_2012_g)
sportsCA_2012_vertices$in_degree = degree(sportsCA_2012_g, mode = 'in')
sportsCA_2012_vertices$out_degree = degree(sportsCA_2012_g, mode = 'out')
sportsCA_2012_vertices$eccentricity = eccentricity(sportsCA_2012_g)
colnames(sportsCA_2012_vertices)[1] = 'user_id'
sportsCA_2012_users = merge(sportsCA_2012_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sportsCA_2013_g = graph.data.frame(Comments_Answers_2013, directed = T)
sportsCA_2013_vertices = get.data.frame(sportsCA_2013_g, what='vertices')
sportsCA_2013_vertices$betweenness = betweenness(sportsCA_2013_g)
sportsCA_2013_vertices$closeness = closeness(sportsCA_2013_g)
sportsCA_2013_vertices$in_degree = degree(sportsCA_2013_g, mode = 'in')
sportsCA_2013_vertices$out_degree = degree(sportsCA_2013_g, mode = 'out')
sportsCA_2013_vertices$eccentricity = eccentricity(sportsCA_2013_g)
colnames(sportsCA_2013_vertices)[1] = 'user_id'
sportsCA_2013_users = merge(sportsCA_2013_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sportsCA_2014_g = graph.data.frame(Comments_Answers_2014, directed = T)
sportsCA_2014_vertices = get.data.frame(sportsCA_2014_g, what='vertices')
sportsCA_2014_vertices$betweenness = betweenness(sportsCA_2014_g)
sportsCA_2014_vertices$closeness = closeness(sportsCA_2014_g)
sportsCA_2014_vertices$in_degree = degree(sportsCA_2014_g, mode = 'in')
sportsCA_2014_vertices$out_degree = degree(sportsCA_2014_g, mode = 'out')
sportsCA_2014_vertices$eccentricity = eccentricity(sportsCA_2014_g)
colnames(sportsCA_2014_vertices)[1] = 'user_id'
sportsCA_2014_users = merge(sportsCA_2014_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sportsCA_2015_g = graph.data.frame(Comments_Answers_2015, directed = T)
sportsCA_2015_vertices = get.data.frame(sportsCA_2015_g, what='vertices')
sportsCA_2015_vertices$betweenness = betweenness(sportsCA_2015_g)
sportsCA_2015_vertices$closeness = closeness(sportsCA_2015_g)
sportsCA_2015_vertices$in_degree = degree(sportsCA_2015_g, mode = 'in')
sportsCA_2015_vertices$out_degree = degree(sportsCA_2015_g, mode = 'out')
sportsCA_2015_vertices$eccentricity = eccentricity(sportsCA_2015_g)
colnames(sportsCA_2015_vertices)[1] = 'user_id'
sportsCA_2015_users = merge(sportsCA_2015_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sportsCA_2016_g = graph.data.frame(Comments_Answers_2016, directed = T)
sportsCA_2016_vertices = get.data.frame(sportsCA_2016_g, what='vertices')
sportsCA_2016_vertices$betweenness = betweenness(sportsCA_2016_g)
sportsCA_2016_vertices$closeness = closeness(sportsCA_2016_g)
sportsCA_2016_vertices$in_degree = degree(sportsCA_2016_g, mode = 'in')
sportsCA_2016_vertices$out_degree = degree(sportsCA_2016_g, mode = 'out')
sportsCA_2016_vertices$eccentricity = eccentricity(sportsCA_2016_g)
colnames(sportsCA_2016_vertices)[1] = 'user_id'
sportsCA_2016_users = merge(sportsCA_2016_vertices, Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

###** Export to CSVs **###

# all dataset
write.csv(sports_users, file = 'sports_users.csv', row.names = F)
write.csv(QA, file = 'QA.csv', row.names = F)
write.csv(Comments_Answers, file = 'Comments_Answers.csv', row.names = F)
write.csv(Comments_Questions, file = 'Comments_Questions.csv', row.names = F)

# 2012
write.csv(sports_2012_users, file = 'sports_2012_users.csv', row.names = F)
write.csv(QA_2012, file = 'QA_2012.csv', row.names = F)
write.csv(Comments_Answers_2012, file = 'Comments_Answers_2012.csv', row.names = F)
write.csv(Comments_Questions_2012, file = 'Comments_Questions_2012.csv', row.names = F)

# 2013
write.csv(sports_2013_users, file = 'sports_2013_users.csv', row.names = F)
write.csv(QA_2013, file = 'QA_2013.csv', row.names = F)
write.csv(Comments_Answers_2013, file = 'Comments_Answers_2013.csv', row.names = F)
write.csv(Comments_Questions_2013, file = 'Comments_Questions_2013.csv', row.names = F)

# 2014
write.csv(sports_2014_users, file = 'sports_2014_users.csv', row.names = F)
write.csv(QA_2014, file = 'QA_2014.csv', row.names = F)
write.csv(Comments_Answers_2014, file = 'Comments_Answers_2014.csv', row.names = F)
write.csv(Comments_Questions_2014, file = 'Comments_Questions_2014.csv', row.names = F)

# 2015
write.csv(sports_2015_users, file = 'sports_2015_users.csv', row.names = F)
write.csv(QA_2015, file = 'QA_2015.csv', row.names = F)
write.csv(Comments_Answers_2015, file = 'Comments_Answers_2015.csv', row.names = F)
write.csv(Comments_Questions_2015, file = 'Comments_Questions_2015.csv', row.names = F)

# 2016
write.csv(sports_2016_users, file = 'sports_2016_users.csv', row.names = F)
write.csv(QA_2016, file = 'QA_2016.csv', row.names = F)
write.csv(Comments_Answers_2016, file = 'Comments_Answers_2016.csv', row.names = F)
write.csv(Comments_Questions_2016, file = 'Comments_Questions_2016.csv', row.names = F)

