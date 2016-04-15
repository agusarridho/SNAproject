library(dplyr)
library(lubridate)

####*** Sports ***#####

# set the directory equals to the location of orginal CSV files

# read all CSVs
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
rm(temp,PostLinks, PostHistory, Badges, Comments, Tags, Votes)

# cleanse tables
Users = Users[-1,]

Posts = Posts %>%
  filter(!is.na(X_OwnerUserId))

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

# show total number of each user's questions (all dataset and yearly)
Freq.Quest_2012 = Questions %>%
  filter(Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Quest_2013 = Questions %>%
  filter(Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Quest_2014 = Questions %>%
  filter(Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Quest_2015 = Questions %>%
  filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Quest_2016 = Questions %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

# show total number of each user's answers (all dataset and yearly)
Freq.Ans_2012 = Answers %>% 
  filter(Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2013 = Answers %>% 
  filter(Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2014 = Answers %>% 
  filter(Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2015 = Answers %>% 
  filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_2016 = Answers %>% 
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

# show total number of each user's answered questions (all dataset and yearly)
Freq.Ans_Quest_2012 = Answered_Questions %>%
  filter(Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2013 = Answered_Questions %>%
  filter(Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2014 = Answered_Questions %>%
  filter(Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2015 = Answered_Questions %>%
  filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Ans_Quest_2016 = Answered_Questions %>%
  group_by(AnswerSeeker) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

# Creating QA_Accepted (accepted answer provider only -> answer seeker) tables (all dataset and yearly)
temp_Answers = Answers %>% 
  select(Answer, AnswerProvider, Year, AnswerDate)

temp_Answered_Questions = Answered_Questions %>% 
  select(AcceptedAnswer, AnswerSeeker) %>%
  rename(Answer = AcceptedAnswer)

QA_Accepted = merge(temp_Answers, temp_Answered_Questions, by = 'Answer')
QA_Accepted = QA_Accepted %>% select(AnswerProvider, AnswerSeeker, AnswerDate, Year)

QA_Accepted$Answer = NULL
QA_Accepted_2012 = QA_Accepted %>% filter(Year == 2012)
QA_Accepted_2013 = QA_Accepted %>% filter(Year == 2013 | Year == 2012)
QA_Accepted_2014 = QA_Accepted %>% filter(Year == 2014 | Year == 2013 | Year == 2012)
QA_Accepted_2015 = QA_Accepted %>% filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012)
QA_Accepted_2016 = QA_Accepted

# show total number of each user's accepted answers (all dataset and yearly)
Freq.Acc_Ans_2012 = QA_Accepted %>% 
  filter(Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Acc_Ans_2013 = QA_Accepted %>% 
  filter(Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Acc_Ans_2014 = QA_Accepted %>% 
  filter(Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Acc_Ans_2015 = QA_Accepted %>% 
  filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012) %>%
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

Freq.Acc_Ans_2016 = QA_Accepted %>% 
  group_by(AnswerProvider) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency))

# Removing columns AnswerId and Year
QA_Accepted$Year = NULL
QA_Accepted_2012$Year = NULL
QA_Accepted_2013$Year = NULL
QA_Accepted_2014$Year = NULL
QA_Accepted_2015$Year = NULL
QA_Accepted_2016$Year = NULL
QA_Accepted_2012$AnswerDate = NULL
QA_Accepted_2013$AnswerDate = NULL
QA_Accepted_2014$AnswerDate = NULL
QA_Accepted_2015$AnswerDate = NULL
QA_Accepted_2016$AnswerDate = NULL
rm(temp_Answers, temp_Answered_Questions)

###** Calculate Network's Scoring **###
library(igraph)

# For QA_Accepted
# 2012
sports_QAAcc_2012_g = graph.data.frame(QA_Accepted_2012, directed = T)
sports_QAAcc_2012_vertices = get.data.frame(sports_QAAcc_2012_g, what='vertices')
sports_QAAcc_2012_edges = get.data.frame(sports_QAAcc_2012_g, what='edges')
sports_QAAcc_2012_vertices$betweenness = betweenness(sports_QAAcc_2012_g)
sports_QAAcc_2012_vertices$closeness = closeness(sports_QAAcc_2012_g)
sports_QAAcc_2012_vertices$in_degree = degree(sports_QAAcc_2012_g, mode = 'in')
sports_QAAcc_2012_vertices$out_degree = degree(sports_QAAcc_2012_g, mode = 'out')
sports_QAAcc_2012_vertices$eccentricity = eccentricity(sports_QAAcc_2012_g)
colnames(sports_QAAcc_2012_vertices)[1] = 'user_id'
sports_QAAcc_2012_users = merge(sports_QAAcc_2012_vertices, 
                                Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2013
sports_QAAcc_2013_g = graph.data.frame(QA_Accepted_2013, directed = T)
sports_QAAcc_2013_vertices = get.data.frame(sports_QAAcc_2013_g, what='vertices')
sports_QAAcc_2013_edges = get.data.frame(sports_QAAcc_2013_g, what='edges')
sports_QAAcc_2013_vertices$betweenness = betweenness(sports_QAAcc_2013_g)
sports_QAAcc_2013_vertices$closeness = closeness(sports_QAAcc_2013_g)
sports_QAAcc_2013_vertices$in_degree = degree(sports_QAAcc_2013_g, mode = 'in')
sports_QAAcc_2013_vertices$out_degree = degree(sports_QAAcc_2013_g, mode = 'out')
sports_QAAcc_2013_vertices$eccentricity = eccentricity(sports_QAAcc_2013_g)
colnames(sports_QAAcc_2013_vertices)[1] = 'user_id'
sports_QAAcc_2013_users = merge(sports_QAAcc_2013_vertices, 
                                Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2014
sports_QAAcc_2014_g = graph.data.frame(QA_Accepted_2014, directed = T)
sports_QAAcc_2014_vertices = get.data.frame(sports_QAAcc_2014_g, what='vertices')
sports_QAAcc_2014_edges = get.data.frame(sports_QAAcc_2014_g, what='edges')
sports_QAAcc_2014_vertices$betweenness = betweenness(sports_QAAcc_2014_g)
sports_QAAcc_2014_vertices$closeness = closeness(sports_QAAcc_2014_g)
sports_QAAcc_2014_vertices$in_degree = degree(sports_QAAcc_2014_g, mode = 'in')
sports_QAAcc_2014_vertices$out_degree = degree(sports_QAAcc_2014_g, mode = 'out')
sports_QAAcc_2014_vertices$eccentricity = eccentricity(sports_QAAcc_2014_g)
colnames(sports_QAAcc_2014_vertices)[1] = 'user_id'
sports_QAAcc_2014_users = merge(sports_QAAcc_2014_vertices, 
                                Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2015
sports_QAAcc_2015_g = graph.data.frame(QA_Accepted_2015, directed = T)
sports_QAAcc_2015_vertices = get.data.frame(sports_QAAcc_2015_g, what='vertices')
sports_QAAcc_2015_edges = get.data.frame(sports_QAAcc_2015_g, what='edges')
sports_QAAcc_2015_vertices$betweenness = betweenness(sports_QAAcc_2015_g)
sports_QAAcc_2015_vertices$closeness = closeness(sports_QAAcc_2015_g)
sports_QAAcc_2015_vertices$in_degree = degree(sports_QAAcc_2015_g, mode = 'in')
sports_QAAcc_2015_vertices$out_degree = degree(sports_QAAcc_2015_g, mode = 'out')
sports_QAAcc_2015_vertices$eccentricity = eccentricity(sports_QAAcc_2015_g)
colnames(sports_QAAcc_2015_vertices)[1] = 'user_id'
sports_QAAcc_2015_users = merge(sports_QAAcc_2015_vertices, 
                                Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# 2016
sports_QAAcc_2016_g = graph.data.frame(QA_Accepted_2016, directed = T)
sports_QAAcc_2016_vertices = get.data.frame(sports_QAAcc_2016_g, what='vertices')
sports_QAAcc_2016_edges = get.data.frame(sports_QAAcc_2016_g, what='edges')
sports_QAAcc_2016_vertices$betweenness = betweenness(sports_QAAcc_2016_g)
sports_QAAcc_2016_vertices$closeness = closeness(sports_QAAcc_2016_g)
sports_QAAcc_2016_vertices$in_degree = degree(sports_QAAcc_2016_g, mode = 'in')
sports_QAAcc_2016_vertices$out_degree = degree(sports_QAAcc_2016_g, mode = 'out')
sports_QAAcc_2016_vertices$eccentricity = eccentricity(sports_QAAcc_2016_g)
colnames(sports_QAAcc_2016_vertices)[1] = 'user_id'
sports_QAAcc_2016_users = merge(sports_QAAcc_2016_vertices, 
                                Users, by.x = 'user_id', by.y = 'X_Id', all.x = T)

# Networks Evolution Table
out_degree_evo = sports_QAAcc_2016_users %>%
  select(user_id, X_DisplayName, out_degree) %>%
  arrange(desc(out_degree)) %>%
  slice(1:10) %>%
  rename(name =  X_DisplayName, out_degree_2016 = out_degree)

temp_2012 = sports_QAAcc_2012_vertices %>%
  select(user_id, out_degree) %>%
  rename(out_degree_2012 = out_degree)

temp_2013 = sports_QAAcc_2013_vertices %>%
  select(user_id, out_degree) %>%
  rename(out_degree_2013 = out_degree)

temp_2014 = sports_QAAcc_2014_vertices %>%
  select(user_id, out_degree) %>%
  rename(out_degree_2014 = out_degree)

temp_2015 = sports_QAAcc_2015_vertices %>%
  select(user_id, out_degree) %>%
  rename(out_degree_2015 = out_degree)

out_degree_evo = merge(out_degree_evo, temp_2015, all.x = T)
out_degree_evo = merge(out_degree_evo, temp_2014, all.x = T)
out_degree_evo = merge(out_degree_evo, temp_2013, all.x = T)
out_degree_evo = merge(out_degree_evo, temp_2012, all.x = T)
out_degree_evo = out_degree_evo %>% arrange(desc(out_degree_2016))

in_degree_evo = sports_QAAcc_2016_users %>%
  select(user_id, X_DisplayName, in_degree) %>%
  arrange(desc(in_degree)) %>%
  slice(1:10) %>%
  rename(name =  X_DisplayName, in_degree_2016 = in_degree)

temp_2012 = sports_QAAcc_2012_vertices %>%
  select(user_id, in_degree) %>%
  rename(in_degree_2012 = in_degree)

temp_2013 = sports_QAAcc_2013_vertices %>%
  select(user_id, in_degree) %>%
  rename(in_degree_2013 = in_degree)

temp_2014 = sports_QAAcc_2014_vertices %>%
  select(user_id, in_degree) %>%
  rename(in_degree_2014 = in_degree)

temp_2015 = sports_QAAcc_2015_vertices %>%
  select(user_id, in_degree) %>%
  rename(in_degree_2015 = in_degree)

in_degree_evo = merge(in_degree_evo, temp_2015, all.x = T)
in_degree_evo = merge(in_degree_evo, temp_2014, all.x = T)
in_degree_evo = merge(in_degree_evo, temp_2013, all.x = T)
in_degree_evo = merge(in_degree_evo, temp_2012, all.x = T)
in_degree_evo = in_degree_evo %>% arrange(desc(in_degree_2016))

rm(temp_2015, temp_2014, temp_2013, temp_2012)

###** Networks Plotting **###
# 2012
# preparing
QA_Accepted_2012_graph = graph.data.frame(QA_Accepted_2012, 
                                          sports_QAAcc_2012_users, directed = T)
V(QA_Accepted_2012_graph)$shape<-"circle"
V(QA_Accepted_2012_graph)$size = 2
V(QA_Accepted_2012_graph)$size = (degree(QA_Accepted_2012_graph,mode = 'out')/10)+1
V(QA_Accepted_2012_graph)$label = V(QA_Accepted_2012_graph)$name
V(QA_Accepted_2012_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_2012_graph)
E(QA_Accepted_2012_graph)$color = 'grey'
components = clusters (QA_Accepted_2012_graph)$ membership
colours = sample (rainbow(max(components)+1))
V (QA_Accepted_2012_graph)$color = colours[components +1]
# out_degree
E(QA_Accepted_2012_graph)$arrow.size <- 0.04
plot(QA_Accepted_2012_graph,
     layout=l *10,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2012_graph,mode = 'out') > 30, V(QA_Accepted_2012_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2012 - OUT degree highlighted')
# in_degree
E(QA_Accepted_2012_graph)$arrow.size <- 0.04
plot(QA_Accepted_2012_graph,
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_2012_graph,mode = 'in')/10)+1,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2012_graph,mode = 'in') > 30, V(QA_Accepted_2012_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2012 - IN degree highlighted')
rm(colours, components, l)

# 2013
# preparing
QA_Accepted_2013_graph = graph.data.frame(QA_Accepted_2013, 
                                          sports_QAAcc_2013_users, directed = T)
V(QA_Accepted_2013_graph)$shape<-"circle"
V(QA_Accepted_2013_graph)$size = 2
V(QA_Accepted_2013_graph)$size = (degree(QA_Accepted_2013_graph,mode = 'out')/10)+1
V(QA_Accepted_2013_graph)$label = V(QA_Accepted_2013_graph)$name
V(QA_Accepted_2013_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_2013_graph)
E(QA_Accepted_2013_graph)$color = 'grey'
components = clusters (QA_Accepted_2013_graph)$ membership
colours = sample (rainbow(max(components)+1))
V (QA_Accepted_2013_graph)$color = colours[components +1]
# out_degree
E(QA_Accepted_2013_graph)$arrow.size <- 0.04
plot(QA_Accepted_2013_graph,
     layout=l *10,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2013_graph,mode = 'out') > 30, V(QA_Accepted_2013_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2013 - OUT degree highlighted')
# in_degree
E(QA_Accepted_2013_graph)$arrow.size <- 0.04
plot(QA_Accepted_2013_graph,
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_2013_graph,mode = 'in')/10)+1,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2013_graph,mode = 'in') > 30, V(QA_Accepted_2013_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2013 - IN degree highlighted')
rm(colours, components, l)

# 2014
# preparing
QA_Accepted_2014_graph = graph.data.frame(QA_Accepted_2014, 
                                          sports_QAAcc_2014_users, directed = T)
V(QA_Accepted_2014_graph)$shape<-"circle"
V(QA_Accepted_2014_graph)$size = 2
V(QA_Accepted_2014_graph)$size = (degree(QA_Accepted_2014_graph,mode = 'out')/10)+1
V(QA_Accepted_2014_graph)$label = V(QA_Accepted_2014_graph)$name
V(QA_Accepted_2014_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_2014_graph)
E(QA_Accepted_2014_graph)$color = 'grey'
components = clusters (QA_Accepted_2014_graph)$ membership
colours = sample (rainbow(max(components)+1))
V (QA_Accepted_2014_graph)$color = colours[components +1]
# out_degree
E(QA_Accepted_2014_graph)$arrow.size <- 0.04
plot(QA_Accepted_2014_graph,
     layout=l *10,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2014_graph,mode = 'out') > 30, V(QA_Accepted_2014_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2014 - OUT degree highlighted')
# in_degree
E(QA_Accepted_2014_graph)$arrow.size <- 0.04
plot(QA_Accepted_2014_graph,
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_2014_graph,mode = 'in')/10)+1,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2014_graph,mode = 'in') > 30, V(QA_Accepted_2014_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2014 - IN degree highlighted')
rm(colours, components, l)

# 2015
# preparing
QA_Accepted_2015_graph = graph.data.frame(QA_Accepted_2015, 
                                          sports_QAAcc_2015_users, directed = T)
V(QA_Accepted_2015_graph)$shape<-"circle"
V(QA_Accepted_2015_graph)$size = 2
V(QA_Accepted_2015_graph)$size = (degree(QA_Accepted_2015_graph,mode = 'out')/10)+1
V(QA_Accepted_2015_graph)$label = V(QA_Accepted_2015_graph)$name
V(QA_Accepted_2015_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_2015_graph)
E(QA_Accepted_2015_graph)$color = 'grey'
components = clusters (QA_Accepted_2015_graph)$ membership
colours = sample (rainbow(max(components)+1))
V (QA_Accepted_2015_graph)$color = colours[components +1]
# out_degree
E(QA_Accepted_2015_graph)$arrow.size <- 0.04
plot(QA_Accepted_2015_graph,
     layout=l *10,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2015_graph,mode = 'out') > 30, V(QA_Accepted_2015_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2015 - OUT degree highlighted')
# in_degree
E(QA_Accepted_2015_graph)$arrow.size <- 0.04
plot(QA_Accepted_2015_graph,
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_2015_graph,mode = 'in')/10)+1,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2015_graph,mode = 'in') > 30, V(QA_Accepted_2015_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2015 - IN degree highlighted')
rm(colours, components, l)

# 2016
# preparing
QA_Accepted_2016_graph = graph.data.frame(QA_Accepted_2016, 
                                          sports_QAAcc_2016_users, directed = T)
V(QA_Accepted_2016_graph)$shape<-"circle"
V(QA_Accepted_2016_graph)$size = 2
V(QA_Accepted_2016_graph)$size = (degree(QA_Accepted_2016_graph,mode = 'out')/10)+1
V(QA_Accepted_2016_graph)$label = V(QA_Accepted_2016_graph)$name
V(QA_Accepted_2016_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_2016_graph)
E(QA_Accepted_2016_graph)$color = 'grey'
components = clusters (QA_Accepted_2016_graph)$ membership
colours = sample (rainbow(max(components)+1))
V (QA_Accepted_2016_graph)$color = colours[components +1]
# out_degree
E(QA_Accepted_2016_graph)$arrow.size <- 0.04
plot(QA_Accepted_2016_graph,
     layout=l *10,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2016_graph,mode = 'out') > 30, V(QA_Accepted_2016_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2016 - OUT degree highlighted')
# in_degree
E(QA_Accepted_2016_graph)$arrow.size <- 0.04
plot(QA_Accepted_2016_graph,
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_2016_graph,mode = 'in')/10)+1,
     vertex.shape='circle',
     vertex.label = ifelse(degree(QA_Accepted_2016_graph,mode = 'in') > 30, V(QA_Accepted_2016_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network 2016 - IN degree highlighted')
rm(colours, components, l)

###*** Interactive Networks QAAcc ***###
library(networkD3)

# 2012
nl = cbind(idn=factor(sports_QAAcc_2012_users$user_id, 
                      levels=sports_QAAcc_2012_users$user_id), 
           sports_QAAcc_2012_users)

el = sports_QAAcc_2012_edges
temp = nl %>% select(user_id, idn)
el = merge(el, temp, by.x = 'to', by.y = 'user_id', all.x = T)
el$to = el$idn
el$idn = NULL
el = merge(el, temp, by.x = 'from', by.y = 'user_id', all.x = T)
el$from = el$idn
el$idn = NULL
el = data.frame(from=as.numeric(el$from)-1, 
                to=as.numeric(el$to)-1 )
nl$Info = paste0(nl$X_DisplayName,', R:', nl$X_Reputation,'-U:', nl$X_UpVotes, '-D:', nl$X_DownVotes)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "out_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "in_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
rm(temp, nl, el)

# 2013
nl = cbind(idn=factor(sports_QAAcc_2013_users$user_id, 
                      levels=sports_QAAcc_2013_users$user_id), 
           sports_QAAcc_2013_users)

el = sports_QAAcc_2013_edges
temp = nl %>% select(user_id, idn)
el = merge(el, temp, by.x = 'to', by.y = 'user_id', all.x = T)
el$to = el$idn
el$idn = NULL
el = merge(el, temp, by.x = 'from', by.y = 'user_id', all.x = T)
el$from = el$idn
el$idn = NULL
el = data.frame(from=as.numeric(el$from)-1, 
                to=as.numeric(el$to)-1 )
nl$Info = paste0(nl$X_DisplayName,', R:', nl$X_Reputation,'-U:', nl$X_UpVotes, '-D:', nl$X_DownVotes)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "out_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "in_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
rm(temp, nl, el)

# 2014
nl = cbind(idn=factor(sports_QAAcc_2014_users$user_id, 
                      levels=sports_QAAcc_2014_users$user_id), 
           sports_QAAcc_2014_users)

el = sports_QAAcc_2014_edges
temp = nl %>% select(user_id, idn)
el = merge(el, temp, by.x = 'to', by.y = 'user_id', all.x = T)
el$to = el$idn
el$idn = NULL
el = merge(el, temp, by.x = 'from', by.y = 'user_id', all.x = T)
el$from = el$idn
el$idn = NULL
el = data.frame(from=as.numeric(el$from)-1, 
                to=as.numeric(el$to)-1 )
nl$Info = paste0(nl$X_DisplayName,', R:', nl$X_Reputation,'-U:', nl$X_UpVotes, '-D:', nl$X_DownVotes)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "out_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "in_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
rm(temp, nl, el)

# 2015
nl = cbind(idn=factor(sports_QAAcc_2015_users$user_id, 
                      levels=sports_QAAcc_2015_users$user_id), 
           sports_QAAcc_2015_users)

el = sports_QAAcc_2015_edges
temp = nl %>% select(user_id, idn)
el = merge(el, temp, by.x = 'to', by.y = 'user_id', all.x = T)
el$to = el$idn
el$idn = NULL
el = merge(el, temp, by.x = 'from', by.y = 'user_id', all.x = T)
el$from = el$idn
el$idn = NULL
el = data.frame(from=as.numeric(el$from)-1, 
                to=as.numeric(el$to)-1 )
nl$Info = paste0(nl$X_DisplayName,', R:', nl$X_Reputation,'-U:', nl$X_UpVotes, '-D:', nl$X_DownVotes)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "out_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "in_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
rm(temp, nl, el)

# 2016
nl = cbind(idn=factor(sports_QAAcc_2016_users$user_id, 
                      levels=sports_QAAcc_2016_users$user_id), 
           sports_QAAcc_2016_users)

el = sports_QAAcc_2016_edges
temp = nl %>% select(user_id, idn)
el = merge(el, temp, by.x = 'to', by.y = 'user_id', all.x = T)
el$to = el$idn
el$idn = NULL
el = merge(el, temp, by.x = 'from', by.y = 'user_id', all.x = T)
el$from = el$idn
el$idn = NULL
el = data.frame(from=as.numeric(el$from)-1, 
                to=as.numeric(el$to)-1 )
nl$Info = paste0(nl$X_DisplayName,', R:', nl$X_Reputation,'-U:', nl$X_UpVotes, '-D:', nl$X_DownVotes)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "out_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
forceNetwork(Links = el, Nodes = nl, Source="from", Target="to",
             NodeID = "Info", Group = "in_degree",linkWidth = 1,
             linkColour = "black", fontSize=28, zoom=T, legend=T,
             Nodesize=6, opacity = 1, charge=-600, 
             width = 1200, height = 1000)
rm(temp, nl, el)

###** Export CSVs **###
write.csv(out_degree_evo, file = "out_degree_evo.csv", row.names = F)
write.csv(in_degree_evo, file = "in_degree_evo.csv", row.names = F)
write.csv(sports_QAAcc_2016_users, file = "sports_QAAcc_2016_users.csv", row.names = F)

write.csv(sports_QAAcc_2015_vertices, file = "sports_QAAcc_2015_vertices.csv", row.names = F)
write.csv(sports_QAAcc_2014_vertices, file = "sports_QAAcc_2014_vertices.csv", row.names = F)
write.csv(sports_QAAcc_2013_vertices, file = "sports_QAAcc_2013_vertices.csv", row.names = F)
write.csv(sports_QAAcc_2012_vertices, file = "sports_QAAcc_2012_vertices.csv", row.names = F)