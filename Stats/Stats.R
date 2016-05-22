
# function to convert
require(XML)
require(plyr)
search()

xmlDataDumpToDataFrame<- function(xmlFilePath){
  
  doc <- xmlParse(xmlFilePath)  
  xmlList<- xmlToList(doc)
  
  total<-length(xmlList)
  print(paste0("Parsing file : ", xmlFilePath ))
  progressBar <- txtProgressBar(min = 0, max = total, style = 3)
  data<-data.frame()
  
  for(i in 1: total){
    data <- rbind.fill(data,as.data.frame(as.list( xmlList[[i]])))
    setTxtProgressBar(progressBar, i)
  }
  
  close(progressBar)
  print(paste0("created object size : ", format(object.size(data), units= "Mb") ))
  return(data)
}

Users <- xmlDataDumpToDataFrame("Users.xml")
save(Users, file = 'users.RData')

library(dplyr)
library(lubridate)

# set the directory equals to the location of orginal CSV files

# load sentiment
Sentiment = read.csv('sentiment.csv', header = T)

# backup original
UsersOri = Users
PostsOri = Posts

# remove admin
Users = UsersOri
Users = Users[-1,]
Users = Users %>% rename(UserId = Id)

# filter posts with owner
Posts = PostsOri
Posts = Posts %>% filter(!is.na(OwnerUserId))

# join sentiment
Posts$Id = as.numeric(levels(Posts$Id)[Posts$Id])
Posts = left_join(Posts, Sentiment)

# filter questions
Questions = Posts %>% filter(PostTypeId == 1)
Questions$PostTypeId = NULL
Questions$ParentId = NULL
Questions$OwnerDisplayName = NULL
Questions$LastEditorDisplayName = NULL
Questions$ClosedDate = NULL
Questions$CommunityOwnedDate = NULL
Questions$LastActivityDate = NULL
Questions$LastEditDate = NULL
Questions$LastEditorUserId = NULL
Questions = Questions %>% 
  rename(QuestionId = Id, QuestionCreationDate = CreationDate,
         QuestionScore = Score, QuestionViewCount = ViewCount,
         QuestionBody = Body, QuestionOwnerUserId = OwnerUserId,
         QuestionTitle = Title, QuestionAnswerCount = AnswerCount,
         QuestionTags = Tags, QuestionFavoriteCount = FavoriteCount,
         QuestionCommentCount = CommentCount, QuestionSentiment = Sentiment)
Questions$QuestionTotalTags = sapply(regmatches(
  Questions$QuestionTags, gregexpr("<", Questions$QuestionTags)), length)

# filter answers
Answers = Posts %>% filter(PostTypeId == 2)
Answers$AcceptedAnswerId = NULL
Answers$PostTypeId = NULL
Answers$Title = NULL
Answers$Tags = NULL
Answers$ViewCount = NULL
Answers$AnswerCount = NULL
Answers$FavoriteCount = NULL
Answers$ClosedDate = NULL
Answers$OwnerDisplayName = NULL
Answers$LastEditorDisplayName = NULL
Answers$LastEditorUserId = NULL
Answers$LastEditDate = NULL
Answers$CommunityOwnedDate = NULL
Answers$LastActivityDate = NULL
Answers = Answers %>% 
  rename(QuestionId = ParentId, AnswerId = Id,
         AnswerCreationDate = CreationDate, AnswerScore = Score,
         AnswerOwnerUserId = OwnerUserId, AnswerBody = Body,
         AnswerCommentCount = CommentCount, AnswerSentiment = Sentiment)

# join questions answers
Answers$QuestionId = as.numeric(levels(Answers$QuestionId)[Answers$QuestionId])
QA = inner_join(Answers, Questions)

# dependent variable accepted
QA$AcceptedAnswerId = as.numeric(levels(QA$AcceptedAnswerId)[QA$AcceptedAnswerId])
QA$Accepted = ifelse(QA$AnswerId == QA$AcceptedAnswerId, 1, 0)
QA$Accepted = ifelse(is.na(QA$Accepted), 0, QA$Accepted)

# responsiveness (time difference between question and answer)
QA$TimeDifference = ymd_hms(as.character(QA$AnswerCreationDate)) - 
  ymd_hms(as.character(QA$QuestionCreationDate))

# filter accepted answers
QA_Accept = QA %>% filter(Accepted == 1)

# graph
library(igraph)
stats_g = QA %>% select(AnswerOwnerUserId, QuestionOwnerUserId)
stats_g = graph.data.frame(stats_g, directed = T)
stats_v = get.data.frame(stats_g, what='vertices')
stats_v$in_degree_all = degree(stats_g, mode = 'in')
stats_v$out_degree_all = degree(stats_g, mode = 'out')

stats_g_acc = QA_Accept %>% select(AnswerOwnerUserId, QuestionOwnerUserId)
stats_g_acc = graph.data.frame(stats_g_acc, directed = T)
stats_v_acc = get.data.frame(stats_g_acc, what='vertices')
stats_v_acc$in_degree_acc = degree(stats_g_acc, mode = 'in')
stats_v_acc$out_degree_acc = degree(stats_g_acc, mode = 'out')

stats_v = left_join(stats_v, stats_v_acc)
stats_v$in_degree_acc = ifelse(is.na(stats_v$in_degree_acc), 0, stats_v$in_degree_acc)
stats_v$out_degree_acc = ifelse(is.na(stats_v$out_degree_acc), 0, stats_v$out_degree_acc)
stats_v$page_rank = page.rank(stats_g, damping = 0.1)$vector
stats_v = stats_v %>% rename(UserId = name)

# create sna table
stats_v$UserId = as.numeric(stats_v$UserId)
Users$UserId = as.numeric(levels(Users$UserId)[Users$UserId])
SNA = left_join(stats_v, Users)
SNA$ProfileImage = ifelse(is.na(SNA$ProfileImageUrl), 0, 1)
SNA$ProfileImageUrl = NULL
SNA$LastAccessDate = NULL

# create master table
SNA = SNA %>% rename(AnswerOwnerUserId = UserId)
SNA$Reputation = as.numeric(levels(SNA$Reputation)[SNA$Reputation])
QA$AnswerOwnerUserId = as.numeric(levels(QA$AnswerOwnerUserId)[QA$AnswerOwnerUserId])
master = left_join(QA, SNA, by = 'AnswerOwnerUserId')

# computing length
library(stringi)
master$QuestionTitleLength = sapply(gregexpr("[[:alpha:]]+", master$QuestionTitle), function(x) sum(x > 0))
master$QuestionBodyLength = sapply(gregexpr("[[:alpha:]]+", master$QuestionBody), function(x) sum(x > 0))
master$AnswerBodyLength = sapply(gregexpr("[[:alpha:]]+", master$AnswerBody), function(x) sum(x > 0))
master$AboutMeLength = sapply(gregexpr("[[:alpha:]]+", master$AboutMe), function(x) sum(x > 0))
master$Website = ifelse(is.na(master$WebsiteUrl), 0, 1)

# compute time from user creation date
master$TimeFromRegister = now() - ymd_hms(as.character(master$CreationDate))

# cleanse master
master$AnswerCreationDate = NULL
master$QuestionCreationDate = NULL
master$QuestionTags = NULL
master$QuestionOwnerUserId = NULL
master$QuestionTitle = NULL
master$AccountId = NULL
master$CreationDate = NULL
master$QuestionId = NULL
master$AnswerOwnerUserId = NULL
master$DisplayName = NULL
master$WebsiteUrl = NULL
master$AboutMe = NULL  
master$Location = NULL
master$AcceptedAnswerId = NULL
master$AnswerBody = NULL
master$QuestionBody = NULL
master$Age = NULL

# factor conversion
str(master)
master$AnswerScore = as.numeric(levels(master$AnswerScore)[master$AnswerScore])
master$AnswerCommentCount = as.numeric(levels(master$AnswerCommentCount)[master$AnswerCommentCount])
master$UpVotes = as.numeric(levels(master$UpVotes)[master$UpVotes])
master$DownVotes = as.numeric(levels(master$DownVotes)[master$DownVotes])
master$Views = as.numeric(levels(master$Views)[master$Views])
master$QuestionScore = as.numeric(levels(master$QuestionScore)[master$QuestionScore])
master$QuestionCommentCount = as.numeric(levels(master$QuestionCommentCount)[master$QuestionCommentCount])
master$QuestionViewCount = as.numeric(levels(master$QuestionViewCount)[master$QuestionViewCount])
master$QuestionAnswerCount = as.numeric(levels(master$QuestionAnswerCount)[master$QuestionAnswerCount])
master$QuestionFavoriteCount = as.numeric(levels(master$QuestionFavoriteCount)[master$QuestionFavoriteCount])

# difftime conversion
master$TimeFromRegister = as.numeric(master$TimeFromRegister)
master$TimeDifference = as.numeric(master$TimeDifference)

# replace NA with 0
master$QuestionFavoriteCount = ifelse(
  is.na(master$QuestionFavoriteCount), 0, master$QuestionFavoriteCount)
master$AboutMeLength = ifelse(
  is.na(master$AboutMeLength), 0, master$AboutMeLength)

# check for NA in master
sapply(master, function(x) sum(is.na(x)))

# ratio between acc and all
master$in_degree_ratio = master$in_degree_acc / master$in_degree_all
master$in_degree_ratio = ifelse(is.na(master$in_degree_ratio), 0, master$in_degree_ratio)
master$out_degree_ratio = master$out_degree_acc / master$out_degree_all

# running logistic regression
train = master
train$AnswerId = NULL
logreg = glm(Accepted ~ ., data=train, family = 'gaussian')
summary(logreg)

# create users importance 
Importance = SNA %>% select(Reputation, out_degree_all, out_degree_acc, page_rank)

# feature scaling
Importance$Reputation = Importance$Reputation / diff(range(Importance$Reputation))
Importance$out_degree_all = Importance$out_degree_all / diff(range(Importance$out_degree_all))
Importance$out_degree_acc = Importance$out_degree_acc / diff(range(Importance$out_degree_acc))
Importance$page_rank = Importance$page_rank / diff(range(Importance$page_rank))

# plotting
plot(log(Importance$Reputation), log(Importance$page_rank), xlab = 'log(Reputation)', ylab = 'log(PageRank)', main = 'Reputation vs PageRank')
plot(log(Importance$Reputation), log(Importance$out_degree_all), xlab = 'log(Reputation)', ylab = 'log(Out Degree for All)', main = 'Reputation vs Out Degree for All Answers')
plot(log(Importance$Reputation), log(Importance$out_degree_acc), xlab = 'log(Reputation)', ylab = 'log(Out Degree for Accepted)', main = 'Reputation vs Out Degree for Accepted Answers')

# correlation
cor_mat = cor(Importance, method = 'pearson')
