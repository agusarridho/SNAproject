library(igraph)
library(shiny)
library(networkD3)
library(dplyr)
library(ggplot2)

##########################################
# plot QA accepted network (bigger dot means higher OUT degree)
QA_Accepted_graph = graph.data.frame(QA_Accepted, sports_QAAcc_users, directed = T)

V(QA_Accepted_graph)$shape<-"circle"

V(QA_Accepted_graph)$size = 2
V(QA_Accepted_graph)$size = (degree(QA_Accepted_graph,mode = 'out')/10)+1
V(QA_Accepted_graph)$label = V(QA_Accepted_graph)$name
V(QA_Accepted_graph)$label.cex = 0.5
l <- layout.fruchterman.reingold(QA_Accepted_graph)

E(QA_Accepted_graph)$arrow.size <- 0.02
plot(QA_Accepted_graph, 
     layout=l *10, 
     vertex.shape='circle', 
     vertex.label = ifelse(degree(QA_Accepted_graph,mode = 'out') > 30, V(QA_Accepted_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network - OUT degree highlighted')

# plot QA accepted network (bigger dot means higher IN degree)
E(QA_Accepted_graph)$arrow.size <- 0.02
plot(QA_Accepted_graph, 
     layout=layout.fruchterman.reingold,
     vertex.size = (degree(QA_Accepted_graph,mode = 'in')/10)+1,
     vertex.shape='circle', 
     vertex.label = ifelse(degree(QA_Accepted_graph,mode = 'in') > 30, V(QA_Accepted_graph)$label, NA),
     vertex.label.color='black',
     vertex.label.font = 2,
     main='QA accepted network - IN degree highlighted')

##########################################
#find and plot largest clique in the QA_accepted_graph
a <- largest.cliques(QA_Accepted_graph)
# let's just take the first of the largest cliques
clique1 <- a[[1]]

# subset the original graph by passing the clique vertices
g2 <- induced.subgraph(graph=QA_Accepted_graph,vids=clique1)

# plot the clique
E(g2)$arrow.size <- 0.1
plot(g2, main='Largest clique of QA accepted network')


V(QA_Accepted_graph)$community <- optimal.community(QA_Accepted_graph)$membership




##########################################
# dynamic graph of QA_accepted using networkD3

QA_Accepted_graph

##########################################
# ploting of histograms and basic things 
# plot density of out and in degree
par(mfrow=c(1,2))
plot(density(sports_QAAcc_users$out_degree), main='Out-degree distribution')
plot(density(sports_QAAcc_users$in_degree), main='In-degree distribution')
par(mfrow=c(1,1))


# Bar chart of top 20 answer providers (total, not only accepted)
topTwenty = Freq.Ans[1:20,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty = merge(topTwenty, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty$Frequency, main="Top 20 Answer providers", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)

# Bar charts of top 20 answer providers 
par(mfrow=c(2,3))
#2012
topTwenty12 = Freq.Ans_2012[1:10,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty12 = merge(topTwenty12, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty12$Frequency, main="Top 10 Answer providers - 2012", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty12$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)
#2013
topTwenty13 = Freq.Ans_2013[1:10,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty13 = merge(topTwenty13, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty13$Frequency, main="Top 10 Answer providers - 2013", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty13$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)
#2014
topTwenty14 = Freq.Ans_2014[1:10,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty14 = merge(topTwenty14, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty14$Frequency, main="Top 10 Answer providers - 2014", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty14$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)
#2015
topTwenty15 = Freq.Ans_2015[1:10,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty15 = merge(topTwenty15, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty15$Frequency, main="Top 10 Answer providers - 2015", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty15$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)
#2016
topTwenty16 = Freq.Ans_2016[1:10,]
temp = Users %>% select(X_Id, X_DisplayName)
topTwenty16 = merge(topTwenty16, temp, by.x = 'AnswerProvider', by.y = 'X_Id')
bar = barplot(topTwenty16$Frequency, main="Top 10 Answer providers - 2016", xlab="Provider")
text(bar,par("usr")[3],labels=topTwenty16$X_DisplayName,srt=45,offset=1,adj=1,xpd=TRUE)

par(mfrow=c(1,1))

# heatmap of the network matrix
QA_Accepted_2016_graph = graph.data.frame(QA_Accepted_2016, sports_QAAcc_2016_users, directed = T)
QA_Accepted_graph_matrix = get.adjacency(QA_Accepted_2016_graph, sparse=F)
colnames(QA_Accepted_graph_matrix) = V(QA_Accepted_2016_graph)$X_DisplayName
rownames(QA_Accepted_graph_matrix) = V(QA_Accepted_2016_graph)$X_DisplayName

palf <- colorRampPalette(c("gold", "black")) 
heatmap(QA_Accepted_graph_matrix, Rowv = NA, Colv = NA, col = palf(5), 
        scale="none", margins=c(10,10) )

# get adjacency matrix
adj = get.adjacency(QA_Accepted_graph)

