# WARNING
# Only use this if you only have xml files
# If you already have csv files, it's better to use them
# because this process will take a very long time to finish

# function for converting xml into dataframe
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

# execute conversion function
Posts = xmlDataDumpToDataFrame("Posts.xml")
Users = xmlDataDumpToDataFrame("Users.xml")

# remove admin user from Users dataframe
Users = Users[-1,]

# rename Id into UserId
Users = Users %>% rename(UserId = Id)

# save dataframes into csv files
write.csv(Users, file = 'users.csv', row.names = F)
write.csv(Posts, file = 'posts.csv', row.names = F)
