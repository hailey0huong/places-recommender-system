setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')
dfBusiness <- read.csv('businessV2.csv', stringsAsFactors = FALSE)
dfBusiness <- dfBusiness[,-c(1,3,4,5,6,7,8,9,10,11)]


dfUser <- read.csv('User.csv', stringsAsFactors = FALSE)
head(dfUser)

# combining google data
google <- read.csv('google', stringsAsFactors = FALSE)
names(google) <- unlist(lapply(names(google), function(y) gsub('tag_','',y)))
google <- google[,-c(1,2,3,4,6,7,8,9,10,11,12,13)]
names(google)[1] <- "business_id"
names(google)[33] <- "breakfast...brunch"

gMat <- matrix(0,3565,81)
colnames(gMat) <- colnames(dfBusiness)

for(i in 1:1239){
  for(j in 1:ncol(google)){
    if(google[i,j] != 0){
      gMat[i,colnames(google)[j]] <- google[i,j]
    }
  }
}

write.csv(gMat,'gMat')
gMat <- read.csv('gMat', stringsAsFactors = FALSE)
gMat <- gMat[,-1]

dfBusiness <- rbind(dfBusiness,gMat)


# normalize values in dataframe (TF)
for(i in 1:nrow(dfBusiness)){
  if(dfBusiness$sum[i] != 0){
    dfBusiness$sum[i] <- sum(dfBusiness[i,2:80])
  }
}

for(i in 1:nrow(dfBusiness)){
  for(j in 2:80){
    if(dfBusiness$sum[i] != 0){
      dfBusiness[i,j] <- dfBusiness[i,j]/dfBusiness$sum[i]
    }
  }
}
  

# remove sum field  
dfBusiness <- dfBusiness[,-81]



# finding IDF
docF <- c()
for(i in 2:length(dfBusiness)){
  docF <- c(docF,sum(dfBusiness[i]))
}
names(docF) <- names(dfBusiness)[2:length(dfBusiness)]


idf <- c()
for(i in 1:length(docF)){
  idf <- c(idf, log10(28203/docF[i]))
}
names(idf) <- names(docF)





# Finding userprofiles 
contentReviews <- c()
users <- unique(dfUser$user_id)


# selecting the top 30 users by review count
counts <- c()
for(i in 1:length(users)){
  counts <- c(counts, nrow(dfUser[dfUser$user_id == users[i],]))
}
names(counts) <- users
counts <- sort(counts, decreasing = TRUE)
users <- names(counts)[1:10]

# running the lookup table
for(x in 1:length(users)){
  print(x)
  df <- dfUser[dfUser$user_id == users[x],]
  
  #print("clearing extra values")
  i <- 1
  while(i < nrow(df)){
    if(df$business_id[i] %in% dfBusiness$business_id){}
    else {
      df <- df[-i,]
      i <- i -3
    }
    i <- i+1
  }
  
  
  u <- matrix(0,nrow(dfBusiness),1)
  rownames(u) <- dfBusiness$business_id
  
  #print("adjusting stars")
  for(i in 1:nrow(df)){
    if(df$stars[i] > 3){
      u[df$business_id[i],1] <- 1
    } else if(df$stars[i] == 1 | df$stars[i] == 2 | df$stars[i] == 3){
      u[df$business_id[i],1] <- -1
    }
  }
  
  #print("calculating userProfiles")
  userProfile <- c()
  for(i in 2:ncol(dfBusiness)){
    s <- 0
    for(j in 1:nrow(dfBusiness)){
      hold <- (u[j]*dfBusiness[j,i])
      s <- s + hold
    }
    userProfile <- c(userProfile,s)
  }
  
  #print("calculating predicted scores")
  userScores <- rep(0,nrow(dfBusiness))
  for(i in 1:nrow(dfBusiness)){
    for(j in 1:length(idf)){
      userScores[i] <- userScores[i] + idf[j]*userProfile[j]*dfBusiness[i,j+1]
    }
  }
  
  #print("cleaning and saving scores")
  userScores
  names(userScores) <- dfBusiness$business_id
  remove <- which(names(userScores) %in%  df$business_id )
  userScores <- userScores[-remove]
  
  toKeep<- names(head(sort(userScores, decreasing = TRUE)))
  
  contentReviews <- rbind(contentReviews,c(users[1],toKeep))
  
  
  
  
  
  
}



write.csv(idf, 'idf.csv')
write.csv(dfBusiness, "BusinessesGoogle.csv")

