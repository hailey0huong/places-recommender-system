setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')
dfBus <- read.csv('business.csv')
str(dfBus)

dfBus$attributes <- NULL
dfBus$categories <- NULL
dfBus$hours <- NULL

str(dfBus)
        
cor(dfBus[9:243])

library(dplyr)
sum(dfBus$japanese)

dfBus[11:243]

catCount <- list()
for(i in 11:243){
  catCount[[names(dfBus[i])]] <-sum(dfBus[,i]) 
}

catCount <- sort(unlist(catCount), decreasing = TRUE)
names(catCount)
hist(catCount)


catMat <- matrix(rep(0, 233**2), nrow=233, ncol =233, byrow =TRUE, dimnames = list(names(dfBus[11:243]), names(dfBus[11:243])))

dfBus[1,11:243]

ass <- c()
for(j in 1:nrow(dfBus)){
  temp <- c()
  for(i in 11:243){
    if(dfBus[j,i] == 1){
      temp <- c(temp, names(dfBus[i]))
    }
  }
  if(length(temp) != 8){
    for(k in 1:(8-length(temp))){
      temp <- c(temp, 'NA')
    }
  }
  ass <- rbind(ass, temp)
}

write.csv(ass, file = "MyData.csv")





library(arules)
trans <- read.transactions("MyData.csv", sep=",")
summary(trans)

inspect(trans[1:5])

f_is <- apriori(trans, parameter=list(support=0.01, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))

itemFrequencyPlot(trans, support = 0.005)
image(trans)

rules <- apriori(trans)

rules <- apriori(trans, parameter = list(support=0.001))
summary(rules)

inspect(rules, by="confidence")

conf1 <- inspect(rules, by="confidence")
conf1 <- conf1[conf1$confidence ==1,]


fastFood <- dfBus[dfBus$fast.food ==1, ]

sum(fastFood$stars)/nrow(fastFood)

range(fastFood$stars)

hist(fastFood$star)

catCount
hold <- dfBus




conf1[1,1]
catCount[which(names(catCount) == "salvadoran")]
length(strsplit(as.character(conf1[49,1]),split=","))
gsub("[{,}]", "", " {teppanyaki}")
hold[,which(names(hold) == 'teppanyaki')] <- NULL

rCat <- c()
for(i in 1:nrow(conf1)){
  s <-unlist(strsplit(as.character(conf1[i,1]),split=","))
  if(length(s) == 1){
    s <- gsub("[{,}]", "", s)
    hold[,which(names(hold) == s)] <- NULL
    rCat <- c(rCat, s)
  }
}


hold$american <- ifelse(hold$american..new. + hold$american..traditional. >=1, 1, 0)

hold$bars <- ifelse(hold$bars + hold$wine.tours +hold$beach.bars + hold$club.crawl >=1, 1, 0)

hold$american..new. <- NULL
hold$american..traditional. <- NULL
hold$wine.tours <- NULL
hold$beach.bars <- NULL
hold$club.crawl <- NULL

x <- catCount

x <- x[x<=100]
names(x)

for(i in 1:length(names(x))){
  hold[,which(names(hold) == names(x)[i])] <- NULL
}


catCount <- list()
for(i in 11:length(hold)){
  catCount[[names(hold[i])]] <-sum(hold[,i]) 
}

catCount <- sort(unlist(catCount), decreasing = TRUE)
catCount




temp <- hold

for(i in 1:nrow(hold)){
  temp$sum[i] <- sum(hold[i,11:length(hold)]) 
}
temp$sum

temp <- temp[temp$sum != 0, ]

toPass <- names(temp)[11:79]

toPass <- gsub("[.]", " ", toPass)


library(stringr)

toPass <- str_squish(toPass)

write.table(toPass, file = "cat.txt", row.names = FALSE)






write.table(mtcars, file = "mtcars.txt", sep = "\t",
            row.names = TRUE, col.names = NA)




> string <- "  Hi buddy   what's up   Bro "
> str_squish(string)






catCount[which(names(catCount) == "salvadoran")]


hold$american <- ifelse(hold$american..new. + hold$american..traditional. >=1, 1, 0)


hold$wine <- ifelse(hold$wine.tasting.room +hold$wine.tours + hold$wine.tasting.room >=1, 1,0) 

hold$pubs <- ifelse(hold$pubs + hold$pub.food >=1, 1,0)

hold$bars <- ifelse(hold$bars + hold$beach.bars +hold$club.crawl + hold$beer.garden +hold$champagne.bars + hold$distilleries + hold$piano.bars + hold$bartenders +hold$tiki.bars +
  +hold$speakeasies + hold$gay.bars + hold$beer.bar +hold$dive.bars >=1, 1, 0)
  
hold$sushi <- ifelse(hold$sushi.bars + hold$conveyor.belt.sushi >=1, 1, 0)

hold$japanese <- ifelse(hold$japanese + hold$japanese.curry >= 1, 1,0)



hold$cofes <- ifelse(hold$cafes + hold$coffee...tea >= 1, 1,0)




hold$american..new. <- NULL
hold$american..traditional. <- NULL 
hold$wine.tasting.room <- NULL 
hold$wine.tours <- NULL 
hold$wine.tasting.room <- NULL 
hold$beach.bars <- NULL 
hold$club.crawl <- NULL 
hold$beer.garden <- NULL 
hold$champagne.bars <- NULL 
hold$distilleries <- NULL 
hold$piano.bars <- NULL 
hold$bartenders <- NULL 
hold$tiki.bars <- NULL 
hold$speakeasies <- NULL 
hold$gay.bars <- NULL 
hold$sushi.bars <- NULL 
hold$conveyor.belt.sushi <- NULL 
hold$japanese <- NULL 
hold$japanese.curry <- NULL 
hold$beer.bar <- NULL
hold$dive.bars <- NULL

catCount <- list()
for(i in 11:length(hold)){
  catCount[[names(hold[i])]] <-sum(hold[,i]) 
}

catCount <- sort(unlist(catCount), decreasing = TRUE)








hold <- dfBus

hold$american <- ifelse(hold$american..new. + hold$american..traditional. >=1, 1, 0)

hold$bars <- ifelse(hold$bars + hold$wine.tours +hold$beach.bars + hold$club.crawl >=1, 1, 0)

hold$american..new. <- NULL
hold$american..traditional. <- NULL
hold$wine.tours <- NULL
hold$beach.bars <- NULL
hold$club.crawl <- NULL

catCount <- list()
for(i in 11:length(hold)){
  catCount[[names(hold[i])]] <-sum(hold[,i]) 
}

catCount <- sort(unlist(catCount), decreasing = TRUE)
catCount


x <- catCount

x <- x[x<=100]
names(x)

for(i in 1:length(names(x))){
  hold[,which(names(hold) == names(x)[i])] <- NULL
}


catCount <- list()
for(i in 11:length(hold)){
  catCount[[names(hold[i])]] <-sum(hold[,i]) 
}
catCount <- sort(unlist(catCount), decreasing = TRUE)
catCount



temp <- hold

for(i in 1:nrow(hold)){
  temp$sum[i] <- sum(hold[i,11:length(hold)]) 
}
temp$sum

temp <- temp[temp$sum != 0, ]

toPass <- names(temp)[11:79]

toPass <- gsub("[.]", " ", toPass)

write.csv(temp, file = "businessV2.csv")
