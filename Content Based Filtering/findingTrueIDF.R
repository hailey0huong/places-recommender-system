setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')
dfBusiness2 <- read.csv('ContentBusinessGoogle.csv', stringsAsFactors = FALSE)
head(dfBusiness2)
dfBusiness2<- dfBusiness2[,-c(1)]

write.csv(dfBusiness2, "data.csv", na = "0")
dfBusiness2 <- read.csv('data.csv')
summary(dfBusiness2)
dfBusiness2<- dfBusiness2[,-c(1)]

# finding IDF
docF2 <- c()
for(i in 2:length(dfBusiness2)){
  docF2 <- c(docF2,sum(dfBusiness2[i]))
}
names(docF2) <- names(dfBusiness2)[2:length(dfBusiness2)]


idf2 <- c()
for(i in 1:length(docF2)){
  idf2 <- c(idf2, log10(28203/docF2[i]))
}
names(idf2) <- names(docF2)


write.csv(dfBusiness2,'BusinessGoogle4.csv')
write.csv(idf2,'idf2.csv')
