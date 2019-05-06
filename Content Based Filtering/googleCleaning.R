setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')

go <- read.csv('google', stringsAsFactors = FALSE)
go2 <- read.csv('googlePlaces', stringsAsFactors = FALSE)


install.packages("zipcode")
library(zipcode)

data(zipcode.civicspace)
data(zipcode)

str(zipcode.civicspace)
levels(as.factor(zipcode.civicspace$state))


go2 <- go2[,c(5,13,14)]

go <- cbind(go,rep(0,nrow(go)),rep(0,nrow(go)))
names(go)[66:67] <- c("Latitude",  "Longitude")


for(i in 1:nrow(go2)){
  go$Latitude[go$place_id == go2[i,1]] <- go2[i,2]
  go$Longitude[go$place_id == go2[i,1]] <- go2[i,3]
}

write.csv(go,"googleCorrected.csv")

hold <- c()
go$state <- 0
go$city <- 0
for(i in 1:nrow(go)){
  for(j in 1:nrow(zipcode.civicspace)){
    hold <- c(hold,dist(matrix(c(go[i,66:67],zipcode.civicspace[j,4:5]), nrow = 2, byrow = TRUE))[[1]])
  }
  num <- which.min(hold)
  go$state[i] <- zipcode.civicspace$state[num]
  go$city[i] <- zipcode.civicspace$city[num]
}



g<- read.csv("/home/arcticnerd/Documents/School work/IIT/CS 571/Project/data to fix/google_nonoverlapping", stringsAsFactors = FALSE)
gFinal <- read.csv('/home/arcticnerd/Documents/School work/IIT/CS 571/Project/data to fix/gCorrected.csv', stringsAsFactors = FALSE)
gFinal$name <- 0

for(i in 1:nrow(g)){
  gFinal$name[gFinal$place_id == g$place_id[i]] <- g$name[i]
}


write.csv(gFinal,"googleWithZip4.csv")

unique(gFinal$name)


