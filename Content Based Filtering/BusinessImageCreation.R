setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')
dfEDA <- read.csv('businessV2.csv')


library(ggplot2)

# histogram of the sum of feature count for businesses
ggplot(dfEDA, aes(x=sum)) + geom_histogram(binwidth = 0.5) +ggtitle("Tag Count by Business") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0, 10, 1)) +
   xlim(0, 11) +xlab('Tag Count') + ylab('Number of Businesses')


# histogram of the count of catagories
count <- c()
for(i in 13:length(dfEDA)-1){
  count <- c(count,sum(dfEDA[i]))
}
names(count) <- names(dfEDA)[13:length(dfEDA)-1]
count <- sort(count, decreasing = TRUE)

count2 <- count[1:20]
names(count2) <- gsub('\\.', ' ', names(count2))
names(count2) <- gsub('\\...', ' ', names(count2))

df <- data.frame(Category = names(count2), Count = count2)
ggplot(df, aes(x = reorder(Category, Count), y = Count)) + geom_bar(stat = "identity") + coord_flip() +ggtitle("Category Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_blank())



# map of restraunts
states <- map_data("state")
mp <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  # do this to leave off the color legend 
  guides(fill=FALSE)
WD <- dfEDA[,8:9]
mp + geom_point(data = WD, aes(x = longitude, y = latitude), alpha = 0.5)+ggtitle("Restaurant Locations") +
  theme(plot.title = element_text(hjust = 0.5))




