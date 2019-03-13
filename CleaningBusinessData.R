#install.packages("rjson")
#install.packages('RJSONIO')
#install.packages('plyr')
#install.packages("stringr")

library(rjson)
library(RJSONIO) 
library(plyr)
library(stringr)

# Sets the working directory. 
setwd('/home/arcticnerd/Documents/School work/IIT/CS 571/Project')

# Function for reading in JSON file
convertJSON <- function(f){ 
  dat <- scan(f,what=character(),sep="\n") 
  return(do.call(rbind.fill, lapply(dat,function(x) data.frame(lapply(fromJSON(x),paste,collapse=" | "),stringsAsFactors=FALSE)))) 
}

# Call to read in JSON file
dat.bus <- convertJSON("business.json")

# Passing the dataframe to another varable
restaurant <- dat.bus
# Removing all businesses that are not open
restaurant <- restaurant[restaurant$is_open == 1,]
# Deleting the is_open column
restaurant$is_open <- NULL
# Changing all values in category to lowercase
restaurant$categories <- tolower(restaurant$categories)
# Listing the values in the category field to be removed 
catItemsRemove <- c('restaurant', 'restaurants')
# Selects only the businesses that have the category restaurant
restaurant <- subset(restaurant, grepl(paste(catItemsRemove, collapse="|"), restaurant$categories))

# Fuction for removing the items listed in catItemsRemove from the category field
breakCategory <- function(inCat, pattern){
  inCat <- paste(setdiff(unlist(lapply(strsplit(inCat, ","), str_trim)), pattern), collapse = ", ")
}

# Loop for using the breakCategory function on the category field, should be replaced with an apply()
for (i in 1:nrow(restaurant)){
  restaurant$categories[i] <- breakCategory(restaurant$categories[[i]], catItemsRemove)
}

# Places a NA value in all blank category entries, this happens when all fields are removed by breakCategory
restaurant$categories[restaurant$categories == ""] <- NA

# writes the dataframe as a cvs file
write.csv(restaurant, file = "business.csv",row.names=FALSE)

