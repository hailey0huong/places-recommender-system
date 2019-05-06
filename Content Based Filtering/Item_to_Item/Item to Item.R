---
title: "Content-Based Filtering"
author: "Xi Sun"
date: "April 8, 2019"
output: pdf_document
---

## Libraries

```{r,warning=FALSE,message=FALSE}
library(hopach)
```

## Loading the dataset

```{r}
business <- read.csv("C:/Users/31586/Desktop/yelp_dataset/businessV2.csv",header=TRUE)
#nrow(business) #24638
#ncol(business) #91
#head(business,100)
```

## Algorithm

```{r,message=FALSE}
randsample <- function(){
  rand_ind <- sample(1:nrow(business))
  rand <- business[rand_ind,c(3:7,10)]
  return(head(rand,5))
} #Generate list at random
list <- randsample()

sele <- readline(prompt="Please select from above: ") #This is for user input
```

## Work around

```{r,message=FALSE}
state_uq <- unique(business$state)
city_uq <- unique(business$city)
```

## Content Based Filtering

```{r}
cbf <- function(...,numberOfResult=5,state,city,postalCode,unique=FALSE){
  x_ind <- c(...)
  state_city_mat <- business[which(business$state==state &
                                   business$city==city &
                                   business$postal_code==postalCode),]
  #if (all(x_ind %in% list)){
  #  x_ind <- x_ind
  #}else{
  #  return("Please select from the list!")
  #}
  n <- length(x_ind)
  m <- nrow(business)
  if (n==0){
    return("Please select something!")
  }else{
    x_ind <- x_ind
  }
  x_0 <- rep(0,79)
  for (i in x_ind){
    x_0 <- x_0+business[i,12:90]
  }
  x_vec <- x_0/sum(x_0)
  tx_vec <- t(x_vec)
  bus_mat <- state_city_mat[,12:90]/state_city_mat[,91]
  dist_mat <- 1-distancevector(bus_mat,tx_vec,"cosangle")
  result_mat <- cbind(state_city_mat[,c(3:7,10)],dist_mat)
  result_mat2 <- result_mat[order(result_mat$dist_mat,decreasing=TRUE),]
  if (unique){
    result_order <- match(unique(result_mat2$name),state_city_mat$name)
    result <- head(result_mat2[result_order,],numberOfResult)[,-c(3:5,7)]
    rownames(result) <- c()
    return(result)
  }else{
    result <- head(result_mat2,numberOfResult)[,-c(3:5,7)]
    rownames(result) <- c()
    return(result)
  }
}
cbf(1,2,numberOfResult=10,
    state="NV",
    city="las vegas",
    postalCode=89156,
    unique=TRUE)
cbf(1,2,numberOfResult=10,
    state="NV",
    city="las vegas",
    postalCode=89156,
    unique=FALSE)
```
