---
title: "tip_analysis"
author: "Xi Sun"
date: "May 4, 2019"
output: pdf_document
---
```{r}
library(jsonlite)
library(stringr)
library(tidyr)
library(chron)
library(dplyr)
library(lubridate)
library(ggplot2)
```

## Read "tip.json" and write "tip.csv"

```{r}
TIP <- stream_in(file("C:/Users/31586/Desktop/yelp_dataset/tip.json"))
#nrow(TIP)
#1223094
TIPind <- which(TIP$business_id %in% business$business_id)
Tip <- TIP[TIPind,]
path.tip <- "C:\\Users\\31586\\Desktop\\yelp_dataset\\"
write.csv(Tip,paste(path.tip,"tip.csv",sep=""))
```

## Read "tip.csv"

```{r}
tip <- read.csv("C:/Users/31586/Desktop/yelp_dataset/tip.csv",header=TRUE)
summary(tip)
#nrow(tip)
#556615
#nrow(business)-length(unique(tip$business_id))
#2202
```

## "compliment_count"

```{r}
compliment <- tip$compliment_count
max(compliment)
#8

table_cpm <- tapply(tip$compliment_count,tip$business_id,sum)
max(table_cpm)
max_cpm <- table_cpm[which.max(table_cpm)]
max_cpm
#68

cmpnot0_ind <- which(table_cpm!=0)
#head(table_cpm[cmpnot0_ind])
length(cmpnot0_ind)
#4057

cpm_freq <- table(table_cpm)
df.cpm_freq <- data.frame(cpm_freq)
head(df.cpm_freq)
ggplot(df.cpm_freq,aes(x=table_cpm,y=Freq))+
  geom_col()+
  geom_text(aes(label=Freq),vjust=-0.35,size=2.2)+
  ggtitle("Compliment count quantities")+
  labs(x="Compliment count",y="Frequency")
ggplot(df.cpm_freq[-1,],aes(x=table_cpm,y=Freq))+
  geom_col()+
  geom_text(aes(label=Freq),vjust=-0.35,size=2.8)+
  ggtitle("Compliment count quantities without 0 compliment")+
  labs(x="Compliment count",y="Frequency")

df.cpm <- data.frame(table_cpm)
df.cpm1 <- data.frame(rownames(df.cpm),df.cpm$table_cpm)
rownames(df.cpm1) <- 1:nrow(df.cpm)
colnames(df.cpm1) <- c("business_id","compliment_count")
head(df.cpm1)
```

## "text"

```{r}
text1 <- tip$text
text1 <- tolower(text1)
text1 <- gsub("[[:punct:]]"," ",text1)
text1 <- trimws(text1)
length(which(text1==""))

text1[grep("yum",text1)] <- "yum"
text1[grep("yummy",text1)] <- "yum"
text1[grep("excellent",text1)] <- "yum"
text1[grep("deli",text1)] <- "yum"
text1[grep("best",text1)] <- "yum"
text1[grep("amazing",text1)] <- "yum"
text1[grep("awesome",text1)] <- "yum"
text1[grep("great",text1)] <- "yum"
text1[grep("love",text1)] <- "yum"
text1[grep("try everything",text1)] <- "yum"
text1[grep("fav",text1)] <- "yum"
text1[grep("fan",text1)] <- "yum"
text1[grep("fun",text1)] <- "yum"
text1[grep("nice",text1)] <- "yum"
text1[grep("wonderful",text1)] <- "yum"
text1[grep("tasty",text1)] <- "yum"
text1[grep("perfect",text1)] <- "yum"
text1[grep("sweet",text1)] <- "yum"
text1[grep("beautiful",text1)] <- "yum"
text1[grep("cool",text1)] <- "yum"
text1[grep("omg",text1)] <- "yum"
text1[grep("incredible",text1)] <- "yum"
text1[grep("reserv",text1)] <- "yum"
text1[grep("fabulous",text1)] <- "yum"
text1[grep("heaven",text1)] <- "yum"
text1[grep("worth",text1)] <- "yum"
text1[grep("fire",text1)] <- "yum"
text1[grep("excite",text1)] <- "yum"
text1[grep("discount",text1)] <- "yum"
text1[which(text1=="a")] <- "yum"
text1[grep("ayce",text1)] <- "yum"
text1[grep("must try",text1)] <- "yum"
text1[grep("addict",text1)] <- "yum"

text1[grep("friendly",text1)] <- "good"
text1[grep("wow",text1)] <- "good"
text1[grep("yes",text1)] <- "good"
text1[grep("good",text1)] <- "good"
text1[grep("happy",text1)] <- "good"
text1[grep("fast",text1)] <- "good"
text1[grep("back",text1)] <- "good"
text1[grep("come",text1)] <- "good"
text1[grep("yay",text1)] <- "good"
text1[grep("yeah",text1)] <- "good"
text1[grep("yep",text1)] <- "good"
text1[grep("yup",text1)] <- "good"
text1[grep("early",text1)] <- "good"
text1[grep("again",text1)] <- "good"
text1[grep("here",text1)] <- "good"
text1[grep("thanks",text1)] <- "good"
text1[grep("free",text1)] <- "good"
text1[grep("everything",text1)] <- "good"
text1[grep("new",text1)] <- "good"
text1[grep("time",text1)] <- "good"
text1[grep("24",text1)] <- "good"
text1[grep("decent",text1)] <- "good"
text1[grep("round",text1)] <- "good"
text1[grep("quick",text1)] <- "good"
text1[grep("huge",text1)] <- "good"
text1[grep("drive thru",text1)] <- "good"
text1[grep("hungry",text1)] <- "good"
text1[grep("food",text1)] <- "good"
text1[grep("live",text1)] <- "good"
text1[grep("authentic",text1)] <- "good"
text1[grep("bingo",text1)] <- "good"
text1[grep("win",text1)] <- "good"
text1[grep("cute",text1)] <- "good"
text1[which(text1=="b")] <- "good"
text1[grep("woohoo",text1)] <- "good"
text1[grep("right",text1)] <- "good"
text1[grep("do it",text1)] <- "good"

text1[grep("cash",text1)] <- "bad"
text1[grep("bad",text1)] <- "bad"
text1[grep("slow",text1)] <- "bad"
text1[grep("no",text1)] <- "bad"
text1[grep("poor",text1)] <- "bad"
text1[grep("over",text1)] <- "bad"
text1[grep("close",text1)] <- "bad"
text1[grep("open late",text1)] <- "bad"
text1[grep("out",text1)] <- "bad"
text1[grep("expen",text1)] <- "bad"
text1[grep("wait",text1)] <- "bad"
text1[grep("busy",text1)] <- "bad"
text1[grep("disappoint",text1)] <- "bad"
text1[grep("skip",text1)] <- "bad"
text1[grep("long",text1)] <- "bad"

text1[grep("rrible",text1)] <- "worst"
text1[grep("worst",text1)] <- "worst"
text1[grep("away",text1)] <- "worst"
text1[grep("don",text1)] <- "worst"
text1[grep("never",text1)] <- "worst"
text1[grep("else",text1)] <- "worst"
text1[grep("uck",text1)] <- "worst"
text1[grep("go home",text1)] <- "worst"
text1[grep("awful",text1)] <- "worst"
text1[grep("avoid",text1)] <- "worst"
text1[grep("dirty",text1)] <- "worst"
text1[grep("rude",text1)] <- "worst"
text1[grep("bomb",text1)] <- "worst"

text1[which(text1=="")] <- 
  ifelse(tip$compliment_count[which(text1=="")]!=0,"good","ok")

text1[which(text1!="yum"&text1!="good"&text1!="bad"&text1!="worst")] <- "ok"

tb.text1 <- table(text1)
tb.text1 <- tb.text1[order(tb.text1,decreasing=TRUE)]
head(tb.text1)

text2 <- c()
text2[which(text1=="yum")] <- 5
text2[which(text1=="good")] <- 4
text2[which(text1=="ok")] <- 3
text2[which(text1=="bad")] <- 2
text2[which(text1=="worst")] <- 1

df.text2 <- data.frame(tip$business_id,text2)
stars <- as.data.frame(tapply(df.text2$text2,df.text2$tip.business_id,mean))
colnames(stars) <- "star"
df.stars <- data.frame(rownames(star),stars$star)
colnames(df.stars) <- c("business_id","star")
head(df.stars)
```

## "date"

```{r}
date <- tip$date
user_id <- tip$user_id
business_id <- tip$business_id
df.date <- data.frame(business_id,date)
df.date <- separate(df.date,date,c("date","time"),sep=" ")
df.date <- data.frame(user_id,df.date)
head(df.date)

sunrise <- times("7:30:00")
sunset <- times("19:30:00")

time <- times(df.date$time)
day_night <- lapply(time>=sunrise&time<=sunset,ifelse,yes="day",no="night")
day_night <- unlist(day_night)

df.date[,"day_night"] <- day_night

daypercent <- length(which(day_night=="day"))/length(day_night)
nightpercent <- length(which(day_night=="night"))/length(day_night)
label.daynight <- paste(c("day","night"),
                        round(c(daypercent*100,nightpercent*100),2))
label.daynight <- paste(label.daynight,"%",sep="")
pie(table(day_night),
    label.daynight,
    col=c("red","blue"),
    main="Percentage of Tips Given at Day/Night")
```
