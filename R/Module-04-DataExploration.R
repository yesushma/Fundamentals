
# dep_auto()  # figure out dependencies automatically
opts_chunk$set(
  width=50,
  cache=FALSE,
  fig.width=8, fig.height=6, 
  out.width="6cm", out.height="4cm"
)

# Clean Output for Slides require no progress reporting
  rxOptions(reportProgress = 0)



load("data/Module3_songData.RData")


songSubset <- songData[1:100, 1:10]

class(songSubset)
rm(songSubset)


songSubset2 <- songData[, c(1,3,3,9)]
rm(songSubset2)


songSubset3 <- songData[, c("artist.name" , "artist.hotttnesss", 
                            "artist.familiarity", "artist.location", 
                            "artist.latitude","artist.longitude", 
                            "song.hotttnesss", "tempo", "duration", 
                            "term", "hfRatio")]


songSubset3 <- songData[, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)]


colSelect <- names(songData) %in% c("artist.name" , "artist.hotttnesss", 
                                    "artist.familiarity", "artist.location", 
                                    "artist.latitude","artist.longitude", 
                                    "song.hotttnesss", "tempo", "duration", 
                                    "term", "hfRatio")

colSelect

songSubset3 <- songData[, colSelect]


summary(songSubset3)


rowSelect <- songSubset3$artist.name=="Aerosmith"
songAerosmith <- songSubset3[rowSelect, ]
dim(songAerosmith)

# alternatively, using subset() instead of []
songAerosmith <- subset(songSubset3, subset=artist.name=="Aerosmith")


summary(songAerosmith) # identify number of NA's for song.hotttnesss
summary(songAerosmith$song.hotttnesss)


mean(songAerosmith$song.hotttnesss, na.rm=TRUE) # calculate replacement value

rowIndex <- is.na(songAerosmith$song.hotttnesss) # identify


songAerosmith[rowIndex,] # subset


songAerosmith$song.hotttnesss[rowIndex] <- mean(songAerosmith$song.hotttnesss, 
                                                na.rm=TRUE) # replace

songAerosmith$song.hotttnesss # verification 1
mean(songAerosmith$song.hotttnesss) # verification 


songAerosmith


songDedup <- songSubset3[!duplicated(songSubset3[1]),]
summary(songDedup)


songDedup[songDedup$artist.name=="Aerosmith",]


for (i in unique(songSubset3$artist.name)) {
  sSub <- subset(songSubset3, artist.name==i)
  meanHot <- mean(sSub$song.hotttnesss, na.rm=TRUE)
  songDedup[songDedup$artist.name == i, "song.hotttnesss"] <- meanHot
}


#  head(songDedup)
#  summary(songDedup)


songDedup[songDedup$artist.name=="Aerosmith",]


#  str(songSubset3)
#  summary(songSubset3)


xtabs(~term,songSubset3)


songSubset3$song.hotttnesssCut <- cut(songSubset3$song.hotttnesss,breaks=quantile(songSubset3$song.hotttnesss, na.rm=TRUE)[2:5],labels=c("low","med","high"))

xtabs(~term+song.hotttnesssCut,songSubset3)


prop.table(xtabs(~term+song.hotttnesssCut,songSubset3), margin=1)


songTable <- prop.table(xtabs(~term+song.hotttnesssCut,songSubset3), margin=1)

colIndex <- order(songTable[,3], decreasing = TRUE)

songTable <- songTable[colIndex,]

head(songTable,3)



songTable2 <- xtabs(~term,songSubset3)

songTable2 <- songTable2[order(songTable2, decreasing = TRUE)]

songTable2[2:11] # rock is the most frequently occuring term


cols <- names(songSubset3)[sapply(songSubset3,is.numeric)]
cols


#  plot(songData[,cols[3:7]])


plot(songData[,cols[3:7]])


x <- songSubset3$artist.hotttnesss
y <- songSubset3$artist.familiarity


#  plot(x,y)
#  abline(lm(y~x))


plot(x,y) 
abline(lm(y~x))


summary(lm(y~x))


tempSubset <- songSubset3[songSubset3$artist.hotttnesss>0 & songSubset3$artist.familiarity>0,]
x <- tempSubset$artist.hotttnesss
y <- tempSubset$artist.familiarity


#  plot(x,y, xlab="Artist Hotttnesss", ylab="Artist Familiarity",
#       main = "Million Song Data Subset")
#  abline(lm(y~x), col="blue", lwd=3, lty=6)
#  summary(lm(y~x))


plot(x,y, xlab="Artist Hotttnesss", ylab="Artist Familiarity",
     main = "Million Song Data Subset") 
abline(lm(y~x), col="blue", lwd=3, lty=6)
summary(lm(y~x))


plot(x)


par(mfrow=c(2,2))
plot(lm(y~x))


hist(x)


plot(density(x))


#  methods(plot)
#  help(plot.data.frame)


#  load("data/songTermHot.RData")
#  
#  boxplot(song.hotttnesss~term, data=songTermHot, cex.axis=.5,
#       xlab="term", ylab="average song hotttnesss")


load("data/songTermHot.RData")

boxplot(song.hotttnesss~term, data=songTermHot, cex.axis=.5,
     xlab="term", ylab="average song hotttnesss")


dotchart(xtabs(~songTermHot$term))


barplot(xtabs(~songTermHot$term))


pie(xtabs(~songTermHot$term))



topTerms <- sort(xtabs(~term,songSubset3), decreasing=TRUE)
topTerms <- names(topTerms[2:11])


#  boxplot(song.hotttnesss~term,
#          data=songSubset3[songSubset3$term %in% topTerms,])


boxplot(song.hotttnesss~term,
        data=songSubset3[songSubset3$term %in% topTerms,])


str(songSubset3$term[songSubset3$term %in% topTerms])


songTermHot <- songSubset3[songSubset3$term %in% topTerms,]
songTermHot$term <- factor(songTermHot$term) # this is key

boxplot(song.hotttnesss~term, data=songTermHot)

#save(songTermHot, file="songTermHot.RData")


boxplot(song.hotttnesss~term, data=songTermHot)


x <- songData$song.hotttnesss
x <- (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE) 
head(x)


normfunct <- function (x=songData$song.hotttnesss) {
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE) 
}

x <- normfunct()
y <- normfunct(songData$duration)


head(songData$tempo, 20)


songData$tempoDec <- round(songData$tempo, -1)
songTemp <- songData[songData$tempoDec==180 | songData$tempoDec==90, ]

songTable <- xtabs(~songData$term)
head(songTable[order(songTable,decreasing=TRUE)],11)


# hip hop, rock and house

songTemp <- songTemp[songTemp$term=="rock" | songTemp$term=="house" | songTemp$term=="hip hop",]

songTemp[sample(nrow(songTemp), 10), c("artist.name","title","release","year")]



subset(songDedup, subset=artist.name=="Aerosmith") 
# Our goal, benchmark


artistAgg <- aggregate(.~artist.name, data=songSubset3, 
                        mean)

subset(artistAgg, subset=artist.name=="Aerosmith") 
# What happened to Aerosmith?


artistAgg2 <- aggregate(.~artist.name, data=songSubset3, mean, na.action="na.pass",  
                        na.rm=TRUE)

subset(artistAgg2, subset=artist.name=="Aerosmith")


#  aggregate(artist.name~term,
#            data=songSubset3,
#            length)


head(aggregate(artist.name~term, 
          data=songSubset3, 
          length))


normfunct <- function (x=songData$song.hotttnesss) {
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE) 
}

colNameSub <- c("artist.hotttnesss", "artist.familiarity", "tempo", "duration", "loudness", "song.hotttnesss")

x <- apply(songData[,colNameSub],2,normfunct)  


#  par(mfrow = c(2, 3))
#  sapply(names(songData[,colNameSub]), function(x) hist(songData[,x], main = x, xlab = ""))


par(mfrow = c(2, 3))
sapply(names(songData[,colNameSub]), function(x) hist(songData[,x], main = x, xlab = ""))


load("data/revGeocodeDF.RData")


#  summary(d)
#  summary(songData)
#  
#  head(d)
#  head(songData)
#  
#  View(d)
#  View(songData)



subset(songSubset3, artist.latitude == 34.23294 & artist.longitude == -102.4102)

subset(d, artist.latitude == 34.23294 & artist.longitude == -102.4102)



songSubset3 <- merge(d, songSubset3, by=c("artist.latitude", "artist.longitude"))

subset(songSubset3, artist.latitude == 34.23294 & artist.longitude == -102.4102)



save(songSubset3, file= "data/Module4_songSubset3.RData")

