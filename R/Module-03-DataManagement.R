
# dep_auto()  # figure out dependencies automatically
opts_chunk$set(
  tidy=TRUE,
  cache=FALSE,
  fig.width=8, fig.height=6, 
  out.width="6cm", out.height="4cm"
)

# Clean Output for Slides require no progress reporting
  rxOptions(reportProgress = 0)



fileName <- "C:/Users/Sushma/Private Training/CapitalOne/Fundamentals/data/MSS10K.csv"
songData <- read.csv(file=fileName)


#  View(songData)


names(songData)


summary(songData)


class(songData)


#  songData2 <- edit(songData)


artistHotttnesss <- songData$artist.hotttnesss

class(artistHotttnesss)
head(artistHotttnesss)


artistFamiliarity <- songData[[10]] 
# artistFamiliarity <- songData[["artist.hotttnesss"]]

class(artistFamiliarity)
head(artistFamiliarity)


(h <- head(artistFamiliarity))

2*h

(h-mean(h))/sd(h)


head(artistHotttnesss/artistFamiliarity)

head(songData$artist.hotttnesss/songData$artist.familiarity)


hfRatio <- artistHotttnesss/artistFamiliarity


songData$hfRatio <- hfRatio


songData$hfRatio <- songData$artist.hotttnesss/songData$artist.familiarity


#  help(Syntax)


#  prod(x)
#  sum(x)
#  length(x)
#  mean(x)
#  var(x)
#  max(x)
#  min(x)
#  range(x)
#  sd(x)
#  sort(x)
#  order(x)


(v1 <- c(1:10))
(v2 <- c(1:2))

v1+v2


v1 <- c(1:10)
v2 <- 2

v1*v2


save(songData, file="data/Module3_songData.RData")


#  rm(list=c("songData", "songSubset4"))


#  load("songObjects.RData")


#  file.remove("songObjects.RData")


#  save(list = ls(), file = ".RData")


#  save.image()


#  write.csv(songData, "data/songData.csv")


getwd()


#  setwd("C:/New/Directory/Path/Yourdata")


#  write.csv(songData, "C:/Apath/Outside/YourWD/songData.csv")


load("data/revGeocodeDF.RData")


#  class(d)
#  
#  names(d)
#  
#  View(d)
#  
#  write.csv(d, "data/revGeocodeDF.csv")

