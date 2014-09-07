
# dep_auto()  # figure out dependencies automatically
opts_chunk$set(
  tidy=TRUE,
  cache=FALSE, fig.height=6, 
  out.width="6cm", out.height="4cm"
)

# Clean Output for Slides require no progress reporting
  rxOptions(reportProgress = 0)



load("data/Module3_songData.RData")
load("data/Module4_songSubset3.RData")


#  install.packages("ggplot2") # Install a package


library("ggplot2") # Load a package

load("data/Module4_songSubset3.RData")


#  help(package="ggplot2") # Get help on a package
#  
#  help("map_data") # Get help for a specific function
#  
#  #... or
#  # ?map_data


mapWorld <- map_data(map = "world")

# What does map_data() function do?

# ?map_data


# What type of object is mapWorld?

str(mapWorld)


# What type of object is mapWorld?

head(mapWorld)


#  songMap <- ggplot() +
#    geom_polygon(data=mapWorld,
#                 aes(x = long, y = lat, group = group),
#                 fill="white")
#  
#  songMap


songMap <- ggplot() + 
  geom_polygon(data=mapWorld, 
               aes(x = long, y = lat, group = group),
               fill="white")

songMap


#  songMap + geom_point(data = songSubset3,
#                       aes(x = artist.longitude,
#                           y = artist.latitude,
#                           size = hfRatio),
#                       alpha=I(.5))


songMap + geom_point(data = songSubset3, 
                     aes(x = artist.longitude, 
                         y = artist.latitude,
                         size = hfRatio),
                     alpha=I(.5))


songState <- songSubset3[!is.na(songSubset3$artist.hotttnesss) &
                         !is.na(songSubset3$artist.familiarity) &
                         !is.na(songSubset3$song.hotttnesss) &
                         !is.na(songSubset3$artist.latitude) &
                         !is.na(songSubset3$artist.longitude),]

dim(songSubset3); dim(songState)



# Note, we can identify (remove) all rows where obervations of ANY of the variables are NA using complete.cases (na.omit). 

dim(na.omit(songSubset3)); dim(songSubset3[complete.cases(songSubset3),])



songStateSub <- subset(songState, geoCountry=="United States")

summary(songStateSub)


songStateAgg <- aggregate(hfRatio~geoState, data=songStateSub, mean)


usStateMap <- map_data('state')
head(usStateMap)


mapData <- merge(songStateAgg, usStateMap, 
                 by.x = 'geoState', by.y = 'region')


head(mapData)


head(usStateMap,2)
head(songStateAgg,2)
songStateAgg$geoState <- tolower(songStateAgg$geoState)


mapData <- merge(songStateAgg, usStateMap, 
                 by.x = 'geoState', by.y = 'region',
                 all.y = TRUE)
head(mapData)


head(usStateMap,3)
head(mapData, 3) 
mapData <- mapData[order(mapData$order), ]


#  (p1 <- ggplot(data = mapData, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = cut_number(hfRatio, 5))))
#  
#  (p1 <- p1 + geom_path(colour = 'orange', linestyle = 2))
#  
#  (p1 <- p1 + scale_fill_brewer('Average Artists Hotness to Familiarity Ratio', palette  = 'PuRd'))


p1 <- ggplot(data = mapData, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = cut_number(hfRatio, 5)))

p1 <- p1 + geom_path(colour = 'orange', linestyle = 2)

(p1 <- p1 + scale_fill_brewer('Average Artists Hotness to Familiarity Ratio', palette  = 'PuRd'))


#  
#  states <- data.frame(state.center, state.abb)
#  
#  (p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2))
#  
#  (p1 <- p1 +  theme(line = element_blank(),
#              axis.text = element_blank(),
#              axis.title = element_blank(),
#              panel.background = element_blank()))
#  
#  ggsave(filename = "p1.jpeg", width=11, height=8.5)
#  



states <- data.frame(state.center, state.abb)

p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2)

(p1 <- p1 +  theme(line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank()))

ggsave(filename = "p1.jpeg", width=11, height=8.5)


