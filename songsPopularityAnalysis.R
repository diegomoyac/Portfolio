##Spotify dataset##

#Download the data

setwd("~/Fall/Fall A/Professional Communication/Last speech")
spotifyData = read.csv('spotifyData.csv')
summary(spotifyData)

#Casual analysis
spotifyData$trackGenre = as.factor(spotifyData$trackGenre)
spotifyData$trackGenre = relevel(spotifyData$trackGenre, ref="pop")
simpleLm = lm(popularity~.-tempo-X-trackID-artist-albumName-trackName, data=spotifyData)
summary(simpleLm)

#top five genres
Kpop = coef(simpleLm)['trackGenrek-pop']
sad = coef(simpleLm)['trackGenresad']
hiphop = coef(simpleLm)['trackGenrehip-hop']
rock = coef(simpleLm)['trackGenrerock-n-roll']

topFive = aggregate(popularity~trackGenre, data = spotifyData, FUN = mean)

#top 5 artist
taylorSwift = subset(spotifyData, artist == 'Peso Pluma')



