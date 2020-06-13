#Data Vis Final Project
library(tidyverse)
library(reshape2)
library(gridExtra)
h_2015 <- read.csv('2015.csv')
h_2016 <- read.csv('2016.csv')
h_2017 <- read.csv('2017.csv')

##colnames(h_2015)<-c("Country","Region","Happiness Rank","Happiness Score",
"Standard Error","Economy","Family","Health","Freedom","Trust","Generosity" ,"Dystopia Residual")

#top 10% of 2015 data and how variables effect happiness score
h_2015[1:15,]

top15_2015 <- h_2015[1:15,]
haptopmean2015 <-mean(top15_2015$Happiness.Score)
means2015 <-(colMeans(top15_2015[sapply(top15_2015, is.numeric)]))/haptopmean2015
means2015[4:10]

#bottom 10%
bottom15_2015 <- h_2015[144:158,]
hapbottommean2015 <- mean(bottom15_2015$Happiness.Score)
bottommeans2015 <-(colMeans(bottom15_2015[sapply(bottom15_2015, is.numeric)]))/hapbottommean2015
bottommeans2015[4:10]

# Pie Chart with Percentages
slices <- c(0.17472624,0.17401361,0.12168597,0.08294748,0.04021450,0.04822559,0.35818943 ) 
lbls <- c("Economy (GDP)","Family",  "Health (Life Expectancy)","Freedom", 
          "Trust (Government Corruption)","Generosity","Dystopia Residual")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
topgraph <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, main="How Variables Effect Happiness (Top 10%)")

#Bottom 10%
slices2 <- c(0.07727785,0.15962628,0.09370198,0.10619555,0.03920589,0.07414065,0.44984324) 
lbls2 <- c("Economy (GDP)","Family",  "Health (Life Expectancy)","Freedom", 
          "Trust (Government Corruption)","Generosity","Dystopia Residual")
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels 
botgraph <- paste(lbls2,"%",sep="") # ad % to labels 
pie(slices2,labels = lbls2, main="How Variables Effect Happiness (Bottom 10%)")

grid.arrange(topgraph, bottomgraph, nrow = 1)


########## final project 



library(dplyr)
library(ggmap)
library(desc)


top_17 <- h_2017 %>%
  filter(rank(desc("Happiness.Score"))<=15)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "[your key here]", account_type = "premium", day_limit = 100)

world_data <- map_data("world")

worldmap <- ggplot(world_data, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap






problem1 #####prints final map



# Create data: 3 sets of data: Low, Medium, High:
low = h_2017[1:51,]
medium = h_2017[52:104,]
high = h_2017[105:155,]

newcol<-c(low,medium,high)

library(rworldmap)
library(ggplot2)
map.world <- map_data(map="world")

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=name_len))

gg <- gg + scale_fill_gradient(low = "green", high = "red", guide = "colourbar")
gg <- gg + coord_equal()
scale_fill
gg



library(rworldmap)
library(ggplot2)
map.world <- map_data(map="world")

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=LMH))

gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
gg <- gg + coord_equal()
gg

library(maptools)
data(wrld_simpl)
plot(wrld_simpl, 
     col = c(gray(.80), "red")[grepl("^U", wrld_simpl@data$NAME) + 1])





#####################################################################
library(rworldmap)
h_2017 <- read.csv('2017.csv',header=TRUE,as.is=TRUE)
sPDF <- joinCountryData2Map(h_2017
                            , joinCode='NAME'
                            , nameJoinColumn='Country'
                            , verbose='TRUE')
#categorise component indices
h_2017$HappinessColor <-
  ifelse(h_2017$Happiness.Score < 4.72,'red'
       ,ifelse(h_2017$Happiness.Score > 5.91,'green'
               ,'amber' ))

#count red, amber , greens per country
numReds<-
  (as.numeric(h_2017$HappinessColor =='red'))
numAmbers<-
  (as.numeric(h_2017$HappinessColor =='amber'))
numGreens<-
  (as.numeric(h_2017$HappinessColor =='green'))

#calculate HPI colour per country
h_2017$HPIcolour <-
  ifelse(h_2017$HappinessColor=='blood red'
         | numReds==1,3
         ,ifelse(numAmbers==1,2
              ,ifelse(numGreens==1,1
                  ,NA)))
#join data to map
sPDF <- joinCountryData2Map(h_2017
                            ,joinCode="NAME"
                            ,nameJoinColumn="Country")
#set colours
colourPalette <- c('navyblue'
                   ,'deepskyblue2'
                   ,'lightcyan2')

#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF
                            ,nameColumnToPlot='HPIcolour'
                            ,catMethod='categorical'
                            ,colourPalette=colourPalette
                            ,addLegend=FALSE
                            ,mapTitle='Countries Based on Happiness Rank')
#changing legendText
mapParams$legendText <-
  c('1 High'
    ,'2 Medium'
    ,'3 Low')

#add legend
do.call(addMapLegendBoxes
         , c(mapParams
             ,x='bottom'
             ,title="World Happiness Color"))
mapParams

