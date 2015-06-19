# ref 1
# https://rpubs.com/orthoeng/81518
# ref 2
# https://rpubs.com/juliayyjin/81507

# [===========================================================================================]
#  Your data analysis must address the following questions:
#    Across the United States, which types of events (as indicated in the EVTYPE_OK variable) 
#    are most harmful with respect to population health?
#    Across the United States, which types of events have the greatest economic consequences?
# [===========================================================================================]

# [===========================================================================================]
# loading libraries
library(dplyr)
library(ggplot2)
library(knitr)
# [===========================================================================================]
# Session Information
sessionInfo()
# [===========================================================================================]

# [===========================================================================================]
# loading the data
setwd("D:/R/RepResearch")

strFile<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(strFile, destfile="repdata-data-StormData.csv.bz2")

data.file<-bzfile("repdata-data-StormData.csv.bz2")

data<-read.csv(data.file)
# [===========================================================================================]

# [===========================================================================================]
# reading the structure of the data base
str(data)

# summaries understanding

# INJURIES: Number of injuries 
summary(data$INJURIES)

# FATALITIES: Number of fatalities 
summary(data$FATALITIES)

# PROPDMGEXP: Alphabetic Codes to signify magnitude “K” for thousands, “M” for millions, and “B” for billions) 
levels(data$PROPDMGEXP)
summary(data$PROPDMGEXP)

# PROPDMG: Property damage estimates, entered as actual dollar amounts 
summary(data$PROPDMG)

# CROPDMGEXP: Alphabetic Codes to signify magnitude “K” for thousands, “M” for millions, and “B” for billions)
levels(data$CROPDMGEXP)
summary(data$CROPDMGEXP)

# CROPDMG: Crop damage estimates, entered as actual dollar amounts 
summary(data$CROPDMG)

# EVTYPE_OK: Event type (Storm, Wind, Winter, etc)
levels(data$EVTYPE_OK)

# STATE: US State
levels(data$STATE)
# [===========================================================================================]

# [===========================================================================================]
#cleaning

# select just the needed columns
sel.data <- select(data, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP,BGN_DATE)



# converting the column year 
sel.data<-mutate(sel.data, Year=as.numeric(format(strptime(BGN_DATE, "%m/%d/%Y %H:%M:%S"),"%Y")))

# creating the column EVTYPE and converting lower case
sel.data$EVTYPE_OK <- tolower(data$EVTYPE) # lower case

# define a function to clean up ev type
cleanup_EVTYPE_OK <- function(x)
{
  # to upper, to make the case consistent
  res <- toupper(x)
  # subsitute space, slash, commas, etc to underscore
  res <- gsub(" +|/+|=+|,+|\\(+|\\)+$", "_", res)
  # reduce duplicate undescores
  res <- gsub("_+", "_", res)
  # remove any starting or ending underscore
  res <- gsub("^_|_$", "", res)
  # remove ending S purarl form
  gsub("S$", "", res)
}

# create a vector of the cleaned up data and set as factor
sel.data$EVTYPE_OK <- as.factor(sapply(data$EVTYPE, cleanup_EVTYPE_OK))


# cleaning the Type

sel.data$EVTYPE_OK <- gsub("avalance", "avalanche", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("mudslides", "mud slides", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("torndao", "tornado", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("thunderstorm wins", "wind", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("wild/forest fire", "wild fire", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("wildfire", "wild fire", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("storm surge/tide", "storm surge", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("rip currents", "rip current", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("heavy surf/high surf", "high surf", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("wild fires", "wild fire", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("brush fire", "wild fire", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("ligntning", "lightning", sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK <- gsub("lighting", "lightning", sel.data$EVTYPE_OK)

tide <-grepl('tide|beach|surge',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[tide] <- "tide"

land <-grepl('land|mud|slide',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[land] <- "land slide"

urban <-grepl('urban',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[urban] <- "urban"

seas <-grepl('seas|marine',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[seas] <- "seas"

surf <-grepl('surf|current|wave|swells',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[surf] <- "wave"

storm <-grepl('storm|storms|dust|shower',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[storm] <- "storm"

fire <-grepl('fire',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[fire] <- "fire"

cold <-grepl('cold|freeze|frost/freeze|frost|icy|hypothermia',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[cold] <- "cold"

wind <-grepl('wind',sel.data$EVTYPE_OK) 
sel.data$EVTYPE_OK[wind] <- "wind"

heat<-grepl('warm|heat|hot',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[heat] <- "heat"

snow<-grepl('winter|snow|freezing|ice|blizzard',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[snow] <- "snow"

flood<-grepl('flood|rain|raising',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[flood] <- "flood"

tornado<-grepl('tornado|waterspou|funnel',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[tornado] <- "tornado"

lightning<-grepl('lightning',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[lightning] <- "lightning"

hurricane<-grepl('hurricane|tropical|typhoon',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[hurricane] <- "hurricane"

hail<-grepl('hail',sel.data$EVTYPE_OK)
sel.data$EVTYPE_OK[hail] <- "hail"

sel.data$EVTYPE_OK<-as.factor(toupper(sel.data$EVTYPE_OK))
# analysis

plot(table(sel.data$Year))

plot(table(sel.data$Year), type="l", xlab="Year",ylab="Yearly Events")

barplot(table(sel.data$STATE))

barplot(table(sel.data$EVTYPE_OK))

with(sel.data, plot(STATE,EVTYPE_OK))

with(sel.data, plot(FATALITIES,INJURIES))

boxplot(INJURIES ~ STATE, data, xlab="State", ylab="Injuries")

boxplot(INJURIES ~ EVTYPE_OK, data, xlab="Type", ylab="Injuries")

health_sum <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE_OK , data = sel.data, FUN = sum)   
health_sum$TOTAL <- health_sum$INJURIES + health_sum$FATALITIES ## make a new column with total 
arrange_health <- arrange(health_sum, desc(TOTAL))# arrange in descending order
health_ten <- arrange_health[1:10,]
health_harm <- reorder(health_ten$EVTYPE_OK, -health_ten$TOTAL)

arrange_health[1:10,]

qplot(health_harm, health_ten$TOTAL, data = health_ten, 
      stat="identity", geom = "bar", fill=EVTYPE_OK) +
  labs(title = "Top Ten Types of Events that is Most Harmful to Health",
       x = "Event Types", y = "Total Injuries + Fatalities") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
