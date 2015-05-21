#Your data analysis must address the following questions:
  
# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

# Across the United States, which types of events have the greatest economic consequences?

setwd("D:/R/RepResearch")

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="repdata-data-StormData.csv.bz2")

data.file<-bzfile("repdata-data-StormData.csv.bz2")

data<-read.csv(data.file)

# structure of the data base
str(data)

# summaries understanding
summary(data$INJURIES)
summary(data$FATALITIES)
summary(data$PROPDMGEXP)

levels(data$PROPDMGEXP)
levels(data$EVTYPE)
levels(data$STATE)

#cleaning
# year OK
data$YearOK=as.numeric(format(strptime(data$BGN_DATE, "%m/%d/%Y %H:%M:%S"),"%Y"))

# define a function to clean up ev type
cleanup_evtype <- function(x)
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
EVTYPECLEAN <- as.factor(sapply(data$EVTYPE, cleanup_evtype))


# analysis
barplot(table(data$STATE))

barplot(table(data$EVTYPE))

with(data, plot(STATE,EVTYPE))

with(data, plot(FATALITIES,INJURIES))

boxplot(INJURIES ~ STATE, data, xlab="State", ylab="Injuries")

boxplot(INJURIES ~ EVTYPE, data, xlab="Type", ylab="Injuries")

plot(table(data$YearOK))

plot(table(data$YearOK), type="l", xlab="Year",ylab="Yearly Events")

