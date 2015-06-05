# dplyr 2 #
###########

setwd("~/git/dplyr-fun")

if(!require(dplyr))
    install.packages("dplyr")

if(!require(ggplot2))
    install.packages("ggplot2")

# hflights is a dataset that comes with dplyr
library(hflights)

hflights

nrow(hflights)
ncol(hflights)

# limit output of head()
head(hflights,5)

hflights <- tbl_df(hflights)

# so much cleaner.
hflights

# plyr stuff (comparison between plyr and dplyr)
plyr_routes <- ddply(hflights, .(Month, DayofMonth, Origin, Dest), plyr::summarise, flights=length(Year))
head(plyr::arrange(plyr_routes, Month, DayofMonth, desc(flights)))

# with dplyr
dplyr_routes <- arrange(
                    summarise(
                        group_by(hflights, Month, DayofMonth, Origin, Dest),
                        flights=n()),
                        Month, DayofMonth, desc(flights))

dplyr_routes <- hflights %>%
                    group_by(Month, DayofMonth, Origin, Dest) %>%
                    arrange(Month, DayofMonth, desc(flights))

# JUNK DOESN"T WORK

### MY OWN WORK ###

# aim = tally total flights per year

# dplyr base functions #
#----------------------#
# filter() – Filters the rows of data based on given criteria.
# select() – Selects specific columns of data.
# arrange() – Orders data by values in a specific column or columns.
# mutate() – Adds new columns to data.
# summarise() – Collapses a table of data using aggregate functions.

is(hflights)

hflights

# year is only 2011 - DUH!

# cancellation look-up table (lut)
# numeric values, can add another column but URGH.
glimpse(hflights)

# presenting counts for individual carriers.
# note use of stat bin and not y plotted.
ggplot(hflights, aes(x=UniqueCarrier,stat='bin')) + geom_bar()

ggplot(hflights, aes(x=UniqueCarrier,stat='bin')) + geom_bar(aes(col=(Cancelled==1)))

# Then use each function seperately.

# UniqueCarrier look-up table
UniqueCarrier_lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights$UniqueCarrier <- UniqueCarrier_lut[hflights$UniqueCarrier]

glimpse(hflights)

ggplot(hflights, aes(x=DepDelay, y=ArrDelay)) + geom_point(alpha=1/10)

ggplot(hflights, aes(x=Distance, y=ArrDelay)) + geom_point(alpha=1/10)

ggplot(hflights, aes(x=DayOfWeek, y=ArrDelay)) + geom_point(alpha=1/10)

ggplot(hflights, aes(x=DepDelay, y=ArrDelay)) + geom_point(aes(col=UniqueCarrier),alpha=1/10)

# Notes: good but all a bit slow (big(ish) data), and not totally informative.
# I need to tear the data apart to make it a litle more interesting.

Diverts <- filter(hflights, Diverted == 1)

glimpse(Diverts)

ggplot(Diverts, aes(x=UniqueCarrier,stat='bin')) + geom_bar()

# Who flies the longest routes?

AA <- filter(hflights, UniqueCarrier == "American" & DepDelay < 500) %>%
        select(DepDelay, ArrDelay, AirTime)

ggplot(AA, aes(x=DepDelay, y=ArrDelay)) + geom_point(alpha=1/10) + stat_smooth()

ggplot(AA, aes(x=AirTime, y=ArrDelay)) + geom_point(alpha=1/10)

# Back to all airlines
glimpse(hflights)

ggplot(hflights, aes(x=AirTime, y=ArrDelay)) + geom_point(aes(col=UniqueCarrier),alpha=1/10)

Miami <- filter(hflights, Dest == "MIA" & ArrDelay > 0 & DepDelay > 0 & ArrTime > 500)

ggplot(Miami, aes(x=UniqueCarrier, stat='bin')) + geom_bar()

ggplot(Miami, aes(x=DepDelay, y=ArrDelay)) + geom_point(aes(col=UniqueCarrier))

ggplot(Miami, aes(x=ArrTime,y=ArrDelay)) + geom_point(aes(col=UniqueCarrier,size=DepDelay))

arrange(Miami, DayOfWeek, DayofMonth)

ggplot(Miami, aes(x=Month,y=ArrDelay)) + geom_point()

# Adding a time value
range(hflights$Month)
range(hflights$DayofMonth)

# Adding hflights$time which gives a value of between 0 and 1 for flight time. 

hflights <- mutate(hflights, time = ((Month / 12) * (DayofMonth / 31)))

range(hflights$time)
(hflights$Month / 12)

ggplot(hflights, aes(x=time,y=ArrDelay)) + geom_point(aes(col=UniqueCarrier),alpha=1/10)

# Now looking at just Miami

Miami <- mutate(Miami, time = ((Month / 12) * (DayofMonth / 31)))

range(Miami$time)

ggplot(Miami, aes(x=time,y=ArrDelay)) + geom_point(aes(col=UniqueCarrier))

Miami <- mutate(Miami, Delay = ArrDelay + DepDelay)

ggplot(Miami, aes(x=time,y=Delay)) + geom_point(aes(col=UniqueCarrier))

Miami <- mutate(Miami, Gain = ArrDelay - DepDelay, GainPerHour = Gain / (AirTime / 60))

ggplot(Miami, aes(x=time,y=Gain)) + geom_point(aes(col=UniqueCarrier))

ggplot(Miami, aes(x=time,y=GainPerHour)) + geom_point(aes(col=UniqueCarrier)) + stat_smooth()

# FOR ALL
hflights <- mutate(hflights, Gain = ArrDelay - DepDelay, GainPerHour = Gain / (AirTime / 60))

ggplot(hflights, aes(x=time,y=Gain)) + geom_point(alpha=1/10) + stat_smooth()
ggplot(hflights, aes(x=time,y=GainPerHour)) + geom_point(alpha=1/10) + stat_smooth()

ggplot(hflights, aes(x=DepDelay,y=Gain)) + geom_point(alpha=1/10) + stat_smooth()
ggplot(hflights, aes(x=DepDelay,y=GainPerHour)) + geom_point(alpha=1/10) + stat_smooth()

glimpse(hflights)


# Difference between airtime and elapsed time

hflights <- mutate(hflights, GroundTime = ActualElapsedTime - AirTime)

ggplot(hflights, aes(x=GroundTime,stat='bin')) + geom_bar()

ggplot(hflights, aes(x=GroundTime,y=ArrDelay)) + geom_point(alpha=1/10,col="steelblue") + stat_smooth()

ggplot(hflights, aes(x=GroundTime,y=Gain)) + geom_point(aes(col=UniqueCarrier),alpha=1/10) + stat_smooth()

#################
# Testing a map #
#################

require(maps)
USA <- map_data("state")

glimpse(USA)

# quickPlot
qplot(long,lat,data=USA)

# ggplot
ggplot(USA, aes(x=long,y=lat, group=group)) + geom_polygon() + theme_bw()

glimpse(hflights)
levels(hflights,Origin)
range(hflights$Origin)


ggplot(hflights,aes(x=Dest,stat='bin')) + geom_bar()

################
# AIRPORT DATA #
################

airports <- read.csv("./global_airports.csv")
airports <- tbl_df(airports)

# load airport data
load("./WorkSpace.RData")
glimpse(hflights)

x = "LHR"
out <- filter(airports, iata_faa == x) %>% select(latitude)
as.numeric(out)

test <- vector()
for(i in 1:dim(hflights)[1]) {
    test <- filter(airports, iata_faa == hflights[i,14])$longitude
}


value <- matrix(0,dim(hflights)[1],1)
for(i in 1:dim(hflights)[1]) {
    x = toString(hflights[i,14])
    out <- filter(airports, iata_faa == x) %>% select(latitude)
    value[i,] <- as.numeric(out)
    print(paste(value[i,],i/dim(hflights)[1]))
}

hflights <- mutate(hflights, OriginLat = as.numeric(value))
glimpse(hflights)

value <- matrix(0,dim(hflights)[1],1)
for(i in 1:dim(hflights)[1]) {
    x = toString(hflights[i,14])
    out <- filter(airports, iata_faa == x) %>% select(longitude)
    value[i,] <- as.numeric(out)
    print(paste(value[i,],i))
}

hflights <- mutate(hflights, OriginLong = as.numeric(value))
glimpse(hflights)

# Test map with only OriginLong and OriginLat
ggplot(USA, aes(x=long,y=lat)) + geom_polygon(aes(group=group)) + theme_bw() +
    geom_point(data=hflights,aes(x=OriginLong,y=OriginLat),col="red")


value <- matrix(0,dim(hflights)[1],1)
for(i in 1:dim(hflights)[1]) {
    x = toString(hflights[i,15])
    out <- filter(airports, iata_faa == x) %>% select(latitude)
    value[i,] <- as.numeric(out)
    print(paste(value[i,],i))
}

hflights <- mutate(hflights, DestLat = as.numeric(value))
glimpse(hflights)

value <- matrix(0,dim(hflights)[1],1)
for(i in 1:dim(hflights)[1]) {
    x = toString(hflights[i,15])
    out <- filter(airports, iata_faa == x) %>% select(longitude)
    value[i,] <- as.numeric(out)
    print(paste(value[i,],i))
}

hflights <- mutate(hflights, DestLong = as.numeric(value))
glimpse(hflights)

ggplot(USA, aes(x=long,y=lat)) + geom_polygon(aes(group=group)) + theme_bw() +
    geom_point(data=hflights,aes(x=OriginLong,y=OriginLat,group=Origin),alpha=1/10) + 
    geom_point(data=hflights,aes(x=DestLong,y=DestLat,group=Dest),alpha=1/10) +
    geom_segment(data=hflights,aes(x=OriginLong, y=OriginLat, xend=DestLong, yend=DestLat),alpha=1/500)

save.image("./WorkSpace.RData")

