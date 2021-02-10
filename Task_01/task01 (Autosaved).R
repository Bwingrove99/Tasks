setwd('~/Desktop/Evolution/tasks/Task_02')
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
head(Feeds)
Feeds <- which(Data$event == 'bottle')
head(Feeds)
dayID <- apply(Data, 1, function(x) paste(x[1:3],collapse='-'))
head(dayID)
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
head(dateID)
Data$age <- dateID - dateID [which(Data$event == 'birth')]
head(data)
beren2<-Data
beren3<- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds] ,  beren3$age[Feeds], length)
head(numFeeds)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
head(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
head(berenANOVA)
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
r02b-cumulativeMilkByTime.pdf<- source("http://jonsmitchell.com/code/plotFxn02b.R")
Extracredit
beren4<- beren3[Naps,]
startHour<- (beren4$start_hour)
startMin<- (beren4$start_minute)
stopHour<- (beren4$end_hour)
stopMin<- (beren4$end_minute)
startHour
startMin
stopHour
stopMin
beren4$sleepTime<- ((stopHour - startHour)*60)+(stopMin-startMin)
beren4
totalNap<- tapply(beren4$sleepTime, beren4$age, sum)
totalNap
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalNap)),totalNap, type="b",pch=16,xlab="age in days",ylab="Nap time in minutes")
cor.test(beren4$start_hour,beren4$sleepTime)
#I found a negative correlation between the these two variables
unique(beren3$event)

#Question 1: There is not enough data
#question 2: The x axis is too small of a scale and the data is too clumbed to read.

Diaper<- which(beren3$event == "bowel")
Diaperdata <- beren3[Diaper,]
head(Diaperdata)
totalFeed
unique(Diaperdata$age)
DiaperDays<- unique(Diaperdata$age)
totalFeed
totalFeed[as.character(DiaperDays)]
DiaperDays
as.character(DiaperDays)
DiaperDaysNA<- as.character(DiaperDays)
na.omit(as.data.frame(x))
DiaperDaysFeeds<- na.omit(as.data.frame(DiaperDaysNA))[,1]
NoDiaper<- setdiff(names(totalFeed), as.character(DiaperDays))
NoDiaper
NoDiaperFeeds<- totalFeed [NoDiaper]
NoDiaperFeeds
boxplot(DiaperDaysNA, NoDiaperFeeds)
t.test(DiaperDaysNA, NoDiaperFeeds)

