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
I found a negative correlation between the these two variables
unique(beren3$event)

Question 1: There is not enough data
question 2: The x axis is too small of a scale and the data is too clumbed to read.

trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <-50
Sample1 <- sample(population1 , Size)
Sample2 <- sample(population2 , Size)
# yes they are different and so were the populations 
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
# Should be 50% or .5
ToMom <- length( grep("mom", Focus))/ length(Focus)
# 0.3076 and .1924. Doesn’t match my prediction
ToMomMom<- length( grep( "grandma_mom", Focus))/ length(Focus)
ToMomDad <- length( grep( "granpa_mom", Focus))/ length(Focus)
# Focus inst equal with relation to the maternal grandparents and the paternal grandparents. Average relatedness was 0.25. 
Sibling_01<-makeBaby(Brenda, Alan)
# I would expect 50%, a ctual was 47%.
ToSib <- length(intersect( Focus, Sibling_01))/ length(Focus)
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan)))/length( Focus))
#Shares different numbers of genes with each 1000 siblings.
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
HWE <- function(p)  {
	aa<-p^2
	ab<-2*p*(1-p)
	bb<-(1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
	}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p<- seq(from = 0, to = 1, by = 0.01)
GenoFreq<- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
# Frequency of aa increases as the frequency of alleles a aincrease in the population. As it decreases, frequency does too. Time is not shown in the plot. Geographical space is not as well.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop<- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Does not match the Hardy Weinberg expectation
Pop<-simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
# More points on the graph. 
install.packages("learnPopGen")
library(learnPopGen)
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes<-5:50
Samples<- rep(PopSizes, 5)
tExt<- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line<- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
# as population increases, the distance from the line increases. Population size increases, there is more extinction for alleles.
