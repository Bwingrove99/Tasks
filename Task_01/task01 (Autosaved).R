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


source("http://jonsmitchell.com/code/fxn05.R")
Pop1<- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h =1, s = 0)
plot (1:nrow (Pop1), Pop1 [,1], ylim=c (0,1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit( nruns = 10, n = 50, ngens = 100, init_p=0.5, h = 1, s = 0 )
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beeside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
results<- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts<- results[,c("yellow","red","green","blue","black","tan")]
backgrounds <- c("White" ,"Red" ,"Yellow" ,"Green" ,"Blue" ,"Black")
backgroundCol <- c ("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#not very even
#Chi-squared is very even when it is low
Avg <- mean(Chisqs)
#they look similar
# chi square differs by background in my opinion
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70))/length(Chisqs)
percSig <- round(100 * propSig)
#i think that it could be a issue
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds){
Data <- Chisqs[which(results[,3] == i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter +1
}
abline( v = 11.70, lty=2, lwd=2, col='black')
#no
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
Fit<- c(1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation2<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit)<- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit<- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit)<- 1:6
Simulation6<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))

install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
model<- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats<- simulate(model, nsim = 1)
Diversity<- stats$pi
Nloci<- length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Age1 <- max(nodeHeights(t1))
t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees [[1]][1])
t1_2 <- read.tree(text=stats$trees [[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
				outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
				}
			}
		}		
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10, 50) +
feat_mutation(par_prior("theta", sample.int(100, 1))) +
sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars [["theta"]])

coalescent.plot(n=5, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
coalescent.plot(n=12, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
coalescent.plot(n=10, ngen=30, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
#The first simulation started with 5.  The next started with 12.  The last started with 10. 
coalescent.plot(n=5, ngen=35, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test) 
coalescent.plot(n=5, ngen=45, col.order="alternating")
test <-coalescent.plot()
print(test)
plot(test)
coalescent.plot(n=5, ngen=50, col.order="alternating")
coalescent.plot(n=5, ngen=49, col.order="alternating")
coalescent.plot(n=5, ngen=5, col.order="alternating")
#49 begins fixation at 10
#5 offspring
#Large impact because each simulation was different
#In generation 0

#I hypothesis that female seal pups will weigh more than the male seal pups
x<- read.table(" ~/Evolution/Tasks/Project")

source("https://jonsmitchell.com/code/reformatData07.R")
source("https://jonsmitchell.com/code/simFxn.R")
plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(0,1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[, 1], x, col=rgb(0,0,0,0.01)))
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x)x-x[1]
plot(1,1, type="n", xlim=c(1998, 2013) ylim=c(-0.25, 0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overalFreq[,1], x, col=rgb(0,0,0,0.01)))
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
dYear <- c()
dAlleles <- c()
for(i in 3:ncol(overalFreq)) {
	dYear <- c(dYear, overallFreq[,1])
	Vec <- overallFreq[,i]
	Init <- overallFreq[1,i]
	dAlleles <- c(dAlleles, Vec - Init)
]
smoothScatter(dYear, dAlleles, colramp=Pal, nbin=100)
smoothScatter(dYear, dAlleles, colramp=Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns=50, n=100, ngens=18, startT=1997, simCol="gray40", rescale=TRUE)
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab="overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')


setwd("~/Desktop/Evolution/Tasks/Task_07")
install.packages("phytools")
library("phytools")
library("ape")
text.string<- "(((((((cow, pig), whale),(bat, (lemur, human))),(robin, iguana)), coelacenth), (gold_fish, trout)), shark) ;"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels (frame="circle", bg='white', cex=1)
vert.tree
str(vert.tree)
tree<-read.tree(text="(((A,B), (C,D)), E) ;")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric
(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col= 'black', border='white', main="", xlab=" edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
tipEdges
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
tree <- read.tree(text='(((A, B), (C, D)), E);')
plot.phylo(tree, type='phylogram', show.tip.label=FALSE, edge.color='red')
Question 4:
plot.phylo(tree, type='radial')
Question 5:
plot.phylo(tree, tip.color = 'red')
Question6-8: Anolis occultis had the shortest edge length.
plot(AnolisTree, cex=0.25) 
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths == min(Lengths))
names(Lengths)
AnolisTree2 <- drop.tip(AnolisTree, 'Anolis_occultus')
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
The line never goes down because it is increasing until tje slope hits a plateau meaning that the lizards will reach an asymptote eventually but increase until then. 
fit.bd(AnolisTree, rho = 0.2)
install.packages('treebase')
library('treebase')
library('ape')
Warblers <- search_treebase("Basileuterus", by="taxon", max_trees=20)
length(Warblers)
pdf("r07-WarblerPhylo.pdf", height=5, width=5)
WarblersPlot <- plot.phylo(Warblers[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Warblers, function(x) try(is.ultrametric(x)))
WillWork
bdwarblers <- fit.bd(Warblers[[1]], rho=0.2)
bdwarblers
b=15.4316 d=8.0977
Skinks <- search_treebase("Dasia", by="taxon", max_trees=20)
length(Skinks)
pdf("r07-SkinksPhylo.pdf", height=5, width=5)
SkinksPlot <- plot.phylo(Skinks[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Skinks, function(x) try(is.ultrametric(x)))
WillWor
bdskinks <- fit.bd(Warblers[[1]], rho=0.2)
bdskinks
b=15.4316 d=8.0977
TreeFrogs <- search_treebase("Hyla", by="taxon", max_trees=20)
length(TreeFrogs)
pdf("r07-TreeFrogsPhylo.pdf", height=5, width=5)
TreeFrogsPlot <- plot.phylo(TreeFrogs[[1]], cex=0.35)
dev.off()
WillWork <- sapply(TreeFrogs, function(x) try(is.ultrametric(x)))
WillWork
bdTreeFrogs <- fit.bd(TreeFrogs[[1]], rho=0.2)
bdTreeFrogs
Skinks <- search_treebase("Dasia", by="taxon", max_trees=20)
length(Skinks)
pdf("r07-SkinksPhylo.pdf", height=5, width=5)
SkinksPlot <- plot.phylo(Skinks[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Skinks, function(x) try(is.ultrametric(x)))
WillWork
bdskinks <- fit.bd(Warblers[[1]], rho=0.2)
bdskinks
install.packages("treebase")
library("treebase")
library("ape")
Elephants <- search_treebase("Elephas", by="taxon", max_trees=20)
length(Elephants)
pdf("r07-WarblerPhylo.pdf", height=5, width=5)
ElephantsPlot <- plot.phylo(Elephants[[1]], cex=0.35
dev.off()
WillWork <- sapply(Elephants, function(x) try(is.ultrametric(x)))
WillWork
bdElephants <- fit.bd(Elephants[[1]], rho=0.2)
bdElephants
Warblers <- search_treebase("Basileuterus", by="taxon", max_trees=20)
length(Warblers)
pdf("r07-WarblerPhylo.pdf", height=5, width=5)
WarblersPlot <- plot.phylo(Warblers[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Warblers, function(x) try(is.ultrametric(x)))
WillWork
bdwarblers <- fit.bd(Warblers[[1]], rho=0.2)
bdwarblers
b=15.4316 d=8.0977
Elephants <- search_treebase("Elephas", by="taxon", max_trees=20)
length(Elephants)
pdf("r07-ElephantPhylo.pdf", height=5, width=5)
ElephantsPlot <- plot.phylo(Elephants[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Elephants, function(x) try(is.ultrametric(x)))
WillWork
bdElephants <- fit.bd(Elephants[[1]], rho=0.2)
bdElephants
pdf("r07-ElephantPhylo.pdf", height=5, width=5)
Monkeys <- search_treebase("Guenon", by="taxon", max_trees=20)
length(Monkeys)
pdf("r07-MonkeysPhylo.pdf", height=5, width=5)
MonkeysPlot <- plot.phylo(Monkeys[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Monkeys, function(x) try(is.ultrametric(x)))
WillWork
bdMonkeys <- fit.bd(Monkeys[[1]], rho=0.2)
bdMonkeys
Primates <- search_treebase("Cheirogaleus", by="taxon", max_trees=20)
length(Primates)
pdf("r07-PrimatesPhylo.pdf", height=5, width=5)
PrimatesPlot <- plot.phylo(Primates[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Primates, function(x) try(is.ultrametric(x)))
WillWork
bdPrimates <- fit.bd(Primates[[1]], rho=0.2)
bdPrimates
Tigers <- search_treebase("Tigris", by="taxon", max_trees=20)
length(Tigers)
pdf("r07-TigersPhylo.pdf", height=5, width=5)
TigersPlot <- plot.phylo(Tigers[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Tigers, function(x) try(is.ultrametric(x)))
WillWork
bdTigers <- fit.bd(Tigers[[1]], rho=0.2)
bdTigers
Tigers <- search_treebase("Tigris", by="taxon", max_trees=20)
length(Tigers)
pdf("r07-TigersPhylo.pdf", height=5, width=5)
TigersPlot <- plot.phylo(Tigers[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Tigers, function(x) try(force.ultrametric(x)))
WillWork
bdTigers <- fit.bd(Tigers[[1]], rho=0.2)
bdTigers
Lions <- search_treebase("Panthera", by="taxon", max_trees=20)
length(Lions)
pdf("r07-LionsPhylo.pdf", height=5, width=5)
LionsPlot <- plot.phylo(Lions[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Lions, function(x) try(is.ultrametric(x)))
WillWork
bdLions <- fit.bd(Lions[[1]], rho=0.2)
bdLions
Chickens <- search_treebase("Junglefowl", by="taxon", max_trees=20)
length(Chickens)
pdf("r07-ChickensPhylo.pdf", height=5, width=5)
ChickensPlot <- plot.phylo(Chickens[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Chickens, function(x) try(is.ultrametric(x)))
WillWork
bdChickens <- fit.bd(Chickens[[1]], rho=0.2)
bdChickens
Dogs <- search_treebase("Canus", by="taxon", max_trees=20)
length(Dogs)
pdf("r07-DogsPhylo.pdf", height=5, width=5)
DogsPlot <- plot.phylo(Dogs[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Dogs, function(x) try(is.ultrametric(x)))
WillWork
bdDogs <- fit.bd(Dogs[[1]], rho=0.2)
bdDogs
Pandas <- search_treebase("Panthera", by="taxon", max_trees=20)
length(Pandas)
pdf("r07-PandasPhylo.pdf", height=5, width=5)
PandasPlot <- plot.phylo(Pandas[[1]], cex=0.35)
dev.off()
WillWork <- sapply(Pandas, function(x) try(is.ultrametric(x)))
WillWork
bdPandas <- fit.bd(Pandas[[1]], rho=0.2)
bdPandas