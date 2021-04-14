setwd('~/Desktop/Evolution/tasks/Task_05')
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
#49fixation at 10
#5 offspring
#Large impact because each simulation was significantly different
#In generation 0