setwd("~/Desktop/Evolution/Tasks/Task_07")
install.packages("phytools")
library("phytools")
library("ape")
text.string<- "(((((((cow, pig), whale),(bat, (lemur, human))),(robin, iguana)), coelacenth), (gold_fish, trout)), shark) ;"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels (frame="circle", bg='white', cex=1)
#1: I think that the shark is more closely related to the goldfish because of node 13. There are less node separations between the two.
vert.tree
#2: There are no branch lengths within the tree.
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
#3: plotTree(AnolisTree,offset=1, show.tip.label=FALSE)
#4: circular_tree(AnolisTree)
#5 plotTree(AnolisTree,offset=1, show.tip.label=TRUE, tip.color="red"
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
bdModel <- make.bd(armadillo[[1]])
fit <- find.mle(bdModel, c(0.1, 0.03), method='optim', lower = 0)
pdf('r07bd_model_armadillo[[1]]', height = 5, width = 5)
samples <- mcmc(bdModel, fit$par, nsteps=200, lower=c(-Inf, -Inf), upper = c(Inf, Inf), w=c(0.1, 0.1))
col <- c('red', 'blue')
profiles.plot(samples[c('lambda', 'mu')], col.line = col, las = 1, legend = 'topright')
abline(v = 0, lty = 2)
bdarmadillo
