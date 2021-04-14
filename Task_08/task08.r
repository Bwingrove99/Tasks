setwd('~/Desktop/Evolution/tasks/Task_06')
library("phytools")
plot(tree, type="fan")
#1: There are 82 tips, produced 161, and lengths are present.
data <- read.csv("https://jonsmitchell.com/data/svl.csv" , stringsAsFactors=F , row.names=1)
#2: This is many different species of lizards and their snout length meausrment. 
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree , svl, vars=TRUE, CI=TRUE)
#3: The values are stored in "ace" and CI95 element is a confidence interval. 
#4:The contrast state is the root each time it is computed.
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree , type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree , svl , plot=F)
plot(obj, type="fan" , legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
#5: It runs the command action each time it is in the list.
fossilNodes <- c()
nodeN <- c()
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN [i] <- Node
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree , svl , anc.states=fossilNodes, C1=TRUE, var=TRUE)
#8-10: EB is the best fit.
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')
