setwd('~/Desktop/Evolution/tasks/Task_10')
install.packages('diversitree')
library(diversitree)
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree<- tree.bisse(Pars, max.taxa=maxN, max.t=maxT)
str(simTree)
?tree.bisse()
stateTable<- table(simTree$tip.state)
stateTable / sum(stateTable)

setwd('~/Desktop/Evolution/tasks/Task_10')
#Question1: frequency 1 and net diversification showed an inversly proportional realtionship. even with low net diversification, this correlations is still shown. 
