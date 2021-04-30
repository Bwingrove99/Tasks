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

Frequencies <- c('State 0', 'State 1')
Colors <- c('green', 'yellow')
Data<- matrix(c(0.50,0.51,0.68,0.73,0.26,0.45,0.4,0.3,0.70,0.68), nrow=2, ncol=10, byrow=TRUE)
Data
Difference<- c(0.13,0.09,0.04,0.03,0.02)
Freq1<- c(0.45,0.4,0.3,0.70,0.68)
Freq0<- c(0.50,0.51,0.68,0.73,0.26)
pdf('Question1.pdf', height=5, width=5)
barplot(Data, main='Changes in freq of states which is based on the variation of R values', xlab='Diversification differences', ylab='frequency', col=c('green','yellow'))
legend('top', frequencies, fill='green','yellow')
dev.off()
Frequencies <- c('State 0', 'State 1')
Colors<- c('blue','red')
Data<- matrix(c(0.92, 0.91, 0.84, 0.82, 0.77, 0.9, 0.947, 0.932, 0.969, 0.944, 0.945, 0.988, 0.934, 0.933, 0.967, 0.989, 0.973, 0.26, 0.2, 0.10, 0.13, 0.27, 0.067, 0.078, 0.081, 0.041, 0.056, 0.046, 0.071, 0.027, 0.039, 0.013, 0.014, 0.024), nrow=2, ncol=17, byrow=TRUE)
Data
Difference<- c(0.06, 0.06, 0, 0, 0, 0.11, 0.11, 0.11, 0.22, 0.22, 0.22, 0.33, 0.33, 0.33, 0.44, 0.44, 0.44)
pdf('Question2.pdf', height = 8, width = 8)
barplot(Data,, main = 'Closeness to zero State 1 will be during transition rate is nonzero', xlab = 'Difference in Diversification Rate', ylab = 'Frequencies', col=c('blue', 'red'))
head(Data)
Freq1_Trial1<-Data[,2]
Freq1_Trial2<-Data[,5]
Freq1_Trial3<-Data[,8]
Variance1 <- var(Freq1_Trial1)
Variance2 <- var(Freq1_Trial2)
Variance3 <- var(Freq1_Trial3)
Variance1
Variance2
Variance3
VarianceMatrix <- c(Variance1, Variance2, Variance3)
VarianceMatrix
Trial<- c(1,2,3)
Trial
pdf('Quest3.pdf', height=8, width=8)
barplot(VarianceMatrix, main='Variance of frequency 1 through each trial', ylim= c(0, 0.5), xlab='Trial number', ylab='Variance in frequencies', col='pink')
dev.off()
head(Data)
Freq_0<- Data[,2]
Freq_0
NDR_0<- Data[,1]
NDR_0
pdf('mytrend1.pdf', height=8, width=8)
plot(NDR_0, Freq_0, xlab='Net diversification rate state 0', ylab='Frequency of State 0', main='How net diversification rate will Influences frequency')
abline(lm(Freq_0~NDR_0), col='orange', lty='dashed')
dev.off()
Freq_1<- Data[,7]
NDR_1<- Data[,5]
pdf('mytrend1.pdf', height=8, width=8)
plot(NDR_0, Freq_0, xlab='Net diversification rate state 0', ylab='Frequency of State 0', main='How net diversification rate will Influences frequency')
abline(lm(Freq_0~NDR_0), col='orange', lty='dashed')
dev.off()
#Question1: frequency 1 and net diversification showed an inversly proportional realtionship. even with low net diversification, this correlations is still shown. 

#Question2: zero was never reached by state 1. State 1 was never 0 even when the nrt diversification was inc. towards 1. 

#Question3: variation was noticed while the parameters were the same for both states. 

#Question4: there are many things that can contribute to the changes in frequency, some of the evolutionary factors may be drift, interbreeding and selection.

