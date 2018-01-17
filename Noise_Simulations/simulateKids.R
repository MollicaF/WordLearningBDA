# Generates n kids data in output ready for JAGS
library(msm) # truncated normal

# The variance is in the parameters not on the AoA
# Parameters
Simulations <- 1000
N <- 1000
K <- 10
Lambda <- 0.5
S <- 4
var <- 0.001
vtext <- "001"

summarized = NULL
for (j in 1:Simulations) {
    data = NULL
    for (i in 1:N) {
        k = rtnorm(1, K, sqrt(K*var), lower=0)
        lambda = rtnorm(1, Lambda, sqrt(Lambda*var), lower=0)
        s = rtnorm(1, S, sqrt(S*var), lower=0)
        AoA = rgamma(1, shape=k, rate=lambda) + s
        data = rbind(data, data.frame(Subject=i, K=k, Lambda=lambda, S=s, AoA=AoA))
    }
    write.csv(data, file=paste('AoA/ParSimulation_', j ,'_Var_',vtext,'.csv',sep=''), row.names=F)
    
    for (a in 15:36) {
        summarized = rbind(summarized, data.frame(Simulation=paste('ParSimulation_',j,sep=''), age=a, yes=dim(data[data$AoA < a, ])[1], N=N))
    }
}

write.csv(summarized, file=paste('JAGSData_Parameter_Var_',vtext,'.csv',sep=''), row.names=F)


s = data.frame(Simulation = unique(summarized$Simulation), group = c(rep(1:12,83),1,2,3,4))

d = merge(summarized, s)

prettysplit = function(Z) {
  write.csv(Z, paste('Parameter/JAGSData_Parameter_Var_',vtext,'_', Z$group[1], sep=''))
}

library(plyr)

ddply(d, .(group), prettysplit)


#################################################################
# The variance is now in the AoA not the generating parameters
#################################################################

summarized = NULL
for (j in 1:Simulations) {
  data = NULL
  for (i in 1:N) {
    AoA = rgamma(1, shape=K, rate=Lambda) + S
    AoA = rtnorm(1, AoA, sqrt(AoA*var), lower=0)  
    data = rbind(data, data.frame(Subject=i, K=K, Lambda=Lambda, S=S, AoA=AoA))
  }
  write.csv(data, file=paste('AoA/ExtSimulation_', j ,'_Var_',vtext,'.csv',sep=''), row.names=F)
  
  for (a in 15:36) {
    summarized = rbind(summarized, data.frame(Simulation=paste('ExtSimulation_',j,sep=''), age=a, yes=dim(data[data$AoA < a, ])[1], N=N))
  }
}

write.csv(summarized, file=paste('JAGSData_External_Var_',vtext,'.csv',sep=''), row.names=F)


s = data.frame(Simulation = unique(summarized$Simulation), group = c(rep(1:12,83),1,2,3,4))

d = merge(summarized, s)

prettysplit = function(Z) {
  write.csv(Z, paste('External/JAGSData_External_Var_',vtext,'_', Z$group[1], sep=''))
}

library(plyr)

ddply(d, .(group), prettysplit)
