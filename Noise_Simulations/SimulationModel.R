library(plyr)
source('Utilities.R')
graphics.off()

#####################################################
# Functions
#####################################################
genMCMC = function( data, numSavedSteps=5000000, saveName=NULL,
                    thinSteps=1000, runjagsMethod=runjagsMethodDefault,
                    nChains=nChainsDefault ) {
    
    require(rjags)
    require(runjags)
    
    yes = data$yes
    N = data$N
    age = data$age
    Nbin = length(unique(age))
    
    dataList = list(yes=yes, N=N, age=age, Nbin=Nbin)
    
    modelString = "
    model {
    k ~ dunif(0, 10000)
    lambda ~ dunif(0,10000)
    start ~ dunif(0,1000)
    for ( a in 1:Nbin ) {
    theta[a] <- pgamma(max(age[a]-start,0), k, lambda)
    yes[a] ~ dbinom(theta[a], N[a])
    }
    }
    "
    writeLines( modelString , con="TEMPmodel.txt" )
    
    parameters = c("k","lambda", "start")
    adaptSteps = 10000
    burnInSteps = 10000
    
    runJagsOut = run.jags(method=runjagsMethod ,
                          model="TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          n.chains=nChains ,
                          inits=list(k=1, lambda=1, start=0),
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
    codaSamples = as.mcmc.list( runJagsOut )
    if ( !is.null(saveName) ) {
        save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
    }
    return( codaSamples )
}

smryMCMC = function(  codaSamples, saveName=NULL ) {
    summaryInfo = NULL
    mcmcMat = as.matrix(codaSamples, chains=TRUE)
    
    summaryInfo = rbind( summaryInfo, 
                         "k" = summarizePost( mcmcMat[,"k"] ) )
    summaryInfo = rbind( summaryInfo ,
                         "lambda" = summarizePost( mcmcMat[,"lambda"] ) )  
    
    summaryInfo = rbind( summaryInfo ,
                         "start" = summarizePost( mcmcMat[,"start"] ) )  
    
    if ( !is.null(saveName) ) {
        write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv", sep="") )
    }
    return( summaryInfo )
}

#####################################################
# Run Code
#####################################################

args <- commandArgs(trailingOnly = TRUE)

sim <- read.csv(args[1], header=T)

fit.gamma <- function(sim) {
    data = data.frame(yes=sim$yes, N=sim$N, age=sim$age)
    
    fileNameRoot=paste(strsplit(args[1], '.csv')[[1]], sim$Simulation[1], sep='_')
    graphFileType = "eps"
    
    mcmcCoda = genMCMC(data=data, numSavedSteps=5000,saveName=fileNameRoot)
        
    summaryInfo = smryMCMC( mcmcCoda, saveName=fileNameRoot )
    return(data.frame(Simulation=fileNameRoot, k_mean=summaryInfo["k", "Mean"], k_median=summaryInfo["k", "Median"], 
                      k_mode=summaryInfo["k", "Mode"], k_hdiLow=summaryInfo["k", "HDIlow"], k_hdiHigh=summaryInfo["k", "HDIhigh"], 
                      lambda_mean=summaryInfo["lambda", "Mean"], lambda_median=summaryInfo["lambda", "Median"], 
                      lambda_mode=summaryInfo["lambda", "Mode"], lambda_hdiLow=summaryInfo["lambda", "HDIlow"], 
                      lambda_hdiHigh=summaryInfo["lambda", "HDIhigh"], start_mean=summaryInfo["start", "Mean"], 
                      start_median=summaryInfo["start", "Median"], start_mode=summaryInfo["start", "Mode"], 
                      start_hdiLow=summaryInfo["start", "HDIlow"], start_hdiHigh=summaryInfo["start", "HDIhigh"]))
}

results = ddply(sim, .(Simulation), fit.gamma)
