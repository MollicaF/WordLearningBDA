library(plyr)
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
source("DBDA2E-utilities.R")

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
        k ~ dunif(1, 10000)
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
print(paste(args[1],'.csv',sep=''))
mcdi <- read.csv(paste(args[1],'.csv',sep=''), header=T)
mcdi$word <- mcdi$item 
mcdi$successes <- round(mcdi$n * mcdi$produces)
mcdi$failures <- mcdi$n - mcdi$successes

fit.gamma <- function(mcdi) {
        data = data.frame(yes=mcdi$successes, N=mcdi$n, age=mcdi$age)
        
        fileNameRoot=paste(paste(args[1],'/',sep=''),mcdi$language[1], mcdi$item.id[1],'_',sep='_')
        graphFileType = "eps"
        
        mcmcCoda = genMCMC(data=data, numSavedSteps=5000,saveName=fileNameRoot)
        
        summaryInfo = smryMCMC( mcmcCoda, saveName=fileNameRoot )
        
        return(data.frame(word=mcdi$word[1], category=mcdi$category[1], language=mcdi$language[1], k_mean=summaryInfo["k", "Mean"], 
                          k_median=summaryInfo["k", "Median"], k_mode=summaryInfo["k", "Mode"], k_hdiLow=summaryInfo["k", "HDIlow"], 
                          k_hdiHigh=summaryInfo["k", "HDIhigh"], lambda_mean=summaryInfo["lambda", "Mean"], 
                          lambda_median=summaryInfo["lambda", "Median"], lambda_mode=summaryInfo["lambda", "Mode"], 
                          lambda_hdiLow=summaryInfo["lambda", "HDIlow"], lambda_hdiHigh=summaryInfo["lambda", "HDIhigh"], 
                          start_mean=summaryInfo["start", "Mean"], start_median=summaryInfo["start", "Median"], 
                          start_mode=summaryInfo["start", "Mode"], start_hdiLow=summaryInfo["start", "HDIlow"], 
                          start_hdiHigh=summaryInfo["start", "HDIhigh"]))
}


results = ddply(mcdi, .(language, item.id), fit.gamma)

output_file_name_final = paste(str(args[1]),'FinalResultsJAGSModel_',str(args[1]),'.csv', sep='')
write.csv(results, output_file_name_final)
