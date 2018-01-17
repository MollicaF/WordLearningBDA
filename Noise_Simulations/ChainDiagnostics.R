DIR = 'Recovered_Parameters/External/Chains'
OUT_PATH = 'ExternalNoise_Convergence.csv'

data = NULL
for (f in list.files(DIR, full.names=T)) {
    load(f)
    filename = strsplit(f, split='_')
    chains = NULL
    m = length(codaSamples)
    for (c in 1:m) {
        mean_k = mean(codaSamples[[c]][,c('k')])
        sd_k = sd(codaSamples[[c]][,c('k')])
        mean_l = mean(codaSamples[[c]][,c('lambda')])
        sd_l = sd(codaSamples[[c]][,c('lambda')])
        mean_s = mean(codaSamples[[c]][,c('start')])
        sd_s = sd(codaSamples[[c]][,c('start')])
        chains = rbind(chains, data.frame(mean_k=mean_k, sd_k=sd_k,
                                          mean_l=mean_l, sd_l=sd_l, 
                                          mean_s=mean_s, sd_s=sd_s))
    }    
    n=length(codaSamples[[c]][,c('start')])
    Xmean_k = mean(chains$mean_k)
    Xmean_l = mean(chains$mean_l)
    Xmean_s = mean(chains$mean_s)
    W_k = mean(chains$sd_k)
    W_l = mean(chains$sd_l)
    W_s = mean(chains$sd_s)
    B_k = sum((chains$mean_k - Xmean_k)**2) * n/(m-1)
    B_l = sum((chains$mean_l - Xmean_l)**2) * n/(m-1)
    B_s = sum((chains$mean_s - Xmean_s)**2) * n/(m-1)
    Rhat_k = (W_k*(n-1)/n + B_k/n)/W_k
    Rhat_l = (W_l*(n-1)/n + B_l/n)/W_l
    Rhat_s = (W_s*(n-1)/n + B_s/n)/W_s
    
    PercentVar = paste(filename[[1]][4], "_", filename[[1]][5],sep='')
    SimulationNo = filename[[1]][8]
    data = rbind(data, data.frame(SimulationNo=SimulationNo, PercentVar=PercentVar, 
                                  Rhat_k=Rhat_k, Rhat_l=Rhat_l, Rhat_s=Rhat_s))
}

write.csv(data, OUT_PATH, row.names=F)
