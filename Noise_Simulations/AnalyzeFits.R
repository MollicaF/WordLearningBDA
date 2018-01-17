library(reshape2)

DIR = 'Recovered_Parameters/External/Summary'
OUT_PATH = 'ExternalNoise_Summary.csv'

data = NULL
for (f in list.files(DIR, full.names=T)) {
    filename = strsplit(f, split='_')
    d = read.csv(f)
    d$PercentVar = paste(filename[[1]][4], "_", filename[[1]][5],sep='')
    d$SimulationNo = filename[[1]][8]
    data = rbind(data, d[,c('SimulationNo','PercentVar','X','Mean','HDIlow','HDIhigh')])
}

Mean = dcast(data, PercentVar + SimulationNo ~ X, value.var='Mean')
colnames(Mean) = c('PercentVar', 'SimulationNo', 'k_Mean', 'lambda_Mean', 'start_Mean')
HDIlow = dcast(data, PercentVar + SimulationNo ~ X, value.var='HDIlow')
colnames(HDIlow) = c('PercentVar', 'SimulationNo', 'k_HDIlow', 'lambda_HDIlow', 'start_HDIlow')
HDIhigh = dcast(data, PercentVar + SimulationNo ~ X, value.var='HDIhigh')
colnames(HDIhigh) = c('PercentVar', 'SimulationNo', 'k_HDIhigh', 'lambda_HDIhigh', 'start_HDIhigh')

d = merge(Mean, merge(HDIlow, HDIhigh))

write.csv(d, OUT_PATH, row.names=F)
