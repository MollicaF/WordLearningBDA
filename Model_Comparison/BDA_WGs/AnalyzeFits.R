library(reshape2)

# Directory of Summary Files
DIR = "MCMC_Summaries"

# Delimiter of file name
DELIM = '_'

# Index of the language in the filename delimited by DELIM
LANG_INX = 1

# Index of the item.id in the filename delimited by DELIM
ITEM_ID_INX = 3

# Output location
OUT_NAME = 'Summary.csv'

# Use either the production or the comprehension mcdi
MCDI = '../../Production/production-mcdi.csv'

data = NULL
for (f in list.files(DIR, full.names=T)) {
    filename = strsplit(strsplit(f,paste0(DIR,'/'))[[1]][2], '_')
    d = read.csv(f)
    d$item.id = paste0('item_',filename[[1]][ITEM_ID_INX])
    d$language = filename[[1]][LANG_INX]
    data = rbind(data, d[,c('language','item.id','X','Mean','HDIlow','HDIhigh')])
}

Mean = dcast(data, language + item.id ~ X, value.var='Mean')
colnames(Mean) = c('language', 'item.id', 'delta_Mean', 'k_Mean', 'lambda_Mean', 'start_Mean')
HDIlow = dcast(data, language + item.id ~ X, value.var='HDIlow')
colnames(HDIlow) = c('language', 'item.id', 'delta_HDIlow', 'k_HDIlow', 'lambda_HDIlow', 'start_HDIlow')
HDIhigh = dcast(data, language + item.id ~ X, value.var='HDIhigh')
colnames(HDIhigh) = c('language', 'item.id', 'delta_HDIhigh', 'k_HDIhigh', 'lambda_HDIhigh', 'start_HDIhigh')

d = merge(Mean, merge(HDIlow, HDIhigh))

mcdi = read.csv(MCDI)

d = merge(d, unique(mcdi[,c('language','item.id','category','item')]), all.x=T)

write.csv(d, OUT_NAME, row.names=F)
