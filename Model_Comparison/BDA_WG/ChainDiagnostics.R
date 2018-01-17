library(coda)

# Directory of Chain Files
DIR = "Chains"

# Delimiter of file name
DELIM = '_'

# Index of the language in the filename delimited by DELIM
LANG_INX = 1

# Index of the item.id in the filename delimited by DELIM
ITEM_ID_INX = 3

# Output location
OUT_NAME = 'Convergence.csv'

data = NULL
for (f in list.files(DIR, full.names=T)) {
  load(f)
  filename = strsplit(strsplit(f,paste0(DIR,'/'))[[1]][2], '_')
  g = data.frame(gelman.diag(codaSamples, autoburnin = F)$psrf)
  
  item.id = paste0('item_',filename[[1]][ITEM_ID_INX])
  language = filename[[1]][LANG_INX]
  data = rbind(data, data.frame(language=language, item.id=item.id, Rhat_k=g['k',1], Rhat_l=g['lambda',1], 
                                Rhat_s=g['start',1], Rhat_delta=g['delta',1]))
}


write.csv(data, OUT_NAME, row.names=F)
