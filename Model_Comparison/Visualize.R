library(ggplot2)
library(plyr)
library(reshape)

data  = read.csv('GenGammaRun.csv')
converge = read.csv('ConvergenceCheck.csv')
converge = subset(converge, language=='English')
converge$converged = "No"
converge$converged[converge$Rhat_k < 1.2 & converge$Rhat_l < 1.2 & converge$Rhat_delta < 1.2] = 'Yes'

100*dim(converge[converge$converged == 'Yes',])[1]/dim(converge)[1]
eng = merge(converge, data)
mcdi  = subset(read.csv('../../Gamma/Old/RawWrdBnk/everything2.csv'), language=='English')
engWG = eng[eng$converged=='Yes',c('item','item.id','k_Mean','lambda_Mean','delta_Mean')]

pred = merge(mcdi, engWG)
pred$BDA_WG = with(pred,pgamma((age/lambda_Mean)**delta_Mean, k_Mean, 1))

pred = pred[,c('item.id', 'item', 'language', 'age', 'produces', 'n', 'category', 'BDA_WG')]

#########################

hidaka = read.csv('../hidaka.csv')
hidaka = hidaka[,c('item','WG_log_delta','WG_log_beta','WG_log_k')]
hidaka$k = exp(hidaka$WG_log_k)
hidaka$lambda = exp(hidaka$WG_log_delta)
hidaka$delta = exp(hidaka$WG_log_beta)

hidaka = merge(mcdi, hidaka)
hidaka$Hidaka_WG = with(hidaka,pgamma((age/lambda)**delta, k, 1))
hidaka = hidaka[,c('item','item.id','age','produces','n','category','Hidaka_WG')]

h = ddply(hidaka, .(item), summarise, Var=var(Hidaka_WG))
dim(h[h$Var!=0,])[1]/dim(h)[1]
hidaka = merge(hidaka, h)
hidaka = hidaka[hidaka$Var!=0,]
hidaka$Var = NULL

##########################
data  = read.csv('../../Weibull/Production/StartTimeRun_P.csv')
converge = read.csv('../../Weibull/Production/ConvergenceCheck_P.csv')
converge = subset(converge, language=='English')
converge$converged = "No"
converge$converged[converge$Rhat_k < 1.2 & converge$Rhat_l < 1.2 & converge$Rhat_delta < 1.2 & converge$Rhat_s < 1.2] = 'Yes'

100*dim(converge[converge$converged == 'Yes',])[1]/dim(converge)[1]
eng = merge(converge, data)
engWGs = eng[eng$converged=='Yes',c('item','item.id','k_Mean','lambda_Mean','delta_Mean','start_Mean')]

pre = merge(mcdi, engWGs)
pre$BDA_WGs = with(pre,pgamma(((age-start_Mean)/lambda_Mean)**delta_Mean, k_Mean, 1))

bdaWgs = pre[,c('item.id', 'item', 'language', 'age', 'produces', 'n', 'category', 'BDA_WGs')]

# ##########################
# data  = read.csv('../TightS/TightSRun.csv')
# eng   = subset(data, language=='English')
# engWGsP = eng[,c('item','item.id','k_Mean','lambda_Mean','delta_Mean','start_Mean')]
# 
# pre = merge(mcdi, engWGsP)
# pre$BDA_WGsP = with(pre,pgamma(((age-start_Mean)/lambda_Mean)**delta_Mean, k_Mean, 1))
# 
# hopps = pre[,c('item.id', 'item', 'language', 'age', 'produces', 'n', 'category', 'BDA_WGsP')]
# 
# ##########################
# data  = read.csv('../Tight/TightRun.csv')
# eng   = subset(data, language=='English')
# engWGP = eng[,c('item','item.id','k_Mean','lambda_Mean','delta_Mean')]
# 
# pre = merge(mcdi, engWGP)
# pre$BDA_WGP = with(pre,pgamma(((age)/lambda_Mean)**delta_Mean, k_Mean, 1))
# 
# pre = pre[,c('item.id', 'item', 'language', 'age', 'produces', 'n', 'category', 'BDA_WGP')]
# 
# ###########################
data  = read.csv('../../Gamma/Run2/StartTimeRun.csv')
converge = read.csv('../../Gamma/Run2/ConvergenceCheck.csv')
converge = subset(converge, language=='English')
converge$converged = "No"
converge$converged[converge$Rhat_k < 1.2 & converge$Rhat_l < 1.2 & converge$Rhat_s < 1.2] = 'Yes'

100*dim(converge[converge$converged == 'Yes',])[1]/dim(converge)[1]
eng   = subset(data, language=='English')
engWGs = eng[,c('item','item.id','k_Mean','lambda_Mean','start_Mean')]

gam = merge(mcdi, engWGs)
gam$BDA_Gs = with(gam,pgamma(((age-start_Mean)*lambda_Mean), k_Mean, 1))

gam = gam[,c('item.id', 'item', 'language', 'age', 'produces', 'n', 'category', 'BDA_Gs')]

###########################
 ## 117 word version
predictions = merge(merge(merge(pred, hidaka),bdaWgs),gam)
pred.m <- melt(predictions, id.vars=c('language','item.id', 'age', 'produces', 'n', 'category', 'item'))

mycor.test <- function(df) {
  cr <- cor.test(df$value, df$produces) 
  data.frame(estimate=cr$estimate, ymin=cr$conf.int[1], ymax=cr$conf.int[2])
}

m <- ddply( pred.m, c("variable"), mycor.test)
m$Model <- m$variable

library(gdata)
m$Model = reorder.factor(m$Model, new.order=c('Hidaka_WG', 'BDA_WG', 'BDA_WGs', 'BDA_Gs'))

plt <- ggplot(m, aes(y=estimate**2, x=Model, color=Model, ymin=ymin**2, ymax=ymax**2)) + 
        geom_point() + 
        geom_errorbar() + 
        ylim(0,1) + theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) + 
        guides(color=F) +
        xlab("") + ylab("Coef. of Determination")
plt


# ggsave('~/Desktop/WrdLrn/Fig/BDAvHidaka.eps', plt, width=4.5, height=3.5)
m <- ddply( pred.m, c("variable"), mycor.test)
m$Model <- m$variable

m.w <- ddply( pred.m, c("item","variable"), mycor.test)
m.w$Model <- m.w$variable
m.w$Model = reorder.factor(m.w$Model, new.order=c('Hidaka_WG', 'BDA_WGP', 'BDA_WG', 'BDA_WGsP', 'BDA_WGs', 'BDA_Gs'))

plt <- ggplot(m.w, aes(y=estimate**2, x=Model, color=Model, ymin=ymin**2, ymax=ymax**2)) + 
  geom_jitter(alpha=0.5) +
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  stat_summary(fun.y=mean, geom='point') +
  #geom_point(alpha=0.5, position_jitter(width=2)) + 
  ylim(0,1) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(color=F) +
  xlab("") + ylab("Coef. of Determination")
plt

#####################################################################################################

# Different number version

pred$variable = colnames(pred)[dim(pred)[2]]
colnames(pred)[dim(pred)[2]-1] = 'value'

hidaka$variable = colnames(hidaka)[dim(hidaka)[2]]
colnames(hidaka)[dim(hidaka)[2]-1] = 'value'
hidaka$language = 'English'
hidaka = hidaka[,colnames(pred)]

bdaWgs$variable = colnames(bdaWgs)[dim(bdaWgs)[2]]
colnames(bdaWgs)[dim(bdaWgs)[2]-1] = 'value'

gam$variable = colnames(gam)[dim(gam)[2]]
colnames(gam)[dim(gam)[2]-1] = 'value'

pred.m = rbind(rbind(rbind(pred, hidaka),bdaWgs),gam)

mycor.test <- function(df) {
  cr <- cor.test(df$value, df$produces) 
  data.frame(estimate=cr$estimate, ymin=cr$conf.int[1], ymax=cr$conf.int[2])
}

m <- ddply( pred.m, c("variable"), mycor.test)
m$Model <- m$variable

library(gdata)
m$Model = reorder.factor(m$Model, new.order=c('Hidaka_WG', 'BDA_WG', 'BDA_WGs', 'BDA_Gs'))

plt <- ggplot(m, aes(y=estimate**2, x=Model, color=Model, ymin=ymin**2, ymax=ymax**2)) + 
  geom_point() + 
  geom_errorbar() + 
  ylim(0,1) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(color=F) +
  xlab("") + ylab("Coef. of Determination")
plt


# ggsave('~/Desktop/WrdLrn/Fig/BDAvHidaka.eps', plt, width=4.5, height=3.5)
m <- ddply( pred.m, c("variable"), mycor.test)
m$Model <- m$variable

m.w <- ddply( pred.m, c("item","variable"), mycor.test)
m.w$Model <- m.w$variable
m.w$Model = reorder.factor(m.w$Model, new.order=c('Hidaka_WG', 'BDA_WG', 'BDA_WGs', 'BDA_Gs'))

plt <- ggplot(m.w, aes(y=estimate**2, x=Model, ymin=ymin**2, ymax=ymax**2)) + 
  geom_jitter(alpha=0.1, aes(color=Model)) +
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  stat_summary(fun.y=mean, geom='point') +
  #geom_point(alpha=0.5, position_jitter(width=2)) + 
  ylim(0,1) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(color=F) +
  xlab("") + ylab("Coef. of Determination")
plt
ggsave('~/Desktop/WrdLrn/Fig/BDAvHidaka.pdf', plt, width=4.5, height=3.5)
