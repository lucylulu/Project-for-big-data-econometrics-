################################################################
#####This is code for duplicating the result of       #########
#######'The reform of administration approval system' ##########
#######' by using double machine learning method.     ######### 
#################################################################
rm(list = ls())  
setwd('/Users/xiazou/Desktop/2019spring/big data/courseproject')
source("machinelearning_Function.R")
library(haven) 
library(splitstackshape) 
library(randomForest) 
library(dplyr) 
library(h2o)   
library(xtable)
library(ggplot2) 
library(gridExtra) 
Sys.unsetenv("http_proxy")

h2o.init(ip="localhost", port=54323,nthreads = 3,max_mem_size = '200G')
#read data  

dtadata <- read_dta('diddata.dta')  
dtadata$treat <- as.factor(dtadata$treat)
dtadata$year<- as.factor(dtadata$year) 
dtadata$firmid<- as.factor(dtadata$firmid) 
dtadata$city<- as.factor(dtadata$city) 
dtadata$cic2 <- as.factor(dtadata$cic2) 
dtadata$ALC <- as.factor(dtadata$ALC)

#################################
## Basic Model###################
###################################

#replicate the result of difference in difference ########

diddata <- dtadata[,c('year','firmid','city','cic2','ALC','lnage','lnage2','exp','lnemp','lnavek','s_state','s_foreign','lnpatent00')]
reg1 <- lm(lnpatent00~ALC+factor(year)+factor(cic2)+factor(city),data = diddata) 
summary(reg1) 
reg2 <- lm(lnpatent00~ALC+lnage+lnage2+exp+lnemp+lnavek+s_state+s_foreign+factor(year)+factor(cic2)+factor(city),data = diddata)
summary(reg2)
reg6 <-lm(lnpatent00~ALC+lnage+lnage2+exp+lnemp+lnavek+s_state+s_foreign+year+cic2+city+firmid,data = diddata)
library(AER)
library(plm)
library(stargazer) 
#VIA plm 
reg6 <- plm(lnpatent00~ALC,data = diddata,index = c('year','cic2','city'))
summary(reg6)


#using h2o 
# library(h2o)
# h2o.init(nthreads =3) 
h2o.diddata <- as.h2o(diddata) 
yvar <- c('lnpatent00') 
xvar <- names(diddata)[c(1,2,5,6,7,8,9,10,11,12)]


reg6 <- h2o.glm(xvar,yvar,h2o.diddata) 
coeftable <-reg6@model$coefficients_table
coeftable[coeftable$names == 'ALC',] 
h2o.shutdown(F) 

################################################
####Double Machine Learning#####################
################################################


#####first try of double machine learning (random forest)  ########

#choose data from year 2003 to see the one year effect of policy in 2002  

#explortory data analysis 
subsetdata <- subset(dtadata,year %in% c(2004) )
table(subsetdata$treat)  
groupbydata <- group_by(subsetdata,treat) 
summarise(groupbydata, mean(lnpatent00))  
####using h2o  

subsetdata <- subset(dtadata,year %in% 2002)

include_de1 <- c('city','cic2','ALC','lnage','lnage2','exp','lnemp','lnavek','s_state','s_foreign','lnpatent00') 
include_de2<- c('age_ps_post','tenure_mayor_post','neibor_post','age_ps_t','age_ps_t2','age_ps_t3','tenure_mayor_t','tenure_mayor_t2','tenure_mayor_t3','neibor_t','neibor_t2','neibor_t3','Industrial_t','Industrial_t2','Industrial_t3','lnfirm_t','lnfirm_t2','lnfirm_t3')
includevar_de <- append(include_de1,include_de2)


h2o.subsetdata <- as.h2o(subsetdata[,includevar_de])  
cols <- c('ALC') 
colsx <- includevar_de[-c(1,2,3)] 


meandes <- h2o.cbind(lapply(colsx,function(x){h2o.group_by(h2o.subsetdata,by=cols,mean(x),gb.control = list(na.methods = 'ignore',col.names = NULL))}))

meandes <- as.data.frame(meandes[,c(1,seq(2,length(meandes),2))])
colnames(meandes) <- c('treat',colsx)
sddes <- h2o.cbind(lapply(colsx,function(x){h2o.group_by(h2o.subsetdata,by=cols,sd(x),gb.control = list(na.methods = 'ignore',col.names = NULL))}))

sddes <- as.data.frame(sddes[,c(1,seq(2,length(sddes),2))])
colnames(sddes) <-c('treat',colsx) 
des <- cbind(meandes,sddes)  
vars <- NULL 
nvar <- length(c('treat',colsx))
for(i in (1:nvar)){
  vars <- append(vars,c(i,i+nvar))
}
des <- des[,vars] 
des[,-c(1,2)]<- round(des[,-c(1,2)],digit = 3)
des <- t(des) 
des<- des[-c(1,2),] 
colnames(des)<-c('control','treat') 
for (i in seq(2,nrow(des),2)){
  rownames(des)[i]<-paste0('sd',' ',rownames(des)[i-1])
  for(j in c(1:2)){
    des[i,j]<-paste0('(',des[i,j],')')
  }
}   
obs <- as.data.frame(h2o.group_by(h2o.subsetdata,by=cols,nrow('ALC'))) 
obs <- matrix(obs$nrow,ncol = 2) 
colnames(obs) <- colnames(des)
des <-rbind(des,obs)
rownames(des)[nrow(des)]<-'obs'

print(xtable(des,auto = T))







####for graph 
h2o.dtadata <- as.h2o(dtadata) 
lnpatenmean <- h2o.group_by(h2o.dtadata,by=cols,mean('lnpatent00'))
lnpatenmean <- as.data.frame(lnpatenmean)  
lnpatensum <- h2o.group_by(h2o.dtadata,by = cols, sum('lnpatent00')) 
lnpatensum <- as.data.frame(lnpatensum)
g1 <- ggplot(data = lnpatenmean,aes(year,mean_lnpatent00,group = treat,color = treat))+geom_point()+geom_line()+geom_vline(aes(xintercept = 5))
g2 <- ggplot(data = lnpatensum,aes(year,sum_lnpatent00,group = treat,color = treat))+geom_point()+geom_line()+geom_vline(aes(xintercept = 5))

setEPS()
filenameplot <-  'commontrendcheck.eps'
postscript(filenameplot,width = 10,height = 10)


grid.arrange(g1,g2,nrow = 1) 

dev.off() 

cols2 <- c('city','year','treat')
lnpatenmean2 <- h2o.group_by(h2o.dtadata,by=cols2,mean('lnpatent00'))
lnpatenmean2 <- as.data.frame(lnpatenmean2)  
lnpatensum2 <- h2o.group_by(h2o.dtadata,by = cols2, sum('lnpatent00')) 
lnpatensum2 <- as.data.frame(lnpatensum2)
g1 <- ggplot(data = lnpatenmean2,aes(year,mean_lnpatent00,group = treat,color = treat))+geom_point()
g2 <- ggplot(data = lnpatensum2,aes(year,sum_lnpatent00,group = treat,color = treat))+geom_point()
setEPS()
filenameplot <-  'randomnesscheck.eps'
postscript(filenameplot,width = 10,height = 10)


grid.arrange(g1,g2,nrow = 1) 

dev.off()  

###boxplot to see the relation between treated and lnpatent00 


setEPS() 

filenameplot <- 'boxplot_randomnesscheck.eps'
databox <- subset(dtadata,year %in% 2002) 
databox <- databox[databox$lnpatent00 != 0,]
boxplot(lnpatent00~treat,data = databox)  

dev.off()











#using the same covariate as the paper  
data_all1 <- dtadata[,c('year','firmid','city','cic2','ALC','lnage','lnage2','exp','lnemp','lnavek','s_state','s_foreign','lnpatent00')]
data_all1<-na.omit(data_all1) 

#####using data from all year ##### 
include1 <- c('year','firmid','city','cic2','ALC','lnage','lnage2','exp','lnemp','lnavek','s_state','s_foreign','lnpatent00') 
include2<- c('age_ps_post','tenure_mayor_post','neibor_post','age_ps_t','age_ps_t2','age_ps_t3','tenure_mayor_t','tenure_mayor_t2','tenure_mayor_t3','neibor_t','neibor_t2','neibor_t3','Industrial_t','Industrial_t2','Industrial_t3','lnfirm_t','lnfirm_t2','lnfirm_t3')
includevar <- append(include1,include2)
data2 <- dtadata[,includevar]
data2 <- na.omit(data2)

##subset data 

data3 <- subsetdata[,includevar]
data3 <- na.omit(data3)

##for 2002 year 
data4 <- dtadata[dtadata$year == 2002,includevar]

data4 <- na.omit(data4)




#response variable 
yvar <- c('lnpatent00')   
#treatment indicator 
d <- 'ALC'  
#control variable  
xvar_1<-include1[c(1,3,4,6,7,8,9,10,11,12)]
xvar <- append(include1[c(1,3,4,6,7,8,9,10,11,12)],include2)

xvar_d <- include1[c(4,6:12)]
 



########For specification 1#################### 

##For panel data ######
all2_sp1 <- gettable_commonsupport(data2,K=2,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=1)
all5_sp1 <- gettable_commonsupport(data2,K=5,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=1)

### for data only in 2002, with M= 100 
all2_2002_sp1 <- gettable_commonsupport(data4,K=2,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=1)
all5_2002_sp1 <- gettable_commonsupport(data4,K=5,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=1)
star = Sys.time()
all2_2002_sp1_2 <- gettable_commonsupport(data4,K=2,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=2)
end = Sys.time() 
end - star
all5_2002_sp1_50 <- gettable_commonsupport(data4,K=5,yvar =yvar,xvar = xvar_1,xvar_d = xvar_d,d = d, Methods = Methods,M=10)








###########################for spcification 2###### 

all5 <- gettable(data2,K=5,yvar =yvar,xvar = xvar,xvar_d = xvar_d,d = d, Methods = Methods,M=1)
all2 <- gettable(data2,K=2,yvar =yvar,xvar = xvar,xvar_d = xvar_d,d = d, Methods = Methods,M=1)

 
all2_2004 <- gettable(data3,K=2,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods,M=1)
all5_2004 <- gettable(data3,K=5,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods,M=1)

all2_2002 <- gettable(data4,K=2,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods,M=1)
all5_2002 <- gettable(data4,K=5,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods,M=1)


all2_2002_commonsupport <- gettable_commonsupport(data4,K=2,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods)
all5_2002_commonsupport <- gettable_commonsupport(data4,K=5,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods)

all2_2004_commonsupport <- gettable_commonsupport(data3,K=2,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods)
all5_2004_commonsupport <- gettable_commonsupport(data3,K=5,yvar = yvar,xvar = xvar,xvar_d = xvar_d, d = d,Methods = Methods)

####save all the result into r 
save(all2,file = 'all2.RData')

save(all2_2002,file = 'all2_2002.RData')

save(all2_2002_commonsupport,file = 'all2_2002_commonsupport.RData')

save(all2_2004,file = 'all2_2004.RData') 

save(all2_2004_commonsupport,file = 'all2_2004_commonsupport.RData')

save(all5,file = 'all5.RData')

save(all5_2002,file = 'all5_2002.RData')

save(all5_2002_commonsupport,file = 'all5_2002_commonsupport.RData')

save(all5_2004,file = 'all5_2004.RData')

save(all5_2004_commonsupport,file = 'all5_2004_commonsupport.RData')



h2o.shutdown(F)
