################### Packages ####################

if (!require("ROCR")) install.packages("ROCR")  #all packages
if (!require("caret")) install.packages("caret") 
if (!require("cutpointr")) install.packages("cutpointr")
if (!require("MLmetrics")) install.packages("MLmetrics")
if (!require("ROCR")) library(ROCR)
if (!require("caret")) library(caret)
if (!require("cutpointr")) library(cutpointr)
if (!require("MLmetrics")) library(MLmetrics)

#####################Datasets###############################

#Download dataset
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OPQMVF
#http://odds.cs.stonybrook.edu/#table1

ds<-read.csv(file = file.choose(),header = F)   #choose dataset

#################### Choose model ####################
#run AHBOS.R before this step
methods <- c("static","AHBOS","dyn")
br1 <- "FD"
method <- methods[2]  #choose method
###################################################

#################### Data arrangement for application ####################

d<-ncol(ds)-1 #data arrangement for application
df<-ds
# df$Actual<-ifelse(df[,d+1]==1,1,0) # outlier tag "1" for outlier "0" for inlier
df$Actual<-ifelse(df[,d+1]=="o",1,0) # outlier tag "o" for outlier "n" for inlier (Goldstein data)
df$id<-1:nrow(df)
conf_mat<-list()
conf_mat$Actual<-table(df[,d+1]) 
actual <- df$Actual
df<-cbind(df,ascore1=0)
###################################################

#################### Score computation for given method ####################
if(method=="AHBOS"){
  for (i in 1:d){
    df<-df[order(df[[i]]),] #begin from the first variable
    score<-AHBOS(df[,i],br = br1)
    df$ascore1<-df$ascore1+score
  }
} else if (method=="static"){    # static hbos
  
  for (i in 1:d){
    
    df<-df[order(df[[i]]),] # begin from the first variable
    hist_inf<-hist(df[,i],breaks=br1,plot=F)
    
    dens<-rep(hist_inf$density,times=hist_inf$counts)
    
    if (length(unique(dens))==1){
      d2<- (1/nrow(df))
    } else {
      d2 <- dens/max(dens)
    }
    score<-log10(1/d2)
    df$ascore1<-df$ascore1+score  #final score
    
  }
} else if (method=="dyn") {
  
  for (i in 1:d){
    df<-df[order(df[[i]]),] 
    
    score<-dyn_hist(df[,i])
    
    df$ascore1<-df$ascore1+score
  }
}
###################################################


#################### Predicton ####################
predicts <- df$ascore1 #prediction
cpf <- cutpointr(predicts,df$Actual, pos_class = 1,neg_class = 0,
                 method = maximize_metric, metric = F1_score)

prauc <- PRAUC(y_pred = predicts,y_true = df$Actual)
###################################################

#################### Results ####################
summary(cpf)
prauc 
###################################################

