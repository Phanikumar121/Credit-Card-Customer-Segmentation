
setwd("C:\\Users\\PHANI KUMAR\\Desktop\\Credit card segmentation")

mydata <- read.csv("CC GENERAL.csv")

#Loading required packages
library(caret)
library(GPArotation)
library(tables)
library(psych)
library(cluster)
library(dplyr)
options(scipen = 999)

str(mydata)
summary(mydata)


mydata<- mutate(mydata,LIMIT_USAGE=BALANCE/CREDIT_LIMIT,
                P_MP_RATIO=PAYMENTS/MINIMUM_PAYMENTS,
                MONTHLY_AVG_PURCHASES = PURCHASES/TENURE,
                MONTHLY_AVG_CASH_ADVANCE = CASH_ADVANCE/TENURE,
                AVG_AMT_PER_PURCHASE = PURCHASES/PURCHASES_TRX,
                AVG_CASH_ADV_PER_TRX = CASH_ADVANCE/CASH_ADVANCE_TRX)

mydata$AVG_AMT_PER_PURCHASE[is.nan(mydata$AVG_AMT_PER_PURCHASE)]<- 0
mydata$P_MP_RATIO[is.nan(mydata$P_MP_RATIO)]<- 0 
mydata$AVG_CASH_ADV_PER_TRX[is.nan(mydata$AVG_CASH_ADV_PER_TRX)]<- 0 
mydata$ONEOFF_PURCHASES[mydata$PURCHASES==0]<- 0
mydata$INSTALLMENTS_PURCHASES[mydata$PURCHASES==0]<- 0
mydata$AVG_AMT_PER_PURCHASE[mydata$AVG_AMT_PER_PURCHASE==Inf]<- 0

summary(mydata)
mydata <- mydata[,-1]

# User defined function for calculating the descriptives:-
my_stats <- function(x){
  n<-length(x)
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<- mean(a)
  max<- max(a)
  min<- min(a)
  p1<-quantile(a,0.01)
  p5<- quantile(a,0.05)
  p95<- quantile(a,0.95)
  p99<-quantile(a,0.99)
  return(c(count=n,nmiss=nmiss,mean=m,max=max,min=min,P1=p1,P5=p5,P95=p95,P99=p99))  
}

options(scipen = 999)
desc_stats<- data.frame(t(apply(mydata,2,my_stats)))
write.csv(desc_stats,"Stats.csv")

####################Missing value treatment##############
colSums(is.na(mydata))

mydata$CREDIT_LIMIT[is.na(mydata$CREDIT_LIMIT)]<- 4494.44945
mydata$MINIMUM_PAYMENTS[is.na(mydata$MINIMUM_PAYMENTS)]<- 864.2065423
mydata$LIMIT_USAGE[is.na(mydata$LIMIT_USAGE)]<- 0.388926409
mydata$P_MP_RATIO[is.na(mydata$P_MP_RATIO)]<- 9.350070124

colSums(is.na(mydata))

#Outlier treatment(User defined function)
#UC = 0.99
#LC = 0.1

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  return(x)
  
}

mydata <- data.frame(sapply(mydata,FUN = outlier_treat))

input_data <- mydata

#Correlation matrix among the vars
corr_matrix <- cor(input_data)

#Eigen values
eigen_values <- mutate(data.frame(eigen(corr_matrix)$values)
                       ,cum_sum_eigen=cumsum(eigen.corr_matrix..values)
                       , pct_var=eigen.corr_matrix..values/sum(eigen.corr_matrix..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corr_matrix..values))                                                   

write.csv(eigen_values,"Eigen values.csv")

scree(corr_matrix,factors = TRUE,pc = TRUE,main = "Scree plot",hline = NULL,add = FALSE)

#Bases on eigen values i would like to go with a seven factor solution

#Based on scree plot, 6 components should be used 
FA <- fa(r = corr_matrix,7,rotate = "varimax",fm = "ml")
FA_SORT <- fa.sort(FA)                         

Loadings1 <- data.frame(FA_SORT$loadings[1:ncol(mydata),])

write.csv(Loadings1,"loadings.csv")

#Vars selected in factor analysis
cluster_vars <- c("ONEOFF_PURCHASES",
                  "MONTHLY_AVG_PURCHASES",
                  "PURCHASES",
                  "PAYMENTS",
                  "PURCHASES_FREQUENCY",
                  "CASH_ADVANCE",
                  "LIMIT_USAGE",
                  "BALANCE",
                  "CASH_ADVANCE_TRX",
                  "TENURE",
                  "INSTALLMENTS_PURCHASES")

inputdata_cluster <- mydata[cluster_vars]

#scaling the selected vars
inputdata_final <- scale(inputdata_cluster)

#building clusters using k-means clustering
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)
cluster_seven <- kmeans(inputdata_final,7)

cust_seg_clust <- data.frame(cbind(mydata,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,
                                km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster,
                                km_clust_7=cluster_seven$cluster))

#Converting the cluster variables to factor variables
cust_seg_clust$km_clust_3 <- as.factor(cust_seg_clust$km_clust_3)
cust_seg_clust$km_clust_4 <- as.factor(cust_seg_clust$km_clust_4)
cust_seg_clust$km_clust_5 <- as.factor(cust_seg_clust$km_clust_5)
cust_seg_clust$km_clust_6 <- as.factor(cust_seg_clust$km_clust_6)
cust_seg_clust$km_clust_7 <- as.factor(cust_seg_clust$km_clust_7)

colnames(mydata)

profile <- tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+CASH_ADVANCE+
                     PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                     CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+                
                     PRC_FULL_PAYMENT+TENURE+LIMIT_USAGE+P_MP_RATIO+ MONTHLY_AVG_PURCHASES+MONTHLY_AVG_CASH_ADVANCE+        
                     AVG_AMT_PER_PURCHASE+AVG_CASH_ADV_PER_TRX ~ mean +(mean*km_clust_3)+(mean*km_clust_4)+
                     (mean*km_clust_5)+(mean*km_clust_6)+(mean*km_clust_7),data = cust_seg_clust)

profile1 <- as.matrix(profile)

profile1 <- data.frame(profile1)

profile2 <- tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6)+
                      (length*km_clust_7),data = cust_seg_clust)

profile2 <- as.matrix(profile2)
profile2 <- data.frame(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)
