library(readr)
pharmacy <- read_csv("C:/Users/VE1/Desktop/mosaic_pharmacy.csv", 
                     col_types = cols(X1 = col_skip()))
View(pharmacy)

summary(pharmacy)

#find columns having more than 80% 0 (missing values)
colSums(pharmacy == 0)
which(colSums(pharmacy == 0) >= 38098*0.8) #almost mosaic household data

#skip columns with household mosaic column
pharmacy_cleaned = pharmacy[,-c(66:137)]
#skip rows with more than 80% empty
pharmacy_cleaned = pharmacy_cleaned[-which(rowSums(pharmacy_cleaned==0)>=143*0.8),]

#delete columns with near zero variance
library(caret)
nzv <- nearZeroVar(pharmacy_cleaned)
near_zero <- pharmacy_cleaned[,nzv] 
#dwelling_type_has_pobox, dwelling_unit_size_10_19,
#dwelling_unit_size_50_100,estimated_current_home_value_range_1m_plus
#home_telco_subscribers_bundle_buyer_voip_16755_a are vaiable with near zero variance
pharmacy_cleaned <- pharmacy_cleaned[,-nzv]


#columns that are highly correlated
numcols = which(sapply(pharmacy_cleaned, function(x) class(x)=="numeric" || class(x)=="integer"))
length(numcols)
corr = cor(pharmacy_cleaned[,numcols], method='spearman', use='pairwise.complete.obs')
findCorrelation(corr, verbose=T, names=T, cutoff=0.95)
highcorr = findCorrelation(corr,cutoff=0.95)
pharmacy = pharmacy_cleaned[,numcols]
pharmacy = pharmacy[,-c(highcorr)]
pharmacy = cbind(pharmacy, pharmacy_cleaned$cvs, pharmacy_cleaned$walg)

write.csv(pharmacy, 'C:/Users/VE1/Desktop/pharmacy_cleaned.csv')

####factor y#####
names(pharmacy)[names(pharmacy) == 'pharmacy_cleaned$cvs'] <- 'CVS'
names(pharmacy)[names(pharmacy) == 'pharmacy_cleaned$walg'] <- 'Walg'
pharmacy[pharmacy$CVS == TRUE,55] <- 'yes'
pharmacy[pharmacy$CVS == FALSE,55] <- 'no'
pharmacy[pharmacy$Walg == TRUE,56] <- 'yes'
pharmacy[pharmacy$Walg == FALSE,56] <- 'no'
summary(pharmacy)

###########mean imputation####################
mean <- pharmacy
mean[mean == 0]<- NA
summary(mean)
install.packages('UpSetR')
install.packages('naniar') # http://naniar.njtierney.com/
library(UpSetR)
library(naniar)
##with na
trc <- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE)
tune.mean = train(CVS ~ ., data=mean[,-56], 
                method = "rpart", 
                na.action = na.pass,
                metric = 'Accuracy',
                tuneLength = 20,  # how many cp values to try
                trControl = trc)

tune.mean #accuracy 0.905

##fill with mean value
for(i in 1:ncol(mean)){
  mean[is.na(mean[,i]), i] <- mean(mean[,i], na.rm = TRUE)
}
summary(mean)
tune.mean = train(CVS ~ ., data=mean[,-56], 
                  method = "rpart", 
                  na.action = na.pass,
                  metric = 'Accuracy',
                  tuneLength = 20,  # how many cp values to try
                  trControl = trc)

tune.mean  #accuracy 0.9048

##random imputaion
random.imp <- function (a){
  missing <- is.na(a)      
  n.missing<- sum(missing)
  a.obs<- a[!missing]
  imputed<- a
  imputed[missing]<- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

random <- pharmacy
random[random == 0]<- NA
for(i in 1:ncol(random)){
  random[is.na(mean[,i]), i] <- random.imp(random[,i])
}

tune.random = train(CVS ~ ., data=random[,-56], 
                  method = "rpart", 
                  na.action = na.pass,
                  metric = 'Accuracy',
                  tuneLength = 20,  # how many cp values to try
                  trControl = trc)
tune.random  #accuracy is 0.9038031


##MULTIPLE IMPUTATION WITH MICE
library(mice)
mice <- pharmacy
mice[mice == 0]<- NA
mice.out <- mice(mice, m=1, method='pmm', seed=390 )
mice.out

MICE <- complete(mice.out, 1)

tune.mice = train(CVS ~ ., data=MICE[,-56], 
                    method = "rpart", 
                    na.action = na.pass,
                    metric = 'Accuracy',
                    tuneLength = 20,  # how many cp values to try
                    trControl = trc)
tune.mice

#accuracy 0.049087


