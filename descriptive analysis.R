library(readr)
library(tidyverse)
library(caret)
library(leaps)
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


####factor y#####
names(pharmacy)[names(pharmacy) == 'pharmacy_cleaned$cvs'] <- 'CVS'
names(pharmacy)[names(pharmacy) == 'pharmacy_cleaned$walg'] <- 'Walg'
pharmacy[pharmacy$CVS == TRUE,55] <- 'yes'
pharmacy[pharmacy$CVS == FALSE,55] <- 'no'
pharmacy[pharmacy$Walg == TRUE,56] <- 'yes'
pharmacy[pharmacy$Walg == FALSE,56] <- 'no'
summary(pharmacy)


PHARMACY <- pharmacy
PHARMACY[PHARMACY == 0]<- NA
prop.table(table(PHARMACY$CVS))

#take political columns out
PHARMACY = PHARMACY[,-(grep(pattern = 'politi', colnames(PHARMACY)))]

#mean imputation
for(i in 1:ncol(PHARMACY)){
  PHARMACY[is.na(PHARMACY[,i]), i] <- mean(PHARMACY[,i], na.rm = TRUE)
}



#####TAKE rochester zip out######
library(dplyr)
rochester = PHARMACY[grepl(146,PHARMACY$zip),]
rownames(rochester) <- NULL
rochester = rochester[11:37,] #as hold out file

pharmacy = PHARMACY[!(PHARMACY$zip %in% rochester$zip),] #dataset without rochester



##########Descriptive analysis with rochester###########
colnames(PHARMACY)
library(MASS)

#Fit the full model
full.model <- glm(as.factor(CVS)~., data = PHARMACY[,-c(1,44)],family=binomial)
coef(full.model)

#select most contributive variables
step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)

step.model$anova








