library(readr)
library(tidyverse)
library(caret)
library(leaps)
pharmacy <- read_csv("C:/Users/VE1/Desktop/mosaic_pharmacy.csv", 
                     col_types = cols(X1 = col_skip()))

table(pharmacy$cvs)

#change true and false into 0 and 1
pharmacy$cvs = as.numeric(pharmacy$cvs)

#skip rows with more than 80% empty
pharmacy_cleaned = pharmacy[-which(rowSums(pharmacy==0)>=215*0.8),]

#delete columns with near zero variance
library(caret)
nzv <- nearZeroVar(pharmacy_cleaned)
near_zero <- pharmacy_cleaned[,nzv] 
pharmacy_cleaned <- pharmacy_cleaned[,-nzv]


#columns that are highly correlated
numcols = which(sapply(pharmacy_cleaned, function(x) class(x)=="numeric" || class(x)=="integer"))
length(numcols)
corr = cor(pharmacy_cleaned[,numcols], method='spearman', use='pairwise.complete.obs')
findCorrelation(corr, verbose=T, names=T, cutoff=0.95)
highcorr = findCorrelation(corr,cutoff=0.95)
pharmacy = pharmacy_cleaned[,numcols]
pharmacy = pharmacy[,-c(highcorr)]

colnames(pharmacy)
#perform logistic regression
summary(glm(cvs ~ health_and_well_being_p116984_1 + health_and_well_being_p116984_3 + health_and_well_being_p116984_4+
      health_and_well_being_p116984_5+education_model_p1189_d+marital_status_p1224_b+occupation_group_v2_p112218_g+opening_weekend_movie_goers
      +tv_movie_fans_16735_a+tv_news_16819_a+est_household_income_125_150k+est_household_income_200_225k+
        est_household_income_225k_plus+estimated_current_home_val_less_than_200k+estimated_current_home_value_200_450k
      , data = pharmacy, family = "binomial"))

summary(glm(cvs ~ .
            , data = pharmacy, family = "binomial"))



