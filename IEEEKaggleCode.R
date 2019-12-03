
##Kaggle notes
##Hi All,
##I see many questions regarding data description, so it maybe a better idea to open a thread for discussion. The following is a bit more details about it:
##        Transaction Table *
##      TransactionDT: timedelta from a given reference datetime (not an actual timestamp)
##TransactionAMT: transaction payment amount in USD
##ProductCD: product code, the product for each transaction
##card1 - card6: payment card information, such as card type, card category, issue bank, country, etc.
##addr: address
##dist: distance
##P_ and (R__) emaildomain: purchaser and recipient email domain
##C1-C14: counting, such as how many addresses are found to be associated with the payment card, etc. The actual meaning is masked.
##D1-D15: timedelta, such as days between previous transaction, etc.
##M1-M9: match, such as names on card and address, etc.
##Vxxx: Vesta engineered rich features, including ranking, counting, and other entity relations.
##Categorical Features:
##        ProductCD
##card1 - card6
##addr1, addr2
##Pemaildomain Remaildomain
##M1 - M9

##Identity Table *
##        Variables in this table are identity information - network connection information (IP, ISP, Proxy, etc) and digital signature (UA/browser/os/version, etc) associated with transactions.
##They're collected by Vesta's fraud protection system and digital security partners.
##(The field names are masked and pairwise dictionary will not be provided for privacy protection and contract agreement)

##Categorical Features:
##DeviceType
##DeviceInfo
##id12 - id38


##tidyverse includes dplyr
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("DescTools")
install.packages("cattonum")
install.packages("caret")
install.packages("corrplot")
install.packages("randomForest")
install.packages("pROC")
install.packages("DMwR")
install.packages("e1071", dep = TRUE)
install.packages("ROCR")
install.packages("xgboost")
install.packages("ranger")
library(ranger)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(DescTools)
library(cattonum)
library(caret)
library(corrplot)
library(randomForest)
library(DMwR)
library(pROC)
library(gbm)
library(ROCR)
library(xgboost)
library(e1071)

##begin reading tables into memory

trainidentity <- read.csv("C:/rdata/project/train_identity.csv",na.strings = c("NA",""," "),stringsAsFactors = F)
traintransaction <- read.csv("C:/rdata/project/train_transaction.csv",na.strings = c("NA",""," "),stringsAsFactors = F)

testidentity <- read.csv("C:/rdata/project/test_identity.csv",na.strings = c("NA",""," "),stringsAsFactors = F)
testtransaction <- read.csv("C:/rdata/project/test_transaction.csv",na.strings = c("NA",""," "),stringsAsFactors = F)

##str(trainidentity)
##summary(trainidentity)
##head(trainidentity,0)

##summary(traintransaction)
##str(traintransaction)
##head(traintransaction,0)

traintransactionidentityinner <- traintransaction %>% inner_join(trainidentity)
fraudtranscountinner <- length(which(traintransactionidentityinner$isFraud==1))
legittranscountinner <- length(which(traintransactionidentityinner$isFraud==0))

rm(traintransactionidentityinner)
gc()
## on an inner join we'd miss half the fraud values and limit ourselves to a little of a quarter of the non fraud values
## therefore, go with left join instead


traintransactionfull <- traintransaction %>% left_join(trainidentity)
testtransactionfull <- testtransaction %>% left_join(testidentity)

rm(trainidentity,traintransaction,testidentity,testtransaction)

gc()

##summary(traintransactionfull)
##str(traintransactionfull)
##head(traintransactionfull,0)

##traintransactionfull[traintransactionfull==""] <- NA


##Figure out top 20% non NA fields and fields with no NA apply to test and train


getnafields=function(x){any(is.na(x))}

traincheck.na=apply(traintransactionfull,2,getnafields)
##testcheck.na=apply(testtransactionfull,2,getnafields);

trainnacolsdf <- data.matrix(traincheck.na)
##testnacolsdf <- data.matrix(testcheck.na)

trainnacolsdfNoNA <-data.matrix(trainnacolsdf[which(traincheck.na==FALSE),])
##testnacolsdfNoNA <-data.matrix(testnacolsdf[which(testcheck.na==FALSE),])

trainNonNAmatrixrows <- row.names(trainnacolsdfNoNA)
##testNonNAmatrixrows <- row.names(testnacolsdfNoNA)

trainnacolsdfhasNA <-data.matrix(trainnacolsdf[-which(traincheck.na==FALSE),])
##testnacolsdfhasNA <- data.matrix(testnacolsdf[-which(testcheck.na==FALSE),])

##testNonNAmatrixrows <- row.names(testnacolsdfNoNA)
trainNAmatrixrows <- row.names(trainnacolsdfhasNA)
##testNAmatrixrows <- row.names(testnacolsdf)
##trainNonNAmatrixrows
##trainNAmatrixrows[1]

traintotalrows <- NROW(traintransactionfull)
testtotalrows <- NROW(testtransactionfull)

trainNAmatrixrowcount <- NROW(trainNAmatrixrows)
##testNAmatrixrowcount <- NROW(testNAMatrixrows)

trainNAmatrixLoop <- c(1:trainNAmatrixrowcount)
##testNAmatrixLoop <- c(1:testNAMatrixrowcount)

for (y in 1:trainNAmatrixrowcount)
{
trainNApercentagerow <- c(trainNAmatrixrows[y],sum(is.na(traintransactionfull[which(colnames(traintransactionfull)==trainNAmatrixrows[y])])/traintotalrows*100))
if (y==1)
{
        trainNApercentageSummary <- trainNApercentagerow
}
else
{
        trainNApercentageSummary <- rbind(trainNApercentageSummary, trainNApercentagerow)
}
}

##for (y in 1:testNAmatrixrowcount)
##{
##  testNApercentagerow <- c(testNAmatrixrows[y],sum(is.na(testtransactionfull[which(colnames(testtransactionfull)==testNAmatrixrows[y])])/testtotalrows*100))
##  if (y==1)
##  {
##    testNApercentageSummary <- testNApercentagerow
##  }
##  else
##  {
##    testNApercentageSummary <- rbind(testNApercentageSummary, testNApercentagerow)
##  }
##}


colnames(trainNApercentageSummary) <- c("FieldName", "PercentNA")
##colnames(testNApercentageSummary) <- c("FieldName", "PercentNA")

trainsortedNaPercentageSummary <- trainNApercentageSummary[order(as.numeric(trainNApercentageSummary[,2])),]
trainsortedNaPercentageSummary

traincounttop20percentfieldsNA <- NROW(trainsortedNaPercentageSummary)/5
trainnafieldstoconsider <- trainsortedNaPercentageSummary[c(1:150),]

testnafieldstoconsider <- trainnafieldstoconsider

##testsortedNaPercentageSummary <- testNApercentageSummary[order(as.numeric(testNApercentageSummary[,2])),]
##testsortedNaPercentageSummary

##testcounttop20percentfieldsNA <- NROW(testsortedNaPercentageSummary)/5
##testnafieldstoconsider <- testsortedNaPercentageSummary[c(1:150),]
##testnafieldstoconsider


##d series will be removed.  With the exception of D1 (apparently Days since creditcard began) and D10


##not missing many addresses
##categorical, convert to factor

##sum(is.na(traintransactionfull$addr1))/nrow(traintransactionfull) * 100
##Only 10% do not contain an address code retain for analysis

## a lot of na's... how to replace and with what suggest even distribution of address codes for na's
##sum(is.na(traintransactionfull$addr2))
##unique(traintransactionfull$addr2)

##addr1graph <- table(traintransactionfull$addr1)
##barplot(addr1graph, main="Addr1 locations",xlab="addr1 loc code")
##addr1 might prove useful
##retain for correlation to fraud?

##addr2graph <- table(traintransactionfull$addr2)
##barplot(addr2graph, main="Addr2 locations",xlab="addr2 loc code")
##categorical and consistently 87 with minor variations probably a country code.  Remove from consideration as too redundant

##numeric and more than half are missing values for dist1 and almost all for dist2 discard both
##sum(is.na(traintransactionfull$dist1))
##sum(is.na(traintransactionfull$dist2))

##traintransactionfull$addr1 <- as.factor(traintransactionfull$addr1)
##which(colnames(traintransactionfull)=="addr1")

##Assess Fields with NA in top 20% give or take
##str(traintransactionfull)

trainnaSummaryRowCount <- NROW((trainnafieldstoconsider))
testnaSummaryRowCount <- NROW(testnafieldstoconsider)

for (y in 1:trainnaSummaryRowCount)
{
        if (y==1)
        {
                nacolIDs <- which(colnames(traintransactionfull)==trainnafieldstoconsider[y])
        }
        else
        {
                nacolIDs <- rbind(nacolIDs,which(colnames(traintransactionfull)==trainnafieldstoconsider[y]))
        }

}

trainnacoldf <- as.data.frame(nacolIDs)
trainnacoldf

##make equivalent we'll need the same features in both train and test
testnacoldf <- trainnacoldf

##testnaSummaryRowCount <- NROW((testnafieldstoconsider))

##for (y in 1:testnaSummaryRowCount)
##{
##  if (y==1)
##  {
##    nacolIDs <- which(colnames(testtransactionfull)==testnafieldstoconsider[y])
##  }
##  else
##  {
##    nacolIDs <- rbind(nacolIDs,which(colnames(testtransactionfull)==testnafieldstoconsider[y]))
##  }
##}

##testnacoldf <- as.data.frame(nacolIDs)
##testnacoldf

trainnacolsclasstypesdf <- sapply(traintransactionfull,class)

##use same columns for training on test
testnacolsclasstypesdf <- trainnacolsclasstypesdf
##testnacolsclasstypesdf <- sapply(testtransactionfull,class)
##nacolsclasstypesdf[41]
##impute values based on data type.

##nacolsclasstypesdf[334]
##sum(is.na(traintransactionfull[,i]))
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}

traintransactionfullrowcount <- NROW(traintransactionfull)

testtransactionfullrowcount <- NROW(testtransactionfull)


for (z in 1:trainnaSummaryRowCount)
{
  i <- trainnacoldf[z,]
if (trainnacolsclasstypesdf[i]=="numeric" ||trainnacolsclasstypesdf[i]=="integer")
{
        medianVal <- median(traintransactionfull[,i],na.rm=T)
        tempcol <-replace_na(traintransactionfull[,i],medianVal)
        traintransactionfull[,i] <- tempcol
}else if (trainnacolsclasstypesdf[i]=="logical")  
{
    ##convert to factor
        numberofNAsInCol <- sum(is.na(traintransactionfull[,i]))
        numberofTrueValuesInCol <- sum(which(traintransactionfull[,i]==TRUE))
        numberofFalseValuesInCol <- sum(which(traintransactionfull[,i]==FALSE))
        TvalProportion <- numberofTrueValuesInCol/(numberofTrueValuesInCol+numberofFalseValuesInCol)
        FvalProportion <- numberofFalseValuesInCol/(numberofTrueValuesInCol+numberofFalseValuesInCol)
        numberOfNewTrueValuesToPopulate <- floor(numberofNAsInCol*TvalProportion)
        numberofNewFalseValuesToPopulate <- numberofNAsInCol-numberOfNewTrueValuesToPopulate
        truepopcounter <- 0
        for (t in 1:traintransactionfullrowcount)
        {
            if (is.na(traintransactionfull[t,i]))
            {
               if (truepopcounter <= numberOfNewTrueValuesToPopulate)
               {
                  traintransactionfull[t,i] <- TRUE
                  truepopcounter <- truepopcounter + 1
               }
               else
               {
                 traintransactionfull[t,i] <- FALSE
               }
            }
            
        }
        ##tempcol <- replace_na(traintransactionfull[,i],"None")
        ##as.factor(tempcol)
        traintransactionfull[,i] <- as.factor(traintransactionfull[,i])
}else if (trainnacolsclasstypesdf[i]=="character")
{
        modeVal <- getmode(traintransactionfull[,i])
        tempcolmode <-replace_na(traintransactionfull[,i],modeVal)
        traintransactionfull[,i] <- as.factor(tempcolmode)
        ##convert to categorical
        ##traintransactionfull[,i] <- as.factor(traintransactionfull[,i])
}else
print("unexpected type")
}

##C series did not have null in Train set but does in Test set, set to median
##card2,addr1,card6 on test has NA's

for(i in 17:30){
  testtransactionfull[is.na(testtransactionfull[,i]), i] <- median(testtransactionfull[,i], na.rm = TRUE)
}
##card2
testtransactionfull[is.na(testtransactionfull[,6]), 6] <- median(testtransactionfull[,6], na.rm = TRUE)
##card6
card6mode <- getmode(testtransactionfull[,10])
testtransactionfull[is.na(testtransactionfull[,10]), 10] <- card6mode
##addr1
testtransactionfull[is.na(testtransactionfull[,11]), 11] <- median(testtransactionfull[,11], na.rm = TRUE)
##Need to add
##"D1"             "D10"            "D15"            "V12"   "V279"

##D1
testtransactionfull[is.na(testtransactionfull[,31]), 31] <- median(testtransactionfull[,31], na.rm = TRUE)
##D10
testtransactionfull[is.na(testtransactionfull[,40]), 40] <- median(testtransactionfull[,40], na.rm = TRUE)
##D15
testtransactionfull[is.na(testtransactionfull[,45]), 45] <- median(testtransactionfull[,45], na.rm = TRUE)
##V12
testtransactionfull[is.na(testtransactionfull[,66]), 66] <- median(testtransactionfull[,66], na.rm = TRUE)
##V53
testtransactionfull[is.na(testtransactionfull[,107]), 107] <- median(testtransactionfull[,107], na.rm = TRUE)

##V279
testtransactionfull[is.na(testtransactionfull[,333]), 333] <- median(testtransactionfull[,333], na.rm = TRUE)

##keep consistent fields with train, but evaluate test
for (z in 1:trainnaSummaryRowCount)
{
  i <- testnacoldf[z,]
  if (testnacolsclasstypesdf[i]=="numeric" ||testnacolsclasstypesdf[i]=="integer")
  {
    medianVal <- median(testtransactionfull[,i],na.rm=T)
    tempcol <-replace_na(testtransactionfull[,i],medianVal)
    testtransactionfull[,i] <- tempcol
  }else if (testnacolsclasstypesdf[i]=="logical")  
  {
    ##convert to factor
    numberofNAsInCol <- sum(is.na(testtransactionfull[,i]))
    numberofTrueValuesInCol <- sum(which(testtransactionfull[,i]==TRUE))
    numberofFalseValuesInCol <- sum(which(testtransactionfull[,i]==FALSE))
    TvalProportion <- numberofTrueValuesInCol/(numberofTrueValuesInCol+numberofFalseValuesInCol)
    FvalProportion <- numberofFalseValuesInCol/(numberofTrueValuesInCol+numberofFalseValuesInCol)
    numberOfNewTrueValuesToPopulate <- floor(numberofNAsInCol*TvalProportion)
    numberofNewFalseValuesToPopulate <- numberofNAsInCol-numberOfNewTrueValuesToPopulate
    truepopcounter <- 0
    for (t in 1:testtransactionfullrowcount)
    {
      if (is.na(testtransactionfull[t,i]))
      {
        if (truepopcounter <= numberOfNewTrueValuesToPopulate)
        {
          testtransactionfull[t,i] <- TRUE
          truepopcounter <- truepopcounter + 1
        }
        else
        {
          testtransactionfull[t,i] <- FALSE
        }
      }
      
    }
    ##tempcol <- replace_na(traintransactionfull[,i],"None")
    ##as.factor(tempcol)
    testtransactionfull[,i] <- as.factor(testtransactionfull[,i])
  }else if (testnacolsclasstypesdf[i]=="character")
  {
    modeVal <- getmode(testtransactionfull[,i])
    tempcolmode <-replace_na(testtransactionfull[,i],modeVal)
    testtransactionfull[,i] <- as.factor(tempcolmode)
    ##convert to categorical
    ##traintransactionfull[,i] <- as.factor(traintransactionfull[,i])
  }else
    print("unexpected type")
}


##next step merge na fields on non fields together into separate dataset, remove outliers and then do sampling/train

##get col ID of field

##simply select the fields desired apart from v#'s
##remove the transaction ID (1) field as join has already been performed
##remove the transactionDT (3) field (We do not know the reference date, uncertain how this field can be of help)
##remove addr, dist1, dist2 (13:15)
##remove the C field series (18:31), we have summarized into addressum
##remove the d field series (33:46), this represents time since card began.  D's apart from 1 are difficult to interpret. Only D1 would appear to reliable, the rest have numerous NAs
##remove all the V#'s (56:394) fields for now.  They represent Vesta engineered fields.  The goal is to build our own classifier and not resuse their logic which is impossible to discern
##I considered removing the M series field as well,  even a summary of match counts would not be sufficient because we don't know what's being matched.  NA should not be replaced


##Analysis of response variable
fraudtranscount <- length(which(traintransactionfull$isFraud==1))
legittranscount <- length(which(traintransactionfull$isFraud==0))
##class imbalance exists which must be addressed.  Research suggests SMOTE and undersampling

boxplot(TransactionAmt ~ isFraud, data=traintransactionfull,na.action = NULL )
##find the outliers they must be considered

##trim outliers above on legitmate transactions only at the extreme as outliers and exceptionality can be important for fraud identifcation
##outliers will only be assessed on training as we have prior knowledge of which is fraud and which is not otherwise we want everything
outliertrans <- subset(traintransactionfull, TransactionAmt > 31000)
## two instances so elimnate both interesting that the data is very similar, same transaction on two different cc's

outliers <- c(which(traintransactionfull$TransactionAmt > 31000)[[1]],which(traintransactionfull$TransactionAmt > 31000)[[2]])

traintransactionfullminusoutlier <- traintransactionfull[-outliers,]
##verify removal
which(traintransactionfullminusoutlier$TransactionAmt > 31000)
##two outliers exist > 31000 which represent legitimate transactions (find and remove), box plot distribution appears to be roughly the same with fraud having a higher thrid quartile than no fraud
boxplot(TransactionAmt ~ isFraud, data=traintransactionfullminusoutlier,na.action = NULL,ylim=c(0,5200),horizontal = TRUE)

traintransactionfull <- traintransactionfullminusoutlier

##test normality
##initial inspection
##get random sample and assess normality

##Normality Analysis of TransactionAMT

##fraudamtnormal <- traintransactionfullminusoutlier %>% filter(traintransactionfullminusoutlier$isFraud==1)
##nonfraudamtnormal <- traintransactionfullminusoutlier %>% filter(traintransactionfullminusoutlier$isFraud==0)

##sampletransactionamountdata <- dplyr::sample_n(traintransactionfullminusoutlier,5000)
##samplefraudamountdata <- dplyr::sample_n(fraudamtnormal,5000)
##samplenonfraudamountdata <- dplyr::sample_n(nonfraudamtnormal,5000)

##ggdensity(sampletransactionamountdata$TransactionAmt, main="Density plot of TransactionAMT", xlab="Transacation Amount")
##ggqqplot(sampletransactionamountdata$TransactionAmt)

##ggdensity(samplefraudamountdata$TransactionAmt, main="Density plot of Fraud TransactionAMT", xlab="Transacation Amount")
##ggqqplot(samplefraudamountdata$TransactionAmt)

##ggdensity(samplenonfraudamountdata$TransactionAmt, main="Density plot of Legitimate TransactionAMT", xlab="Transacation Amount")
##ggqqplot(samplenonfraudamountdata$TransactionAmt)

##shapiro.test(sampletransactionamountdata$TransactionAmt)
##shapiro.test(samplefraudamountdata$TransactionAmt)
##shapiro.test(samplenonfraudamountdata$TransactionAmt)

## p-value less that .05 therefore we assume normality for TransactionAmt

##c series #of addresses associated these will be aggregated to a single field
## no empties will sum these fields to aggregate will a high number be better or not?  Aggregate should work and simplify matters as this an address match count and numeric

traintransactionfull$addresssum <- rowSums(traintransactionfull[,18:31])
testtransactionfull$addresssum <- rowSums(testtransactionfull[,17:30])

##remove ID column (1), addr2(13) as it's too redundant and All "C" fields (18:31) because we've summed them up, then remove all that contain NA

traintransactiontemp <- traintransactionfull[-c(1,13,18:31)]

##namecoltoremovefromtest <- names(which(sapply(traintransactiontemp, anyNA)))

nonnulltraintransaction <- traintransactiontemp %>% select_if(function(x){!any(is.na(x))})

##remove addr2(12) as it's too redundant and All "C" fields (17:30) because we've summed them up, then remove all that contain NA
##dist1, d2,d11,m1,v84,v138,v35,v322 removed from test to make it consistent with train
testTransactionIDs <- testtransactiontemp$TransactionID

testtransactiontemp <- testtransactionfull[-c(1,12,13,17:30,32,41,46,89,138,192,376)]

##colIdstoremovefromtest <- which(sapply(testtransactiontemp, anyNA))-1

##testtransactiontemp <- testtransactiontemp[-colIdstoremovefromtest]

testtransactiontemp <- testtransactiontemp %>% select_if(function(x){!any(is.na(x))})

##test vs train disparity need to remove
##"dist1"          "D2"             "D11"            "M1" 
##testtransactiontemp <- testtransactiontemp[-c(13,32,41,46)]
##nonnulltesttransaction <- testtransactiontemp %>% select_if(function(x){!any(is.na(x))})
nonnulltesttransaction <- testtransactiontemp



##colnames(traintransactionfull)
colnames(testtransactionfull)
##sum(is.na(traintransactionfull$addresssum))
##sum(is.na(testtransactionfull$addresssum))

colnames(nonnulltraintransaction[,-1])
colnames(nonnulltesttransaction)

rm(traintransactionfull,traintransactiontemp,testtransactiontemp,testtransactionfull)
gc()


##Convert to factors (if not already)
##Categorical Features:
##        ProductCD
##card1 - card6
##addr1, addr2
##Pemaildomain Remaildomain
##M1 - M9


##nonnulltraintransaction$card1 <- as.factor(nonnulltraintransaction$card1)
##nonnulltraintransaction$card2 <- as.factor(nonnulltraintransaction$card2)
##nonnulltraintransaction$card3 <- as.factor(nonnulltraintransaction$card3)
##nonnulltraintransaction$card5 <- as.factor(nonnulltraintransaction$card5)
##nonnulltraintransaction$addr1 <- as.factor(nonnulltraintransaction$addr1)
nonnulltraintransaction$ProductCD <- as.factor(nonnulltraintransaction$ProductCD)
nonnulltesttransaction$ProductCD <- as.factor(nonnulltesttransaction$ProductCD)
nonnulltesttransaction$card4 <- as.factor(nonnulltesttransaction$card4)
nonnulltesttransaction$card5 <- as.numeric(nonnulltesttransaction$card5)
nonnulltesttransaction$addr1 <- as.numeric(nonnulltesttransaction$addr1)
nonnulltesttransaction$TransactionDT <- as.numeric(nonnulltesttransaction$TransactionDT)
nonnulltesttransaction$card6 <- as.factor(nonnulltesttransaction$card6)

##change row valuess with "debit or credit" NOTE: none has fraud.  Feature causes issues on encoding on testset which lacks examples change to credit

which(nonnulltraintransaction$card6=="debit or credit" & nonnulltraintransaction$isFraud==1)

rowIDsToUpdate <- as.vector(which(nonnulltraintransaction$card6=="debit or credit"))
card6colIndex <- which(colnames(nonnulltraintransaction)=="card6")
##set the factor here prior to matrix conversion

nonnulltraintransaction[rowIDsToUpdate,card6colIndex] <- as.factor("credit")
nonnulltraintransaction<-droplevels(nonnulltraintransaction)


##create useful categorical label for fraud

##build training set using SMOTE and ROSE

set.seed(500)

##try using create folds instead?


splitIndex <- createDataPartition(nonnulltraintransaction$isFraud, list=FALSE, times = 1,p=.8)

##assess original non na first

trainSplit <- nonnulltraintransaction[splitIndex,]
validationSplit <- nonnulltraintransaction[-splitIndex,]

##check percentages

prop.table(table(trainSplit$isFraud))
prop.table(table(nonnulltraintransaction$isFraud))
##verifed to be roughly equivalent


##SMOTE requires a factor
trainSplit$Fraud <- ifelse (trainSplit$isFraud==1, "Y","N")
trainSplit$Fraud <- as.factor(trainSplit$Fraud)

##apply SMOTE

##try to balance the data for training SMOTE first
##SMOTEtrain <- SMOTE(Fraud ~., data=trainSplit,perc.under=100,perc.over=200)
##prop.table(table(SMOTEtrain$isFraud))
##not exactly 50/50 so let's try again (multiple attempts result in the following)
SMOTEtrain <- SMOTE(Fraud ~., data=trainSplit,perc.under=150,perc.over=222)
prop.table(table(SMOTEtrain$isFraud))

##https://towardsdatascience.com/understanding-gradient-boosting-machines-9be756fe76ab

numberOfCollsInSMOTEtrain <- NCOL(SMOTEtrain)

SMOTEtrainLessFactorFraud<-SMOTEtrain[,-numberOfCollsInSMOTEtrain]
maxInteractionDepth <- floor(sqrt(NCOL(SMOTEtrainLessFactorFraud)))


##Random Forest using Ranger (beware 17 hours to process)
rfControl <- trainControl(method="repeatedcv",number=10,verboseIter = TRUE,repeats = 5)

dmy <- dummyVars("~.",data=SMOTEtrainLessFactorFraud,fullRank = T)
dffactorless <- data.frame(predict(dmy,newdata=SMOTEtrainLessFactorFraud))
ydffactorless <- dffactorless$isFraud

rfDefault <- caret::train(as.factor(isFraud)~.,data=dffactorless,method="ranger",trControl=rfControl,metric="Accuracy",num.trees=100)

rfPredTrain <- predict(rfDefault,newdata = dffactorless)

trainRFCF <- confusionMatrix(rfPredTrain,as.factor(ydffactorless))
trainRFCF

train_roc_obj <- roc(ydffactorless, as.numeric(rfPredTrain))
rfTrainAUC <- auc(train_roc_obj)

dmyVal <- dummyVars("~.",data=testSplit,fullRank = T)
dfValFactorless <- data.frame(predict(dmyVal,newdata=testSplit))
ydfValFactorless <- dfValFactorless$isFraud

rfPredVal <- predict(rfDefault, newdata = dfValFactorless)

ValRFCF <- confusionMatrix(rfPredVal,as.factor(ydfValFactorless))
ValRFCF

val_roc_obj <- roc(ydfValFactorless,as.numeric(rfPredVal))
rfValAUC <-auc(val_roc_obj)
rfValAUC
##End Random Forest Ranger


##Begin GBM model
gbmTrainModel <- gbm(isFraud~.,data=SMOTEtrainLessFactorFraud,distribution="bernoulli",n.trees=100,interaction.depth = maxInteractionDepth,shrinkage=.1,cv.folds = 10)

gbmTrainPerformancecv <- gbm.perf(gbmTrainModel,method="cv")
##playing with perf does not appear to help, the value equates to the ntrees value...
##testing the n.trees number 500 appears a good break point
##research indicates cv method is better than OOB
print(gbmTrainPerformancecv)
dfSummary <- as.data.frame(summary(gbmTrainModel))

gbmValPredictor <- predict(object=gbmTrainModel, newdata=validationSplit[,-1],n.trees = 100,type="response")

gbmTrainPredictor <- predict(object=gbmTrainModel,newdata = SMOTEtrainLessFactorFraud[,-1], n.trees=100, type="response")

##evaluate results
validationpredictionThreshold <- as.factor(ifelse(gbmValPredictor>.75,1,0))
TrainpredictionThreshold <- as.factor(ifelse(gbmTrainPredictor>.75,1,0))

validationFraud <- as.factor(validationSplit$isFraud)
trainFraud <- as.factor(SMOTEtrainLessFactorFraud$isFraud)

ValCF <- confusionMatrix(validationpredictionThreshold,validationFraud)
TrainCF <-confusionMatrix(TrainpredictionThreshold,trainFraud)

##evaluate performance
##ROC curve
gbmPredVal <- prediction(gbmValPredictor, validationSplit$isFraud)
gbmPredTrain <- prediction(gbmTrainPredictor, SMOTEtrainLessFactorFraud$isFraud)
gbmROCVal <- performance(gbmPredVal,"tpr","fpr")
plot(gbmROCVal)
plot(gbmROCVal, add=TRUE, col="green")
legend("right", legend=c("GBM"),col=c("green"),lty=1:2,cex=0.6)

##AUC
aucVal <- performance(gbmPredVal,"auc")
aucTrain <- performance(gbmPredTrain,"auc")
gbmaucval <- as.numeric(aucVal@y.values)
gbmauctrain <- as.numeric(aucTrain@y.values)

gbmauctrain
gbmaucval
TrainCF
ValCF
##End GBM

##begin xgboost analysis

##convert catgeoricals (one hot)

xgbtraincontrol <- trainControl(method="cv", number = 10)
x_xgbModelTrainMatrix <- model.matrix(~.+0,data = SMOTEtrainLessFactorFraud[,-1]) 

y_xgbModelTrain <- SMOTEtrainLessFactorFraud$isFraud
##colnames(x_xgbModelTrainMatrix)
x_xgbModelValMatrix <- model.matrix(~.+0,data = validationSplit[,-1])
y_xgbModelVal <- as.factor(validationSplit$isFraud)

xgbParams <- list(objective="binary:logistic",eta=.1,max.depth=12,eval_metric="auc")

xgb2 <- xgboost(data=x_xgbModelTrainMatrix,trControl=xgbtraincontrol,label=y_xgbModelTrain,params=xgbParams,nrounds=5000,verbose=TRUE,print_every_n = 100, early_stopping_rounds = 10)


xgbpredtrain <- predict(xgb2,x_xgbModelTrainMatrix)

xgbpredTrainThreshold <- as.factor(ifelse(xgbpredtrain>.75,1,0))

xgbTrainCF <- confusionMatrix (xgbpredTrainThreshold, as.factor(y_xgbModelTrain))
xgbTrainCF

xgb_roc_train <- roc(y_xgbModelTrain, xgbpredtrain, algorithm = 2)
plot(xgb_roc_train) 
xgbtrainauc <- auc(xgb_roc_train)
xgbtrainauc

xgbpredval <- predict(xgb2,x_xgbModelValMatrix)

xgbpredvalthreshold <- as.factor(ifelse(xgbpredval>.75,1,0))

xgbValCF <- confusionMatrix (xgbpredvalthreshold, as.factor(y_xgbModelVal))
xgbValCF

roc_val <- roc(y_xgbModelVal, xgbpredval, algorithm = 2)
plot(roc_val) 
xgbvalauc <- auc(roc_val)
xgbvalauc
##END Xgboost Analysis

##Apply best model to Kaggle Test set
##As Xgboost has the best over accuracy and AUC proceed to use the testing set DF
colnames(nonnulltesttransaction)
colnames(x_xgbModelTrainMatrix)
##colnames(SMOTEtrainLessFactorFraud[,-1])


names(which(sapply(nonnulltesttransaction, anyNA)))

x_xgbModelTestMatrix <- model.matrix(~.+0,data = nonnulltesttransaction)

##xgb3 <- xgboost(data=x_xgbModelTestMatrix,,params=xgbParams,nrounds=5000,verbose=TRUE,print_every_n = 100, early_stopping_rounds = 10)
##xgb2$feature_names
##class(x_xgbModelTestMatrix)
##class(xgb2)

colnames(x_xgbModelTestMatrix)
xgb2$feature_names
xgbpredTest <- predict(xgb2,newdata=x_xgbModelTestMatrix)

##generate predictions from test data
xgbpredTestthreshold <- ifelse(xgbpredTest>.75,.5,0)

##class(testTransactionIDs)
kagglesubmissionset <- cbind(testTransactionIDs,xgbpredTestthreshold)















