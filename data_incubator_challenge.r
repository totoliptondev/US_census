# data downloaded from http://www.census.gov/programs-surveys/acs/data/pums.html, -> 2014 ACS 1-year PUMS CsV format.
# data dictionary: http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict14.txt
setwd("K:/Data_analytics/US_census")
library("dplyr")
library("data.table")
library(DAAG)
library("xgboost")

# The following are some factors that could be related to income level. 
# ST: state
# JWMNP: Travel time to work (removed due to too many missing entry)
# MAR: Marital status 
# SCHL: Education level       
# SEX        
# WKHP: Usual hours worked per week past 12 months        (removed due to too many missing entry)
# ESR: Employment status        
# OC: own children         
# PINCP: Total person's income      
# RAC1P: race       
# FOD1P: field of degree	   (removed due to too many missing entry)
# QTRBIR: quater of birth?
# SOCP: occupation
hasRead = TRUE

##read data and save it as RData to save time nect time:
if(!hasRead){

  
  colsToKeep <- c("PINCP", "SCHL", "ESR", "ST",  "MAR","SEX", "OC", "RAC1P", "SOCP", "AGEP")

  popData <- fread("K:/Data_analytics/US_census/pums/ss14pusa.csv" , select=colsToKeep )  
  popData2 <- fread("K:/Data_analytics/US_census/pums/ss14pusb.csv", select=colsToKeep )
  popData3 <- fread("K:/Data_analytics/US_census/pums/ss13pusa.csv", select=colsToKeep )
  popData4 <- fread("K:/Data_analytics/US_census/pums/ss13pusb.csv", select=colsToKeep )

  popData <- rbind(popData, popData2, popData3, popData4)
  
  rm(popData2, popData3, popData4)
  
  save(popData, file="popData.Rdata")
}else{
  load("popData.Rdata")
} 



popData <- tbl_df(popData) 
popData$SOCP <- substring(popData$SOCP, 1, 2)



popData$SOCP[is.na(popData$SOCP) ] <- 0
ds <- popData %>% 
      na.omit() 

ds$PINCP <- ds$PINCP/1000

ds$SCHL[ds$SCHL<21 ] = 0
ds$SCHL[ds$SCHL == 21] = 1
ds$SCHL[ds$SCHL == 22] = 2
ds$SCHL[ds$SCHL == 23] = 2
ds$SCHL[ds$SCHL == 24] = 3

ds$ESR[ds$ESR == 3] = 0
ds$ESR[ds$ESR == 6] = 0
ds$ESR[ds$ESR != 0] = 1

#ds$MAR[ds$MAR != 1 ] = -1
#ds$OC <- ds$OC + 1
train <-  sample_n(ds,500000)

# 10 higest, 0 lowest
# occupation_map = c(  "11" = "6", "13" = "5", "15" = "8", "17" = "8", # managers and engineers
#                      "19" = "7", "21" = "4", "25" = "3", "27" = "8", # scientist, social worker, educator
#                      "29" = "10", "31" = "6", "33" = "4", "35" = "3", # doctors, cooks
#                      "37" = "1", "39" = "2", "41" = "3", "43" = "5", # janitors, sales
#                      "45" = "3", "47" = "6", "49" = "5", "51" = "4", # social worker, 231 is laywer
#                      "53" = "6", "55" = "5", "99" = "1") # drivers


train$SOCP <- as.numeric(train$SOCP)
train$SOCP[is.na(train$SOCP)] <- 0

trainData <- model.matrix(~(.-PINCP)^2 + (.-PINCP)^3 + PINCP, train)
trainData <- tbl_df(data.frame(trainData))

fit <- lm(PINCP ~ ., data = trainData)



test = sample_n(ds,100000)


test$SOCP <- as.numeric(test$SOCP)
test$SOCP[is.na(test$SOCP)] <- 0

testData <- model.matrix(~(.-PINCP)^2 + (.-PINCP)^3 + PINCP, test)
testData <- tbl_df(data.frame(testData))
res = predict(fit,testData )
new <- rbind(res, ds$PINCP[1:10000])



sample <- c(50, 20, 1, 23, 2, 1, 1, 1, 47)
names(sample) <- c("ST", "AGEP","MAR", "SCHL","SEX", "ESR", "OC", "RAC1P", "SOCP")
sample <- tbl_df(data.frame(t(sample)))
test_sample <- model.matrix(~(.)^2 + (.)^3 , data = sample )
test_sample <- tbl_df(data.frame(test_sample))
ans <- predict(fit, test_sample )
ans


param <- list("objective" = "reg:linear",    
              "eval_metric" = "logloss",    
              "max_depth" = 20,   
              "eta" = 0.02,             # step size shrinkage 
              "subsample" = 0.8,        # part of data instances to grow tree 
              "colsample_bytree" = 0.7  # subsample ratio of columns when constructing each tree 
)

feature.names <- names(trainData)[c(-1, -11)]

clf <- xgboost(data        = data.matrix(trainData[ ,feature.names]),
               label       = train$PINCP,
               param       = param,
               nrounds     = 40)
               #early.stop.round = 20)

res <- xgb.cv(param, data = data.matrix(trainData[1:100000 ,feature.names]), label = train$PINCP, nround = 60, nfold = 5)


