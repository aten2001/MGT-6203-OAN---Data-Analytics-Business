library(readr)
dirmrt = read.csv("direct_marketing.csv")
model = lm(AmountSpent ~ Gender + Salary, data = dirmrt)
summary (model)
head(dirmrt,4)

dirmrt$IsMarried <- ifelse(dirmrt$Married == "Married", 1, 0)

model2 = lm(AmountSpent ~ Gender + Salary + IsMarried, data = dirmrt)
summary (model2)


#######################################################################

library(MASS)
head(Boston,4)
library(MASS)
library(ROCR)

Boston$Result <- ifelse(Boston$medv > 30,1,0)
names(Boston)

Boston$medv <- NULL
# Apply logistic regression algorithm on Boston data set train

logis <- glm(Result ~ ., data = Boston, family = binomial)

summary(logis)

# Predict using the model built
Boston$pred <- predict(logis,data=Boston,type="response")
Boston$predictvalue <- ifelse(Boston$pred>0.8, 1,0)

# Calculate confusion matrix
confusion_matrix <- table(actual=Boston$Result,Boston$predictvalue)

g <- roc.plot(Boston$Result, Boston$predictvalue)
g
auc(g)


#######################################################################
# install.packages("PerformanceAnalytics")
dt <- read.csv("AMZN.csv",header=TRUE)
# arithmetic return
rtn <- diff(dt$Adj_Close)/dt$Adj_Close[-length(dt$Adj_Close)]
# omit the first row
dt <- dt[-1,]
# add the return to the data
dt$rtn <- rtn
# add one column of monthly risk-free rate
dt$rf <- 0.19/100
# calculate the sharpe ratio using the function
library(PerformanceAnalytics)
row.names(dt) <- as.Date(dt$Date, format=c("%m/%d/%Y"))
sharp = SharpeRatio(dt[,3,drop=FALSE],Rf=dt[,4,drop=FALSE])
sharp #-0.342 
#sharpe ratio is negative because excess-return is negative


prod(dt$rtn+1)-1

#######################################################################

library(dplyr)
UPS_KO <-read.csv("UPS_KO.csv",header=TRUE)
UPS_KO <- UPS_KO %>% mutate(UPS_RF = UPS - RF)%>% mutate(KO_RF = KO - RF)
head(UPS_KO,4)

factor_model_UPS <- lm(UPS_RF~Mkt_RF +SMB+ HML, UPS_KO)
summary(factor_model_UPS)
factor_model_KO <- lm(KO_RF~Mkt_RF +SMB+ HML, UPS_KO)
summary(factor_model_KO)


#######################################################################

data  <-read.csv("KAG_conversion_data_wrangled.csv",stringsAsFactors = FALSE) 

head(data,4)

data %>% filter(CPC== max(CPC))

round((data %>% filter(CPC >=1 ) %>% nrow())/(data %>% nrow())*100,2)

data %>% filter(campaign_id == 936) %>% group_by(age) %>% summarise(Impr = mean(Impressions)) %>% arrange(desc(Impr))
data %>%  group_by(age) %>% summarise(Impr = length(Impressions)) %>% arrange(desc(Impr))

data %>%group_by(campaign_id) %>% summarise(MeanCPC = mean(CPC)) %>% arrange(MeanCPC)



#######################################################################

Queue <-read.csv("Queue.csv",header=TRUE)
head(Queue,4)
MondayStats <- Queue %>% filter(Day == "Monday") %>% summarise(arrvalRate = length(Customer_number))
MondayArrivalRate <- MondayStats[1,1]/7
MondayServiceRate <- 140
MondayUtilization <- MondayArrivalRate/MondayServiceRate
MondayArrivalRate
MondayServiceRate
MondayUtilization

TuesdayStats <- Queue %>% filter(Day == "Tuesday") %>% summarise(arrvalRate = length(Customer_number))
TuesdayArrivalRate <- TuesdayStats[1,1]/8
TuesdayServiceRate <- 120
TuesdayUtilization <- TuesdayArrivalRate/TuesdayServiceRate
TuesdayArrivalRate
TuesdayServiceRate
TuesdayUtilization


WednesdayStats <- Queue %>% filter(Day == "Wednesday") %>% summarise(arrvalRate = length(Customer_number))
WednesdayArrivalRate <- WednesdayStats[1,1]/7
WednesdayServiceRate <- 200
WednesdayUtilization <- WednesdayArrivalRate/WednesdayServiceRate
WednesdayArrivalRate
WednesdayServiceRate
WednesdayUtilization



i_mm1 <- NewInput.MM1(lambda = TuesdayArrivalRate, mu = TuesdayServiceRate, n = 20)
Report(o_mm1)

#######################################################################


