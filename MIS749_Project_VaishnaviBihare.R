library(caret)
library(dplyr)
library(tidyverse)
library(MASS)
library(arm)
library(missForest) 
library(psych)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(corrplot)

#Importing the train and test data

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)

#Structure of the data

dim(train)
str(train)

#Percentage of Missing data in train dataset
sum(is.na(train)) / (nrow(train) *ncol(train))

#Checking for duplicate row
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

#The housing train data set has 1460 rows and 81 features with the target feature Sale Price.

dim(test)
str(test)

#Percentage of missing data in test dataset
sum(is.na(test)) / (nrow(test) * ncol(test))

#The housing test data set has 1459 rows and 80 features with the target feature Sale Price.

#Combine them together (Later use)
test$SalePrice<-rep(NA,1459)

train$isTrain <- 1

test$isTrain <- 0

house_data <- rbind(train,test)
dim(house_data)

#Hence we have 2919 rows and 82 colums in total

#Data Pre-Proccessing

numeric_variables <- which(sapply(train, is.numeric))
numeric_headers <- names(numeric_variables)
cat('Here we have', length(numeric_variables),'numeric variables')
# There are actually 37 numeric predictors, as isTrain and ID are not predictors.

character_variables <- which(sapply(train, is.character))
character_header <- names(character_variables)
cat('There are', length(character_variables),'character variables')

summary(train$SalePrice)


#Understanding the Variables

#Response Variable SalePrice

options(scipen=10000)
ggplot(data = train[!is.na(train$SalePrice),], aes(x=SalePrice, fill= ..count..))+ 
  geom_histogram(binwidth = 10000)+
  scale_x_continuous(breaks = seq(0,800000, by=100000), labels= comma)+
  ggtitle("Histogram respresenting Sale Price")+
  xlab("Housing Price")+
  ylab("Count of Houses")+
  theme(plot.title = element_text(hjust =0.5))

#It is right skewed, lets take the log to make it normally distributed

train$SalePrice <- log(train$SalePrice )

ggplot(train, aes(x=SalePrice, fill=..count..))+geom_histogram(binwidth = 0.05)+
  ggtitle("Histogram of normally distributed SalePrice")+
  ylab("Count of Houses")+
  xlab("Housing Prices")+
  theme(plot.title = element_text(hjust = 0.5))

#MSZoning vs SalePrice
ggplot(data = train[!is.na(train$MSZoning) & train$SalePrice > 0,], aes(x = MSZoning, y = SalePrice)) +
  geom_boxplot(na.rm = T) +
  theme_minimal() +
  labs(x = "MSZoning",
       y = "SalePrice",
       title = "MSZoning vs. Sale Price")


#Year Built and SalePrice
ggplot(data = train[train$SalePrice>0,], aes(x = YearBuilt, y = SalePrice)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "loess", se = FALSE, na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Year Built",
       y = "Sale Price",
       title = "Year Built vs. Sale Price") 


#Fireplace vs SalePrice
ggplot(data = train[train$SalePrice>0,], aes(x = FireplaceQu, y = SalePrice)) +
  geom_boxplot(na.rm = T) +
  theme_minimal() +
  labs(x = "Fireplace Quality",
       y = "Sales Price",
       title = "Fireplace Quality vs. Sales Price")

#We can see NA's, we'll remove them later

#Let's do overallQual next, as it is impotant to understand how sale price is affected by the Overall Quality

ggplot(data=train[!is.na(train$SalePrice),], aes(x=factor(OverallQual),y=SalePrice))+
  geom_boxplot(color='green')+
  scale_y_continuous(breaks = seq(0, 800000, by=100000), labels = comma)+
  labs(x="Overall Quality")

#we can see positive correlation here

#Next is  Above grade living area

ggplot(data=train[!is.na(train$SalePrice),], aes(x=GrLivArea,y=SalePrice))+
  geom_point(col='red')+ geom_smooth(method = "lm", se= FALSE, color='black', aes(group=1))+
  scale_y_continuous(breaks = seq(0, 800000, by=100000), labels = comma)

#we can see positive correlation here also, lets verify it by a correlation plot

#Correlation of the numeric values 

data_nv <- train[,numeric_variables]
cor_va <- cor(data_nv, use="pairwise.complete.obs")

#Sorting the values with the decreasing correlation with the SalePrice
sorted_cor <- as.matrix(sort(cor_va[,'SalePrice'], decreasing = TRUE))
high_cor <- names(which(apply(sorted_cor, 1, function(x) abs(x)>0.5)))
cor_va <- cor_va[high_cor,high_cor]

corrplot.mixed(cor_va, tl.col='Black', tl.pos="lt")


#Data Cleaning

#Dropping ID as it is not necessary for prediction
house_data= house_data[-1]
#view(house_data)

# To find number of missing value for all variable in combined dataset (Train+Test)

sapply(house_data[,1:80], function(x) sum(is.na(x)))

house_data$Alley [is.na(house_data$Alley)] <- "Unknown"
house_data$MasVnrType[is.na(house_data$MasVnrType)] <- "Unknown"
house_data$BsmtQual[is.na(house_data$BsmtQual)] <- "Unknown"
house_data$BsmtCond[is.na(house_data$BsmtCond)] <- "Unknown"
house_data$BsmtExposure[is.na(house_data$BsmtExposure)] <- "Unknown"
house_data$BsmtFinType1[is.na(house_data$BsmtFinType1)] <- "Unknown"
house_data$BsmtFinType2[is.na(house_data$BsmtFinType2)] <- "Unknown"
house_data$Electrical[is.na(house_data$Electrical)] <- "Unknown"
house_data$FireplaceQu[is.na(house_data$FireplaceQu)] <- "Unknown"
house_data$GarageType[is.na(house_data$GarageType)] <- "Unknown"
house_data$GarageFinish[is.na(house_data$GarageFinish)] <- "Unknown"
house_data$GarageQual[is.na(house_data$GarageQual)] <- "Unknown"
house_data$GarageCond[is.na(house_data$GarageCond)] <- "Unknown"
house_data$PoolQC[is.na(house_data$PoolQC)] <- "Unknown"
house_data$Fence[is.na(house_data$Fence)] <- "Unknown"
house_data$MiscFeature[is.na(house_data$MiscFeature)] <- "Unknown"

house_data$LotFrontage[is.na(house_data$LotFrontage)] <- round(mean(house_data$LotFrontage,na.rm=TRUE))
house_data$MasVnrArea[is.na(house_data$MasVnrArea)] <- round(mean(house_data$MasVnrArea,na.rm=TRUE))
house_data$GarageYrBlt[is.na(house_data$GarageYrBlt)] <- round(mean(house_data$GarageYrBlt,na.rm=TRUE))


elements <- names(house_data)
elements <- elements[elements != "SalePrice"]
for(i in elements)
{
  if(is.character(house_data[[i]]))
  {
    levels <- sort(unique(c(house_data[[i]])))
    house_data[[i]] <- factor(house_data[[i]],levels=levels)
  }
}

for (i in elements) 
{
  if(class(levels(house_data[[i]])) == "character")
    house_data[[i]] <- seq_along(levels(house_data[[i]]))[house_data[[i]]]
}

str(house_data)

#rmse
rmse <- function(actualVal, predictedVal) 
{
  sqrt(mean((actualVal - predictedVal)^2))
}


#Model Creation

#Seting the seed to make the output reproduceable
set.seed(100)

#Creating test and train data
train <- house_data[house_data$isTrain==1,]

test <- house_data[house_data$isTrain==0,]

#Model Development

set.seed(15)

linearmodel <- lm(SalePrice ~ . , data=train)

varImp(linearmodel)

head(sort(abs(linearmodel$coefficients),decreasing = TRUE),n=16)

#Hence the strongest predictors from linear model are: Pool Quality, Types of Utilities available,
#Street, Garage Cars, Overall Quality, External Quality, Condition2, BsmtQual, Kitched Qual, BsmtFullBath,
#RoofMatl, LandSlope, Heating, Overall Condition

#now lets do Ridge Regression

ridge_model <- train(SalePrice ~ . , data=train, preProcess= c("center", "scale"),method = "glmnet",
                     tuneGrid= expand.grid(alpha=0,lambda = seq(0,10, .1)))
varImp(ridge_model)

#laaso regression

lassoModel <- train(SalePrice ~ ., data = train, preProcess = c("center", "scale"),method = "glmnet",
                    tuneGrid= expand.grid(alpha=1,lambda = seq(0,10, .1)))

varImp(lassoModel)

#lasso and ridge model(Mix model)
mixModel <- train(SalePrice ~ ., data = train, preProcess = c("center", "scale"),method = "glmnet",
                  tuneGrid= expand.grid(alpha=0:1,lambda = seq(0,10, .1)))

varImp(mixModel)

#Comparing the output of all the above models and techniques we reached to a conclusion that the strongest 
#predictors which can be used to develop a model are: 
#PoolQC,GrLivArea, X2ndFlrSF ,GarageCars,OverallQual,KitchenQual,BsmtQual,OverallCond,X1stFlrSF,
#MSSubClass,PoolArea,ExterQual,MasVnrArea,TotRmsAbvGrd,GarageArea,LotArea

##Different modeling methods were then applied on the above predictors to find the most accurate model.

ctrl <- trainControl(method = "cv", number=10)
set.seed(100)

#Linear Model
linear.model <- train(SalePrice ~ PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                      + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                        MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                      data = train,
                      method = "lm", trControl=ctrl)

linear.model

#Out Sample RMSE and Rsquare value

RMSE_linear <- rmse(train$SalePrice, predict(linear.model,data = newtrain))

#Insample RMSE value
RMSE_linear


#Ridge Regression
model.ridge <- train(SalePrice ~ PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                     + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                       MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                     data = train, preProcess = c("center", "scale"),
                     method = "ridge",trControl=ctrl)

model.ridge
#Out Sample RMSE and Rsquare value
RMSE_ridge <- rmse(train$SalePrice, predict(model.ridge,data = newtrain))

#Insample RMSE value
RMSE_ridge

#Lasso model
model.lasso <- train(SalePrice ~ PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                     + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                       MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                     data = train, preProcess = c("center", "scale"),
                     method = "lasso", trControl=ctrl)

model.lasso
#Out Sample RMSE and Rsquare value

RMSE_lasso <- rmse(train$SalePrice, predict(model.lasso,data = newtrain))

#Insample RMSE value
RMSE_lasso

#Comparing the above model

c.models<- list("LM"=linear.model, "Ridge" = model.ridge,
                "Lasso" = model.lasso)

house_price.resamples<- resamples(c.models)
summary(house_price.resamples)

#plot performances
bwplot(house_price.resamples, metric="RMSE")
bwplot(house_price.resamples, metric="Rsquared")


#Lets apply other techniques to get the best result
#kNN
model.Knn <- train(SalePrice ~ PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                   + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                     MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                   data = train, preProcess = c("center", "scale"),
                   method = "knn", trControl=ctrl)

model.Knn
#Out Sample RMSE and Rsquare value

RMSE_knn <- rmse(train$SalePrice, predict(model.Knn,data = newtrain))

#Insample RMSE value
RMSE_knn

#Random Forest
library(randomForest)

model.rf<- randomForest(SalePrice~PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                            + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                              MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                            data = train, trControl=ctrl)

importance<- importance(model.rf)

varImpPlot(model.rf)

model.rf

RMSE_rf <- rmse(train$SalePrice, predict(model.rf,newdata = train))
#Insample RMSE value
RMSE_rf

#M5P Model
library(RWeka)
model.m5p <- M5P(SalePrice ~ PoolQC + GrLivArea  + GarageCars + GarageArea + TotRmsAbvGrd +OverallQual
                 + KitchenQual + BsmtQual + OverallCond + X1stFlrSF  + PoolArea + ExterQual + 
                   MasVnrArea + MSSubClass + X2ndFlrSF + LotArea ,
                 data = train)

model.m5p

#Out Sample RMSE and Rsquare value
RMSE_m5p <- rmse(train$SalePrice, predict(model.m5p,newdata = train))

#Insample RMSE value
RMSE_m5p


## Model Comparison


#This plot compares the RMSEs for all the models and shows the minimum and maximum RMSE of the models. 
#This helped me  finalize the best model.

test.rmse <- data.frame( model = c("lm", "ridge", "lasso", "knn", "rf", "m5p"),
                         rmse = c(RMSE_linear,RMSE_ridge, RMSE_lasso, RMSE_knn, RMSE_rf, RMSE_m5p))


test.rmse <- test.rmse[order(test.rmse$rmse, decreasing = TRUE),]

test.rmse$model <- factor(test.rmse$model, levels = test.rmse$model)

plot(test.rmse, main = "Model Comparison")


#Prediction using M5P model

prediction <- predict(model.rf, test)

prediction <- data.frame( prediction= prediction)

write.table(prediction, file="Prediction1.csv", row.names=FALSE, col.names=TRUE)






