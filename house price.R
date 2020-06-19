house<- read.csv("~/Downloads/train (2).csv")


#cleaning
house<-house[,-1]
house<-house[,-6]


house$BsmtQual<-ifelse(is.na(house$BsmtQual),"NO",house$BsmtQual)
house$BsmtCond<-ifelse(is.na(house$BsmtCond),"NO",house$BsmtCond)
house$BsmtExposure<-ifelse(is.na(house$BsmtExposure),"NO",house$BsmtExposure)
house$BsmtFinType1<-ifelse(is.na(house$BsmtFinType1),"NO",house$BsmtFinType1)
house$BsmtFinType2<-ifelse(is.na(house$BsmtFinType2),"NO",house$BsmtFinType2)
house$FireplaceQu<-ifelse(is.na(house$FireplaceQu),"NO",house$FireplaceQu)
house$GarageType<-ifelse(is.na(house$GarageType),"NO",house$GarageType)
house$GarageFinish<-ifelse(is.na(house$GarageFinish),"NO",house$GarageFinish)
house$GarageQual<-ifelse(is.na(house$GarageQual),"NO",house$GarageQual)
house$GarageCond<-ifelse(is.na(house$GarageCond),"NO",house$GarageCond)
house$PoolQC<-ifelse(is.na(house$PoolQC),"NO",house$PoolQC)
house$Fence<-ifelse(is.na(house$Fence),"NO",house$Fence)
house$MiscFeature<-ifelse(is.na(house$MiscFeature),"NO",house$MiscFeature)


summary(house$SalePrice)
hist(house$SalePrice)
boxplot(house$SalePrice)


#managing outliers
library(dplyr)
Q<-quantile(house$SalePrice, probs=c(.25, .75))
iqr <- IQR(house$SalePrice)
house<- house%>% filter(SalePrice > (Q[1] - 1.5*iqr) &  +SalePrice< (Q[2] + 1.5*iqr))


#missing values


str(house)
house<-house%>%mutate_if(is.character,as.factor)


library(mice)
md.pattern(house)

impute<-mice(house, m=1, maxit=500, method='cart', seed=500)

new_house<-complete(impute,1)


library(h2o)
h2o.init()

new_house<-as.h2o(new_house)

splits<-h2o.splitFrame(data = new_house,ratios = c(0.7,0.15))

train<-splits[[1]]
valid<-splits[[2]]
test<-splits[[3]]


y<-"SalePrice"
x<-setdiff(names(new_house),"SalePrice")

aml<-h2o.automl(y=y,x=x,training_frame = train,validation_frame = valid,max_models = 10)

lb<-aml@leaderboard

model_id<-as.data.frame(lb$model_id)[,1]
best_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_id, value=TRUE)[1])


pred1<-h2o.predict(best_family,newdata = test)


check<-h2o.performance(best_family,newdata = test)



#testing on new dataset


house<- read.csv("~/Downloads/test (2).csv")
test2<- read.csv("~/Downloads/test (2).csv")


house<-house[,-1]
house<-house[,-6]

house$BsmtQual<-ifelse(is.na(house$BsmtQual),"NO",house$BsmtQual)
house$BsmtCond<-ifelse(is.na(house$BsmtCond),"NO",house$BsmtCond)
house$BsmtExposure<-ifelse(is.na(house$BsmtExposure),"NO",house$BsmtExposure)
house$BsmtFinType1<-ifelse(is.na(house$BsmtFinType1),"NO",house$BsmtFinType1)
house$BsmtFinType2<-ifelse(is.na(house$BsmtFinType2),"NO",house$BsmtFinType2)
house$FireplaceQu<-ifelse(is.na(house$FireplaceQu),"NO",house$FireplaceQu)
house$GarageType<-ifelse(is.na(house$GarageType),"NO",house$GarageType)
house$GarageFinish<-ifelse(is.na(house$GarageFinish),"NO",house$GarageFinish)
house$GarageQual<-ifelse(is.na(house$GarageQual),"NO",house$GarageQual)
house$GarageCond<-ifelse(is.na(house$GarageCond),"NO",house$GarageCond)
house$PoolQC<-ifelse(is.na(house$PoolQC),"NO",house$PoolQC)
house$Fence<-ifelse(is.na(house$Fence),"NO",house$Fence)
house$MiscFeature<-ifelse(is.na(house$MiscFeature),"NO",house$MiscFeature)

house<-house%>%mutate_if(is.character,as.factor)


house<-as.h2o(house)

pred2<-h2o.predict(best_family,newdata=house)

check2<-h2o.performance(best_family,newdata = house)


names(a)<-c("id","SalePrice")

my_submission<-a



write.csv(my_submission,file="submission.csv",row.names = FALSE)
