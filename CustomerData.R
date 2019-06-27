rm(list = ls(all=TRUE)) #remove all
setwd("D:/AI/ML/multipleregressionactivitysheet") # set working directory
CustomerData=read.csv("CustomerData.csv") # read file
dim(CustomerData) #dimensions of data
str(CustomerData) # structure of data
summary(CustomerData) #summary of data

CustomerData$CustomerID=NULL # remove/drop a cell

CustomerData$City=as.factor(CustomerData$City) #convert int to factor

set.seed(007) # We can control the randomness of the sampling for future reproducibility by using the "set.seed()" function
train_rows <- sample(x = 1:nrow(CustomerData), size = 0.75*nrow(CustomerData)) # The "sample()" function helps us randomly sample the row numbers. The x parameter takes a vector as an input (here the vector is a sequence of numbers from 1 to the number of rows in dataset). The size parameter takes the number of elements to be randomly sampled
train_data <- CustomerData[train_rows, ] # We use the following indices to subset the train and test sets from the data
test_data<-CustomerData[-train_rows,]
model1 <- lm(formula = TotalRevenueGenerated  ~. , data = train_data) 
summary(model1)

model2<-lm(formula=TotalRevenueGenerated  ~ City+MinAgeOfChild+Tenure+FrquncyOfPurchase+NoOfUnitsPurchased+ 
           FrequencyOFPlay+NoOfGamesBought+FavoriteChannelOfTransaction+FavoriteGame, data=train_data)
summary(model2)

par(mfrow=c(2,2))
plot(model2)

library(DMwR)
preds_model <- predict(model2, test_data[, !(names(test_data) %in% c("TotalRevenueGenerated"))])
preds_model <- predict(model2, test_data[ #mse=2186 rmse=46.7
preds_model2 <- predict(model2, train_data[, !(names(train_data) %in% c("TotalRevenueGenerated"))])
regr.eval(train_data$TotalRevenueGenerated, preds_model) #mse=11631 rmse=107


library(MASS)
stepAIC(model1, direction = "both")
model3=lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
           Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
           NoOfGamesBought + FavoriteChannelOfTransaction + FavoriteGame, 
         data = train_data)

library(car) 
vif(model3) 
model4=lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
            Tenure +  FrequencyOFPlay +  FavoriteChannelOfTransaction + FavoriteGame, 
          data = train_data)
vif(model4)
preds<- predict(model4, test_data[, !(names(test_data) %in% c("TotalRevenueGenerated"))])
regr.eval(test_data$TotalRevenueGenerated, preds) #mse=6955 rmse=83.3
regr.eval(train_data$TotalRevenueGenerated, preds) #mse=7067 rmse=84


