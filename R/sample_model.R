sales.sample <- sales[sales$STORE_NUM == 351, ]

temperature <- sales.sample$max_temp_amt
sale <- sales.sample$BRAKES_QTY
store <- sales.sample$STORE_NUM

sample <- data.frame(temp=temperature, s=sale)

n <- nrow(sample)
k <- floor(n*3/4)
train <- sample[1:k, ]
test <- sample[(k+1):n, ]

train <- train[order(train$temp), ]
test <- test[order(test$temp), ]


library(caret)
set.seed(1)
lm1Fit <- train(s ~ temp, 
                data = train,
                method = "lm", 
                trControl = trainControl(method= "cv"))
lm1Fit


## Create squared terms
train$ED2 <- train$temp^2
test$ED2 <- test$temp^2

set.seed(1)
lm2Fit <- train(s ~ temp + ED2, 
                data = train, 
                method = "lm", 
                trControl = trainControl(method= "cv"))
lm2Fit


## Finally a MARS model (via the earth package)

library(earth)
set.seed(1)
marsFit <- train(s ~ temp, 
                 data = train, 
                 method = "earth",
                 tuneLength = 15,
                 trControl = trainControl(method= "cv"))
marsFit

plot(marsFit)



test$lm1  <- predict(lm1Fit,test)  
test$lm2  <- predict(lm2Fit,test)
test$mars <- predict(marsFit, test)

postResample(pred = test$lm1,  obs = test$s)
postResample(pred = test$lm2,  obs = test$s)
postResample(pred = test$mars,  obs = test$s)
