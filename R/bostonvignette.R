install.packages("mlbench")
library(mlbench)
install.packages("caret")
library(caret)
BostonHousing <- MASS::Boston
#1
scale_BostonHousing <- scale(as.matrix(BostonHousing[, c(1:3,5:13)]))
scale_BostonHousing <- as.data.frame(scale_BostonHousing)
scale_BostonHousing$medv <- BostonHousing$medv
scale_BostonHousing$chas <- BostonHousing$chas
BostonHousing <- scale_BostonHousing
train <- caret::createDataPartition(y = BostonHousing$medv, p=0.8, list = FALSE)
training <- BostonHousing[train,]
test <- BostonHousing[-train, ]
###############################################
###########KOM IHÅG ATT SCALEA DATA############
###############################################

#2
fitlinear <- caret::train(medv~ . , 
                          data=training,
                          method="lm",
                          preProc = c("center","scale"))


fitlinear_forward <- caret::train(medv~.,
                                  data=training,
                                  method="leapForward",
                                  tuneGrid = data.frame(nvmax=1:(ncol(BostonHousing)-1)),
                                  preProc = c("center","scale"))

#3
fitlinear$results

fitlinear_forward$results[fitlinear_forward$bestTune[[1]],]

#4


Rid <- list(type = "Regression",
              library = "Lab4HugoOtto",
              loop = NULL)
prm <- data.frame(parameter = "lambda",
                  class =  "numeric" ,
                  label = "lambda")
Rid$parameters <- prm


pred <- function(modelFit, newdata , preProc = NULL, submodels = NULL){
  predict(modelFit, newdata)
}

Rid$predict <- pred

Rid$prob <- list(NULL)

Rid$sort <- function(x) x[order(-x$lambda),]

Rid$label <- "Ridge regression"

Rid$grid <- function(x, y, len=NULL, search="grid"){
  data.frame(lambda=seq(from= 0 , to = 5, by = 0.2))
}

Fit<-function(x,y,lambda,param,lev,last,classProbs,...){
  
  names <- lapply(x, function(x){
    all(x == y)
  })
  y_name <- names(x)[unlist(names)]
  x_names <- names(x)[!unlist(names)]
  
  formula <- paste(y_name,"~", sep="")
  
  
  for (xvar in 1:length(x_names)) {
    formula <- paste(formula, "+", x_names[xvar], sep="")
  }
  
  formula <- as.formula(formula)
  model <- Lab4HugoOtto::ridgereg( formula = formula, data=x,lambda= param$lambda)
  return(model)
}

Rid$fit <- Fit

RidFit <- caret::train( y = training$medv,
                          x = training,
                          method = Rid
)
RidFit
RidFit$results[RidFit$results$lambda==RidFit$bestTune$lambda, ]


# 5

control <- trainControl(
  method = "repeatedcv",
  number = 10
)

RidFit_control <- caret::train( y = training$medv,
                        x = training,
                        method = Rid,
                        trControl = control
)
RidFit_control
RidFit_control$results[RidFit_control$results$lambda==RidFit_control$bestTune$lambda, ]
# 6

RidFit_predict <- predict(RidFit_control, test)
RidFit_test <- test$medv - RidFit_predict
ESS_ridfit <- sum((RidFit_predict-mean(test$medv))^2)

R_2_ridfit <- 1- (sum(RidFit_test^2)/ESS_ridfit)

fitlinear_predict <- predict(fitlinear, test)
fitlinear_test <- test$medv - fitlinear_predict
ESS_fitlinear <- sum((fitlinear_predict-mean(test$medv))^2)

R_2_fitlinear <- 1- (sum(fitlinear_test^2)/ESS_fitlinear)


fitlinear_forward_predict <- predict(fitlinear_forward, test)
fitlinear_forward_test <- test$medv - fitlinear_forward_predict
ESS_forward <- sum((fitlinear_forward_predict-mean(test$medv))^2)

R_2_forward <- 1- (sum(fitlinear_forward_test^2)/ESS_forward)
#Forward is the best!





