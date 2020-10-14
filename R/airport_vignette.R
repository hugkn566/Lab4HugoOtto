#1.2.1
#1
weather <- nycflights13::weather
flights <- nycflights13::flights

flights <- flights[,c("year","month", "day", "hour", "sched_dep_time", "dep_delay", "origin", "distance")]
weather <- weather[,c("origin","year","month","day","hour","temp","humid","wind_speed","precip", "visib")]

#2

data <- dplyr::inner_join(flights, weather, by= c("year", "month", "day", "hour", "origin"))
data <- data[complete.cases(data), ]
data <- data[, 5:13]
data_scale <- scale(as.matrix(data[,c(1,4:9)]))
data_scale <- as.data.frame(data_scale)
data_scale$dep_delay <- data$dep_delay
data_scale$origin <- data$origin
data <- data_scale
#3
train <- caret::createDataPartition(y = data$dep_delay, p=0.8, list = FALSE)
training <- data[train,]
training <- as.data.frame(training)
ny_data <- data[-train,]
train2 <- caret::createDataPartition(y = ny_data$dep_delay, p=0.75, list = FALSE)
vali <- ny_data[train2, ]
test <- ny_data[-train2, ]

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
  data.frame(lambda=seq(from= 1 , to = 2, by = 1))
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

RidFit_delay <- caret::train( y = training$dep_delay,
                        x = training,
                        method = Rid
)



#4

rmse <- function(error){
  sqrt(mean(error^2))
}

RidFit_delay_predict <- predict(RidFit_delay, vali)
error <- vali$dep_delay - RidFit_delay_predict
vali_error <- rmse(error)
vali_error


#5

RidFit_delay_predict_test <- predict(RidFit_delay, test)
error_test <- test$dep_delay - RidFit_delay_predict_test
test_error <- rmse(error = error_test
                   )
test_error
