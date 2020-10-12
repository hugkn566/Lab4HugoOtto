#1.2.1
#1
weather <- nycflights13::weather
flights <- nycflights13::flights

flights <- flights[,c("year","month", "day", "hour", "sched_dep_time", "dep_delay", "origin", "distance")]
weather <- weather[,c("origin","year","month","day","hour","temp","humid","wind_speed","precip", "visib")]

#2

data <- dplyr::inner_join(flights, weather, by= c("year", "month", "day", "hour", "origin"))
data <- data[complete.cases(data), ]

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
  data.frame(lambda=seq(from= 0 , to = 1, by = 0.5))
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

RidFit_delay <- caret::train( y = training$dep_delay[1:10000],
                        x = training[1:10000,-c(1,2)],
                        method = Rid
)
#Take away Year and month!
