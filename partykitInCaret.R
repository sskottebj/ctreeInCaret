library(caret)
library(partykit)

ctreePartykit <- list(type = c("Classification", "Regression"),
                      library  = "partykit",
                      loop = NULL)

ctreePartykit$parameters <- data.frame(parameter = c("alpha"),
                                       class = c("numeric"),
                                       label = c("1 - P-Value Threshold"))

ctreePartykit$grid <- function(x, y, len = NULL, search = "grid") {
  if(search == "grid") {
    out <- data.frame(alpha = seq(from = 0.99, to = 0.01, length = len))
  } else {
    out <- data.frame(alpha = runif, min = 0,
                      max = 1)
  }
}

ctreePartykit$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  theDots <- list(...)
  ctl <- do.call(getFromNamespace("ctree_control", "partykit"),
                 list(alpha = param$alpha))
  if(!is.null(wts))
    theDots$weights <- wts
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = dat,
                      control = ctl), theDots)
  out <- do.call(partykit::ctree, modelArgs)
  out
}

ctreePartykit$predict <- function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  out <- predict(modelFit, newdata)
  out
}

ctreePartykit$prob = function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  out <- predict(modelFit, newdata, type = "prob")
  out
}

ctreePartykit$tags = NULL
ctreePartykit$levels = function(x) levels(x$data[, 1])
ctreePartykit$sort = function(x) x[order(-x$alpha), ]

# library(mlbench)
# data(Sonar)
# set.seed(998)
# 
# inTraining <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)
# training <- Sonar[inTraining, ]
# testing <- Sonar[-inTraining, ]
# 
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 5,
#                            repeats = 1,
#                            classProbs = TRUE,
#                            summaryFunction = mnLogLoss
#                            )
# #)
# 
# set.seed(123)
# mod <- train(Class ~ .,
#                        data = training,
#                        method = ctreePartykit,
#                        trControl = fitControl)
# set.seed(123)
# test <- train(Class ~ .,
#              data = training,
#              method = "ctree",
#              trControl = fitControl)
# mod
# test