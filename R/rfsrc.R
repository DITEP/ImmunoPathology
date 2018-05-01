######################################################
# few commands to run random forest with survival data
library(randomForestSRC)

# data format
# DATA: a data frame or matrice with covariables and outcome values in survival format (OS,STATUS)

# run the rf
Fitrf<- rfsrc(Surv(OS,STATUS) ~ ., data = DATA, 
               ntree = 5000, importance = T)

# check relative importance of the varibales
vimp(Fitrf)

# check error
Fitrf$err.rate[Fitrf$ntree]

# compute stats with fonctions from the caret package (quite long to install if you haven't), but you cann also do it manually
precision <- posPredValue(Fitrf$class.oob, Fitrf$yvar)
recall <- sensitivity(Fitrf$class.oob, Fitrf$yvar)
F1 <- (2 * precision * recall) / (precision + recall)
