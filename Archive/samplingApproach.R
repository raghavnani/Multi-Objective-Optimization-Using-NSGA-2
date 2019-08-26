#Setting working directory
setwd("~/Documents/Studium/Masterstudium/Semester 1/Data Analytics 2/MultiObjOptimisation")

source("sourcer.R")
library (mlr)
library(tgp)
library(reticulate)

# source_python("test.py")

#Receive dataframe from API
#dataframe = generateInputGrid(endpoint = endpoint, base = base, token = token, dimensions = dimensions)
#write.csv(dataframe)

#Create trained model
task <- makeRegrTask(data=dataframe[1:3], target="func1")
learner = makeLearner(predict.type = "se", cl = "regr.bgp")
model <- train(learner = learner, task = task)

#Create a grid
seqx1 = seq(-5,5,by=0.1)
seqx2 = seq(-5,5,by=0.1)
pgrid = expand.grid(seqx1,seqx2)

#Predict whole grid
colnames(pgrid) <- c("x1","x2")
                      
pred <- predict(model, newdata = pgrid)
head(pred$data)
tail(pred$data)
result = cbind(pgrid,pred$data)
colnames(result) <- c("x1","x2","response","se")
scatter3D(x=result$x1, y=result$se, z=result$x2, phi = 0, theta = 100,
       main = "Datapoints", ylab = "Function 1", xlab = "X1", zlab = "X2")

### WORK IN PROGRESS ###

#Identify points with lowest prediction accuracy/highest confidence interval

#Retrieve them from the API and add them to the test data
