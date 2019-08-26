###Master###
setwd("C:/Users/felix/Google Drive/Master/WWU/Data Analytics 2/MultObjOptimisation")
source("sourcer.R")
base="optim.uni-muenster.de:5000/"
token="866de98d0d47426e92cc0e3394df5f07"
batchSize=50
endpoint = "api"

dimensions = 3
if(dimensions == 3){
        endpoint = "api-test3D"
}
if(dimensions == 2) {
        endpoint = "api-test2D"
}

#import data and build dataframe for observations and the two target variables. 
#dataframe = generateDataFrames(endpoint = endpoint, batchSize = batchSize, loops = 8, base = base, 
 #                              token = token, dimensions = dimensions, sample = "random", functions = "func2")

dataframe = read.csv("export.csv")
#visualise these datapoints in a 3D explorable space. 
visualiseDatapoints(dataframe = dataframe, dimensions = dimensions, mode = "func2")

#Generate train-test split for model training and tuning
split = split(df = dataframe, dimensions = dimensions)
train1 = as.data.frame(split[1])
test1 = as.data.frame(split[2])
train2 = as.data.frame(split[3])
test2 = as.data.frame(split[4])

#Train surrogate models for both functions including hyperparameter tuning and receive predictions on the test set
#install_tensorflow(version = "1.12")
ann = kerasModel(train1, train2, test1, test2, dimensions = dimensions)
xgboost = XGBoostModel(train1, train2, test1, test2)
rf = rfModel(train1, train2, test1, test2)
svm = svmModel(train1, train2, test1, test2)
knn = knnModel(train1, train2, test1, test2)

#Create data frame for comparing the different models
surrogate1 = assessPerformance(ann, knn, svm, xgboost, rf, surrogate = 1)
surrogate2 = assessPerformance(ann, knn, svm, xgboost, rf, surrogate = 2)

#Use best-performing surrogate models to predict the whole hypercube
if(dimensions == 3){
        grid = generateGrid(stepSize = 0.5, dimensions = dimensions)
}
if(dimensions == 2) {
        grid = generateGrid(stepSize = 0.1, dimensions = dimensions)
}

surrogate1Pred = predict(surrogate1, x = as.matrix(grid))
surrogate2Pred = predict(surrogate2, newdata = grid)

grid$func1 <- surrogate1Pred$data$response
grid$func1 <- surrogate1Pred
grid$func2 <- surrogate2Pred$data$response

#Visualize both surrogate models
visualiseDatapoints(dataframe = grid, dimensions = dimensions, mode = "func1")
visualiseDatapoints(dataframe = grid, dimensions = dimensions, mode = "func2")


output = maxCrowdingDistance(dataframe = dataframe, dimensions = dimensions)
smallest = data.frame(x1 = c(), x2 = c(), x3 = c(), y1 = c(), y2 = c())

#perform evolution for multi-objective optimisation
for(gen in 1:400){
       
        
        #Perform multi-object Optimization to receive points to maximize the crowding distance in the grid
        df = mutation(output = output[,1:3])
        dfNew = df[1:20,]
        
        #Get the pareto front as predicted by our models
        #pred_func1 = predict(surrogate1, newdata = dfNew)
        pred_func1 = predict(surrogate1, x = as.matrix(dfNew))
        pred_func2 = predict(surrogate2, newdata = dfNew)
        
        #dfNew$func1 <- pred_func1$data$response
        dfNew$func1 <- c(pred_func1)
        dfNew$func2 <- pred_func2$data$response
        
        output = maxCrowdingDistance(dataframe = dfNew, dimensions = dimensions)
        
        #save best individual of each iteration
        smallest = rbind(smallest, output[1,] )
        print(head(output, n = 3))

        
}

plot(x = smallest$func1, y = smallest$func2)
maxCrowdingDistance(dataframe = smallest, dimensions = dimensions)


