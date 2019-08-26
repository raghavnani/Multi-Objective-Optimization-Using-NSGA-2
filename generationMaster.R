#'@description generate data and request responses from api to build dataframes and returns them to master layer and saves them in csv
#'@author: Felix
#'@param fun: function that needs to be approximated
#'@param endpoint: endpoint that will be used
#'@param batchSize: batchSize of observations to be requested from the API; default = 50
#'@param lopps: How many times should the function request?
#'@param base: base for Api request
#'@param dimensions: how many dimensions does the request have? 
#'@param sample: how should be sampled? randomly or intelligently?
#'@param functions: what functions should by sampled?
generateDataFrames = function(endpoint = "api-test2D", batchSize = 50, loops = 1, base, token, dimensions = 2, sample = "intelligent",
                              functions = "both"){
        outDf = data.frame()
        #initial random sample
        if(sample == "random") {
                
                for(loop in 1:loops){
                        
                        #generate input randomly based on dimensions
                        input = generateInput(batchSize, seed = sample(1:10000, size = 1), dimensions = dimensions)
                        print(input)
                        
                        #request data
                        if(dimensions == 3){
                                output = cbind(input,
                                               func1 = apirequest(input = input[,1:3], func = 1, endpoint = endpoint, base = base, token = token),
                                               func2 = apirequest(input = input[,1:3], func = 2, endpoint = endpoint, base = base, token = token))
                        }
                        
                        else{
                                output = cbind(input,
                                               func1 = apirequest(input = input[,1:2], func = 1, endpoint = endpoint, base = base, token = token),
                                               func2 = apirequest(input = input[,1:2], func = 2, endpoint = endpoint, base = base, token = token))
                        }
                        
                        #save data
                        outDf = rbind(outDf, output)
                        write.csv(dataframe, "export.csv", append = T, row.names = F)
                }
                
                return(outDf)
                
        }
        
        if(sample == "intelligent") {
                
                #generate initial sample. Uniform from space with random arrangement
                
                input = generateInput(batchSize, seed = sample(1:10000, size = 1), dimensions = dimensions)
                input$seed = NULL
                
                
                if(dimensions == 3){
                        
                        #request initial sample
                        output = cbind(input,
                                       func1 = apirequest(input = input[,1:3], func = 1, endpoint = endpoint, base = base, token = token),
                                       func2 = apirequest(input = input[,1:3], func = 2, endpoint = endpoint, base = base, token = token))
                        write.csv(output, "export.csv", append = T, row.names = F)
                        print("random initial sample looks like this:")
                        visualiseDatapoints(dataframe = output, dimensions = dimensions, mode = "all")
                        
                        print("proceeding with adaptive sampling")
                        for(i in 1:loops){
                                iteration = intelligentSample(output = output, endpoint = endpoint, token = token, base = base, dimensions = dimensions, functions ="both")
                                output = rbind(output, iteration)
                                write.csv(output, "export.csv", append = T, row.names = F)
                             
                        }
                }
                
                else{
                        #request initial sample
                        output = cbind(input,
                                       func1 = apirequest(input = input[,1:2], func = 1, endpoint = endpoint, base = base, token = token),
                                       func2 = apirequest(input = input[,1:2], func = 2, endpoint = endpoint, base = base, token = token))
                        print("random initial sample looks like this:")
                        write.csv(output, "export.csv", append = T, row.names = F)
                        visualiseDatapoints(dataframe = output, dimensions = dimensions, mode = "all")
                        
                        print("proceeding with adaptive sampling")
                        
                        for(i in 1:loops){
                                iteration = intelligentSample(output = output, endpoint = endpoint, token = token, base = base, dimensions = dimensions)
                                output = rbind(output, iteration)
                                write.csv(output, "export.csv", append = T, row.names = F)

                        }
                }
                return(output)
                
        }
        
        #after intial runs of random or intelligent samples, we can proceed with this type of sampling using the saves csv
        if(sample == "continue") {
                
                output = read.csv("export.csv")
                        
                for(i in 1:loops){
                        iteration = intelligentSample(output = output, endpoint = endpoint, token = token, base = base,
                                                      dimensions = dimensions, functions = functions)
                        output = rbind(output, iteration)
                        write.csv(output, "export.csv", append = T, row.names = F)
                }
                return(output)
        }
}

####only for use with adaptive sampling does not work####

#' @description generate hypercube input variables for api requests [-5:5]
#' @author: Felix
#' @param batchSize: Size of df to be generated
#' @param mode: how do are these samples generated
#' @param seed: random seed. Just needed for reproducibility
generateInput = function(batchSize = 50, seed, dimensions) {
        print(paste("generating random sample with seed" , seed))
        seed = seed
        set.seed(seed)
        
        if(dimensions == 3){
                x1 = sample(seq(-5, 5, by = 0.2))
                x2 = sample(seq(-5, 5, by = 0.2))
                x3 = sample(seq(-5, 5, by = 0.2))
                
                input = data.frame(x1 = x1[1:50], x2 = x2[1:50], x3 = x3[1:50], seed = seed)
        }
        else{
                x1 = sample(seq(-5, 5, by = 0.2))
                x2 = sample(seq(-5, 5, by = 0.2))
                
                input = data.frame(x1 = x1[1:50], x2 = x2[1:50], seed = seed)
        }
        
        return(input)
}


#' @description generate hypercube grid as basis for adaptive sampling  [-5:5]
#' @author: Niclas
#' @param stepSize: Size of the steps for the initial grid df to be generated
#' @param dimensions: how many dimensions?
#' @param lowerBound: from what number to start? 
#' @param higherBound: til what number?
generateGrid = function(lowerBound = -5, upperBound = 5, stepSize, dimensions) {
  
  if(dimensions == 3){
    x1 = seq(lowerBound, upperBound, by = stepSize)
    x2 = seq(lowerBound, upperBound, by = stepSize)
    x3 = seq(lowerBound, upperBound, by = stepSize)
    input = expand.grid(x1,x2,x3)
    colnames(input) <- c("x1","x2","x3")
    
    print(paste("generating initial grid of length" , nrow(input)))
    
  }
  else{
    x1 = seq(lowerBound, upperBound, by = stepSize)
    x2 = seq(lowerBound, upperBound, by = stepSize)
    input = expand.grid(x1,x2)
    colnames(input) <- c("x1","x2")
    
    print(paste("generating initial grid of length" , nrow(input)))
    
  }
  
  return(input)
}


#'@description generate initial grid with responses from api to enable efficient sampling
#'@author: Niclas
#'@param endpoint: endpoint that will be used
#'@param token: token for api request
#'@param dimensions: how many domensions? 
#'@param base: base for api 
generateInputGrid = function(endpoint, base, token, dimensions){
  outDf = data.frame()
  
  #fill dataframe with request and reponse data
  input = generateGrid(lowerBound = -5, upperBound = 5, stepSize = 2, dimensions)
  
  if(dimensions == 3){
    output = cbind(input,
                   func1 = apirequest(input = input[,1:3], func = 1, endpoint = endpoint, base = base, token = token),
                   func2 = apirequest(input = input[,1:3], func = 2, endpoint = endpoint, base = base, token = token))
  }
  
  else{
    output = cbind(input,
                   func1 = apirequest(input = input[,1:2], func = 1, endpoint = endpoint, base = base, token = token),
                   func2 = apirequest(input = input[,1:2], func = 2, endpoint = endpoint, base = base, token = token))
  }
  outDf = rbind(outDf, output)
  return(outDf)
}


#' @description takes output from an initial random sample and applies adaptive sampling 
#' @author: Felix
#' @param output: initial random sample to test on
#' rest is taken from higher level function
intelligentSample = function(output, endpoint, base, token, dimensions, functions = "both"){
        
        if(dimensions == 3) {
                if(functions == "both"){
                        
                        #fit function to dataframe for function1
                        f1 = lm(func1 ~ x1 + x2 + x3, data = output)
                        #calculate confidence intervals
                        conf = as.data.frame(predict(f1, newdata = output[,1:3], interval = "confidence"))
                        #select highest intervals, select random input point from the highest 25
                        output$high = abs(conf$lwr - conf$upr)
                        sel1 = dplyr::arrange(output, desc(high))[1:25,]
                        sel1 = sel1[sample(1:nrow(sel1), 1),]
                        #fit function to dataframe for function 2
                        f2 = lm(func2 ~ x1 + x2 + x3, data = output)
                        #calculate confidence intervals
                        conf = as.data.frame(predict(f2, newdata = output[,1:3], interval = "confidence"))
                        #select highest intervals, select random input point from the highest 25
                        output$high = abs(conf$lwr - conf$upr)
                        sel2 = dplyr::arrange(output, desc(high))[1:25,]
                        sel2 = sel2[sample(1:nrow(sel2), 1),]
                        input = as.data.frame(sapply(rbind(sel1[,1:3],sel2[,1:3]), jitter))   
                        #check for values out of bounds (-5,5) - even though Api allows for requests out of bounds? 
                        #check for duplicates
                        input = input[input$x1 <= 5 & input$x1 >= -5 & input$x2 <= 5 & input$x2 >= -5 & input$x3 <= 5 & input$x3 >= -5, ]
                        input[!input$x1 %in% output$x1 & !input$x2 %in% output$x2, ]
                        if(nrow(input) == 0){
                                return()
                        }
                        iteration = cbind(input,
                                          func1 = apirequest(input = input[,1:3], func = 1, endpoint = endpoint, base = base, token = token),
                                          func2 = apirequest(input = input[,1:3], func = 2, endpoint = endpoint, base = base, token = token))       
                        
                }
                if(functions == "func1"){
                        
                        #fit function to dataframe for function1
                        f1 = lm(func1 ~ x1 + x2 + x3, data = output)
                        #calculate confidence intervals
                        conf = as.data.frame(predict(f1, newdata = output[,1:3], interval = "confidence"))
                        #select highest intervals, select random input point from the highest 25
                        output$high = abs(conf$lwr - conf$upr)
                        sel1 = dplyr::arrange(output, desc(high))[1:25,]
                        sel1 = sel1[sample(1:nrow(sel1), 1),]
                        input = sel1[,1:3]
                        input$x1 = jitter(input$x1)
                        input$x2 = jitter(input$x2)
                        input$x3 = jitter(input$x3)
                        #check for values out of bounds (-5,5) - even though Api allows for requests out of bounds? 
                        #check for duplicates
                        input = input[input$x1 <= 5 & input$x1 >= -5 & input$x2 <= 5 & input$x2 >= -5 & input$x3 <= 5 & input$x3 >= -5, ]
                        input[!input$x1 %in% output$x1 & !input$x2 %in% output$x2, ]
                        if(nrow(input) == 0){
                                return()
                        }
                        iteration = cbind(input,
                                          func1 = apirequest(input = input[,1:3], func = 1, endpoint = endpoint, base = base, token = token),
                                          func2 = NA)
                        
                }
                if(functions == "func2"){
                        
                        #fit function to dataframe for function1
                        f1 = lm(func2 ~ x1 + x2 + x3, data = output)
                        #calculate confidence intervals
                        conf = as.data.frame(predict(f1, newdata = output[,1:3], interval = "confidence"))
                        #select highest intervals, select random input point from the highest 25
                        output$high = abs(conf$lwr - conf$upr)
                        sel1 = dplyr::arrange(output, desc(high))[1:25,]
                        sel1 = sel1[sample(1:nrow(sel1), 1),]
                        input = sel1[,1:3]
                        input$x1 = jitter(input$x1)
                        input$x2 = jitter(input$x2)
                        input$x3 = jitter(input$x3)                        #check for values out of bounds (-5,5) - even though Api allows for requests out of bounds? 
                        #check for duplicates
                        input = input[input$x1 <= 5 & input$x1 >= -5 & input$x2 <= 5 & input$x2 >= -5 & input$x3 <= 5 & input$x3 >= -5, ]
                        input[!input$x1 %in% output$x1 & !input$x2 %in% output$x2, ]
                        if(nrow(input) == 0){
                                return()
                        }
                        iteration = cbind(input,
                                          func1 = NA,
                                          func2 = apirequest(input = input[,1:3], func = 2, endpoint = endpoint, base = base, token = token))
                        
                }
        }
        
        else{
                #fit function to dataframe for function1
                f1 = lm(func1 ~ x1 + x2, data = output)
                #calculate confidence intervals
                conf = as.data.frame(predict(f1, newdata = output[,1:2], interval = "confidence"))
                #select highest intervals, select input points
                output$high = abs(conf$lwr - conf$upr)
                sel1 = dplyr::arrange(output, desc(high))[1:7,]
                sel1 = sel1[sample(1:nrow(sel1), 1),]
                #fit function to dataframe for function 2
                f2 = lm(func2 ~ x1 + x2, data = output)
                #calculate confidence intervals
                conf = as.data.frame(predict(f2, newdata = output[,1:2], interval = "confidence"))
                #select highest intervals, select input points
                output$high = abs(conf$lwr - conf$upr)
                sel2 = dplyr::arrange(output, desc(high))[1:7,]
                sel2 = sel2[sample(1:nrow(sel2), 1),]
                input = as.data.frame(sapply(rbind(sel1[,1:2],sel2[,1:2]), jitter))   
                #check for values out of bounds (-5,5) - even though Api allows for requests out of bounds?
                input = input[input$x1 <= 5 & input$x1 >= -5 & input$x2 <= 5 & input$x2 >= -5 & input$x3 <= 5 & input$x3 >= -5, ]
                input[!input$x1 %in% output$x1 & !input$x2 %in% output$x2, ]
                if(nrow(input) == 0){
                        return()
                }
                #request top 20 most varied predictions of both functions
                iteration = cbind(input,
                                  func1 = apirequest(input = input[,1:2], func = 1, endpoint = endpoint, base = base, token = token),
                                  func2 = apirequest(input = input[,1:2], func = 2, endpoint = endpoint, base = base, token = token))
        }
        
        return(iteration)
}

#'@description Generate train and test split on the basis of sampled dataframe
#'@author: Niclas
#'@param df: pass dataframe to be split
#'@param dimensions: how many dimensions does this df have?
split = function(df, dimensions){
  
  set.seed(1234)
  
  #Access and standardize data
  if(dimensions == 2){
    input1 = df[c("x1","x2","func1")]
    #input1 = data.frame(scale(input1)) # Standardizing the data
    
    input2 = df[c("x1","x2","func2")]
    #input2 = data.frame(scale(input2)) # Standardizing the data
  }
  
  if(dimensions == 3){
    input1 = df[c("x1","x2","x3","func1")]
    #input1 = data.frame(scale(input1)) # Standardizing the data
    
    input2 = df[c("x1","x2","x3","func2")]
    #input2 = data.frame(scale(input2)) # Standardizing the data
  }
  
  #Create train/test split for training algorithms
  ids1 = sample(1:nrow(input1), size=nrow(input1)*0.75, replace = FALSE)
  train1 = input1[ids1,]
  test1 = input1[-ids1,]
  
  ids2 = sample(1:nrow(input2), size=nrow(input2)*0.75, replace = FALSE)
  train2 = input2[ids2,]
  test2 = input2[-ids2,]
  
  return(list(train1,test1,train2,test2))
}

#' @description takes all models and evaluates them with respect to performance on the function 1 and 2
#' @author Felix/ Niclas
#' params: give models and which surrogate you want to evaluate
assessPerformance = function(ann, knn, svm, xgboost, rf, surrogate){
       
        performance = data.frame("Algorithms" = c("ANN", "XGBoost", "Random Forest", "Support Vector Machine", "k-Nearest Neighbors"),
                                 "MSE_func1" = c(ann[[3]],xgboost[[3]],rf[[3]],svm[[3]],knn[[3]]),
                                 "MSE_func2" = c(ann[[4]],xgboost[[4]],rf[[4]],svm[[4]],knn[[4]]),
                                 "RSQRT_func1" = c(ann[[5]],xgboost[[5]],rf[[5]],svm[[5]],knn[[5]]),
                                 "RSQRT_func2" = c(ann[[6]],xgboost[[6]],rf[[6]],svm[[6]],knn[[6]]))
        performance$Algorithms = as.character(performance$Algorithms)
        
        print(performance)
        
        if(surrogate == 1) {
                surrogate1 = performance[performance$MSE_func1 == min(performance$MSE_func1),1]
                if(surrogate1 == "XGBoost") {
                        return(xgboost[[1]])
                }       
                if(surrogate1 == "ANN") {
                        return(ann[[1]])
                }
                if(surrogate1 == "Random Forest") {
                        return(rf[[1]])
                }
                if(surrogate1 == "Support Vector Machine") {
                        return(svm[[1]])
                }
                if(surrogate1 == "k-Nearest Neighbors") {
                        return(knn[[1]])
                }   
        }
        if(surrogate == 2){
                surrogate2 = performance[performance$MSE_func2 == min(performance$MSE_func2),1]
                if(surrogate2 == "XGBoost") {
                        return(xgboost[[2]])
                }
                if(surrogate2 == "ANN") {
                        return(ann[[2]])
                }
                if(surrogate2 == "Random Forest") {
                        return(rf[[2]])
                }
                if(surrogate2 == "Support Vector Machine") {
                        return(svm[[2]])
                }
                if(surrogate2 == "k-Nearest Neighbors") {
                        return(knn[[2]])
                }    
        }

}
