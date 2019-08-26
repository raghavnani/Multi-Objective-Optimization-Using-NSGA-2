#' @description performs non dominated sort and crowding distance to determine best performing pairs
#' @author Niclas
#' @param dataframe: containing data to be sorted
#' @param dimensions: how many dimensions does this have? 
maxCrowdingDistance = function(dataframe, dimensions){
        
        #Source python script for optimization
        use_python("/usr/local/bin/python")
        source_python('nsga.py')
        
        #Execute python function to identify points that maximize crowding distance
        indices = exec(dataframe$func1, dataframe$func2)
        indices = c(unlist(indices))

        #Apply 1 to every index - In Python indexing starts from 0 but in R it starts from 1
        indices = sapply(indices , function(x){
                x = x+1
        })
        
        if(dimensions == 2){
                output = dataframe[indices,]
        }
        
        if(dimensions == 3){
                output = dataframe[indices,]
        }
        
        return(output)
}


#' @description perform mutation on input vectors
#' @author Raghav
#' @param output: dataframe that needs to be mutated
mutation = function(output){
        r = rnorm(n = nrow(output),mean = 0,sd = 1)
        
        
        for (i in 1:length(r)) {
                if(r[i] > 0.6 ){
                        while(1){
                                new  = output[i,c('x1')] + rnorm(n = 1,mean = 0,sd = 0.5)
                                if(new <=5 & new >=-5){
                                        output[i,c('x1')] = new
                                        if(dim(output)[2] == 3){
                                        output[i,c('x3')] = new   
                                        }
                                        break;
                                }
                                
                        }      
                        
                }else if (r[i] < -0.4){
                        
                        while(1){
                                new  = output[i,c('x2')] + rnorm(n = 1,mean = 0,sd = 0.5)
                                if(new <=5 & new >=-5){
                                        output[i,c('x2')] = new
                                        break;
                                }
                                
                        }
                        
                }else{
                        
                        
                        while(1){
                                new1  = output[i,c('x1')] + rnorm(n = 1,mean = 0,sd = 0.5)
                                new2  = output[i,c('x2')] + rnorm(n = 1,mean = 0,sd = 0.5)
                                if(dim(output)[2]==3){
                                        new3  = output[i,c('x3')] + rnorm(n = 1,mean = 0,sd = 0.5)
                                        
                                }
                                
                                if(new1 <=5 & new1 >=-5){
                                        output[i,c('x1')] = new1
                                        break;
                                }
                                if(new2 <=5 & new2 >=-5){
                                        output[i,c('x2')] = new2
                                        break;
                                }
                                if(dim(output)[2]==3){
                                        if(new3 <=5 & new3 >=-5){
                                        output[i,c('x2')] = new2
                                        break;
                                        }
                                }
                                
                        }

                        
                }
        }
        
        return(output)
}


#' @description calculate rsquared metric for evaluation
#' @author Raghav
#' @param pred: prediction of the model
#' @param actual: true target of the data
rsq = function(pred , actual){
        rss <- sum((pred - actual) ^ 2)  ## residual sum of squares
        tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
        rsq <- 1 - rss/tss
        return(rsq)
}

