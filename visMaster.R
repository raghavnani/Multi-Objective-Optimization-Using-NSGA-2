#' @description takes care of the visualisations after the generation of th dataframe 
#' @author Felix
#' @param dataframe: dataframe generated and sent by the API
#' @param dimensions: dimensions of the features
#' @param mode: "all", "func1" or "func2" points to be visualised?
visualiseDatapoints = function(dataframe, dimensions, mode = "all"){
  
  if(dimensions == 2){
    
    if(mode == "all") {
      
      compDataframe1 = dataframe[, -4]
      colnames(compDataframe1)[3] = "target"
      compDataframe1$group = 1
      compDataframe2 = dataframe[, -3]
      colnames(compDataframe2)[3] = "target"
      compDataframe2$group = 2
      compDataframe = rbind(compDataframe1, compDataframe2)
      
      scatter3D(x = compDataframe$x1, y = compDataframe$target, z= compDataframe$x2, phi = 0, theta = 100,
                colvar = compDataframe$group,
                col = c("#7570B3", "#D95F02"), main = "Datapoints",
                ylab = "Target", xlab = "X1", zlab = "X2")
      
    }
    
    if(mode == "func1") {
                #add colour here to for data ranges
      scatter3D(x = dataframe$x1, y = dataframe$func1, z= dataframe$x2, phi = 0, theta = 100,
                main = "Datapoints",
                ylab = "Function 1", xlab = "X1", zlab = "X2")
      
    }
    
    if(mode == "func2") {
      
      scatter3D(x = dataframe$x1, y = dataframe$func2, z= dataframe$x2, phi = 0, theta = 100,
                main = "Datapoints",
                ylab = "Function 2", xlab = "X1", zlab = "X2")
      
    }
    
  }
  
  if(dimensions == 3) {
    
    if(mode == "all") {
      
      compDataframe1 = dataframe[, -5]
      colnames(compDataframe1)[4] = "target"
      compDataframe1$group = 1
      compDataframe2 = dataframe[, -4]
      colnames(compDataframe2)[4] = "target"
      compDataframe2$group = 2
      compDataframe = rbind(compDataframe1, compDataframe2)
      
      scatter3D(x = compDataframe$x1, y = compDataframe$x2, z= compDataframe$x3, phi = 0, theta = 100,
                colvar = compDataframe$target, main = "Datapoints",
                ylab = "X2", xlab = "X1", zlab = "X3")
      
    }
    
    if(mode == "func1") {
            #add colours here for 3D measurements
      
      scatter3D(x = dataframe$x1, y = dataframe$x2, z= dataframe$x3, phi = 0, theta = 100,
                colvar = dataframe$func1, main = "Datapoints",
                ylab = "X2", xlab = "X1", zlab = "X3")
      
    }
    
    if(mode == "func2") {
      
      scatter3D(x = dataframe$x1, y = dataframe$x2, z= dataframe$x3, phi = 0, theta = 100,
                colvar = dataframe$func2, main = "Datapoints",
                ylab = "X2", xlab = "X1", zlab = "X3")
      
    }
    
  }
  
  plotrgl()
  
}