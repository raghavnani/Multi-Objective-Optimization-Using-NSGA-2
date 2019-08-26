#' @description set up of function for further processing
#' @author Felix
#' @param name: name of the algo
#' @param model: modelObject
prepareFunction = function(name, model) {
  
  fn = smoof::makeMultiObjectiveFunction(name = name,
                                            fn = model,
                                            #insert parametersets that can be optimised
                                            par.set = getOptimSpace(name))
  
  lower = getLowerBoxConstraints(fn)
  upper = getUpperBoxConstraints(fn)
  
  control = initECRControl(fn)
  control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
  control = registerECROperator(control, "selectForSurvival", selGreedy)
  
  return(list(fn, control))
  
}

#' @description define optimisation space for model at hand
#' @author Felix
#' @param name: name of the algorithm
getOptimSpace = function(name) {
  
  if(name == "KNN") {
    
  }
  if(name == "ANN") {
    
  }
  if(name == "XGB") {
    
  }
  if(name == "SVM") {
    
  }
  if(name == "RF") {
    
  }
  
  return(set)
}


#' @description do evolution of model and plot progress
#' @author Felix
evolution = function(fn, MU, control, MAX.ITER, LAMBDA){
 
  lower = getLowerBoxConstraints(fn)
  upper = getUpperBoxConstraints(fn)
  
  population = genReal(MU, getNumberOfParameters(fn), lower, upper)
  fitness = evaluateFitness(control, population)
  
  for (i in seq_len(MAX.ITER)) {
    # animate the loop
    pl = autoplot(fn, length.out = 1000L)
    df = data.frame(x = unlist(population), y = as.numeric(fitness))
    pl = pl + geom_point(data = df, mapping = aes(x = x, y = y))
    print(pl)
    Sys.sleep(0.2)
    # sample lambda individuals at random
    idx = sample(1:MU, LAMBDA)
    # generate offspring by mutation and evaluate their fitness
    offspring = mutate(control, population[idx], p.mut = 1)
    fitness.o = evaluateFitness(control, offspring)
    # now select the best out of the union of population and offspring
    sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
    population = sel$population
    fitness = sel$fitness
  }
   
}
