knnModel = function(train1, train2, test1, test2){
        
        #Create learner
        train_task1 = makeRegrTask(id = 'train', data = train1[!is.na(train1$func1),], target = 'func1')
        train_task2 = makeRegrTask(id = 'train', data = train2, target = 'func2')
        
        knn_learner = makeLearner(cl = 'regr.kknn')
        
        #Creating 5-fold Cross Validation
        rdesc = makeResampleDesc("CV", iters = 5L)
        
        #Creating a parameter set for the learner
        knn_params <- makeParamSet(
                makeDiscreteParam("k", values = c(4,5,6,7,8,9,10)),
                makeDiscreteParam("distance", values = c(1,2,3,4))
        )
        
        #Define how to search through the parameter set
        #ctrl = makeTuneControlRandom(maxit=5)
        ctrl = makeTuneControlGrid()
        
        #Fine Tuning the Models for both functions based on param set
        res1 = tuneParams("regr.kknn", task = train_task1, resampling = rdesc,
                          par.set = knn_params, control = ctrl)
        res2 = tuneParams("regr.kknn", task = train_task2, resampling = rdesc,
                          par.set = knn_params, control = ctrl)
        
        #Setting the parameters which are selected as best from fine tuning
        lrn1 = setHyperPars(makeLearner("regr.kknn"), par.vals = res1$x)
        lrn2 = setHyperPars(makeLearner("regr.kknn"), par.vals = res2$x)
        
        #Retrain models based on optimal hyperparamsets
        mod1 = mlr::train(learner = lrn1, task = train_task1)
        mod2 = mlr::train(learner = lrn2, task = train_task2)
        
        #Predict the targets in the test data
        pred1 = predict(mod1, newdata = test1[!is.na(test1$func1),])
        perf1 = performance(pred=pred1, measures=list(mse))
        pred2 = predict(mod2, newdata = test2)
        perf2 = performance(pred=pred2, measures=list(mse))
        
        rsqrt1 = rsq(pred1$data$response, pred1$data$truth) 
        rsqrt2 = rsq(pred2$data$response, pred2$data$truth)
        
        #Creating one dataframe with predictions for both functions
        #new_pred1 = pred1$data$response
        #new_pred2 = pred2$data$response
        
        #df = data.frame(new_pred1, new_pred2)
        
        return(list(mod1, mod2, perf1, perf2, rsqrt1, rsqrt2))
}