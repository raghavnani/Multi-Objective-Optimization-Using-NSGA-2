XGBoostModel = function(train1, train2, test1, test2){
        
        #Create XGBoost learner
        train_task1 = makeRegrTask(id = 'train', data = train1[!is.na(train1$func1),], target = 'func1')
        train_task2 = makeRegrTask(id = 'train', data = train2, target = 'func2')
        
        xgb_learner = makeLearner(cl = 'regr.xgboost')
        
        #Creating 5-fold Cross Validation
        rdesc = makeResampleDesc("CV", iters = 5L)
        
        #Creating a parameter set for the XGBoost learner
        xgb_params <- makeParamSet(
                # The number of trees in the model (each one built sequentially)
                makeIntegerParam("nrounds", lower = 100, upper = 500),
                # number of splits in each tree
                makeIntegerParam("max_depth", lower = 1, upper = 10),
                # "shrinkage" - prevents overfitting
                makeNumericParam("eta", lower = 0.1, upper = 0.5)
        )
        
        #Define how to search through the parameter set
        ctrl = makeTuneControlRandom(maxit=100)
        #ctrl = makeTuneControlGrid()
        
        #Fine Tuning the Models for both functions based on param set
        res1 = tuneParams("regr.xgboost", task = train_task1, resampling = rdesc,
                          par.set = xgb_params, control = ctrl)
        res2 = tuneParams("regr.xgboost", task = train_task2, resampling = rdesc,
                          par.set = xgb_params, control = ctrl)
        
        #Setting the parameter for XGBooster which is selected as best from fine tuning
        lrn1 = setHyperPars(makeLearner("regr.xgboost"), par.vals = res1$x)
        lrn2 = setHyperPars(makeLearner("regr.xgboost"), par.vals = res2$x)
        
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