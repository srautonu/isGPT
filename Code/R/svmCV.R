
svmCV <-
  function(formula, data, svmCost, cross, svmScale = TRUE) {
    N = length(data[, 1])
    folds = seq(from=1,to=N, by=round(N/cross))
    folds[cross+1] = N+1
    predVector = c()
    for (i in 1:cross) {
      trainFolds = data
      testFold = data[(folds[i]:(folds[i+1]-1)),]
      trainFolds = data[-(folds[i]:(folds[i+1]-1)),]
      svmmodel = svm(formula, trainFolds, kernel = "linear", cost = svmCost, scale = svmScale)
      
      svmpred = predict(svmmodel, testFold)
      predVector = c(predVector, as.numeric(svmpred))
      i = i + 1
    }
    svmprediction = prediction(as.numeric(predVector), as.numeric(data$protection))  
    
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
    sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
    mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
    return(list(
      "acc" = acc,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mccv
      ))
  }