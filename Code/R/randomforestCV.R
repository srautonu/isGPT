randomForestCV <-
  function(formula, data, rfImportance, cross) {
    N = length(data[, 1])
    folds = seq(from=1,to=N, by=round(N/cross))
    folds[cross+1] = N+1
    predVector = c()
    for (i in 1:cross) {
      trainFolds = data
      testFold = data[(folds[i]:(folds[i+1]-1)),]
      trainFolds = data[-(folds[i]:(folds[i+1]-1)),]
      
      model = randomForest(formula, trainFolds, importance=TRUE);
      
      pred = predict(model, testFold)
      predVector = c(predVector, as.numeric(pred))
      i = i + 1
    }
    
    predAndTruth = prediction(as.numeric(predVector), as.numeric(data$protection))
    acc = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2]
    sensitivity = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
    mcc = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];
    
    model = randomForest(formula, data, importance=TRUE);
    return(list(
      "model" = model,
      "acc" = acc,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mcc
      ))
  }