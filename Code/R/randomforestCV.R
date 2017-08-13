randomForestCV <-
  function(formula, data, rfImportance) {
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
      predVector = c(predVector, as.numeric(svmpred))
      i = i + 1
    }
    prediction = prediction(as.numeric(predVector), as.numeric(data$protection))  
    
    acc = unlist(ROCR::performance(prediction,"acc")@y.values)[2]
    sensitivity = unlist(ROCR::performance(prediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(prediction,"spec")@y.values)[2];
    mcc = unlist(ROCR::performance(prediction,"mat")@y.values)[2];
    
    return(list(
      "model" = model,
      "acc" = acc,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mcc
      ))
  }