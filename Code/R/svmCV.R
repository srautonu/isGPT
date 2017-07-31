
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
    
    #acc = max(unlist(ROCR::performance(svmprediction,"acc")@y.values), na.rm = TRUE)
    #F1 = max(unlist(ROCR::performance(svmprediction,"f")@y.values), na.rm = TRUE)
    #prec = max(unlist(ROCR::performance(svmprediction,"prec")@y.values), na.rm = TRUE)
    #recall = max(unlist(ROCR::performance(svmprediction,"rec")@y.values), na.rm = TRUE)
    #sensitiviy = max(unlist(ROCR::performance(svmprediction,"sens")@y.values), na.rm = TRUE)
    #specificity = max(unlist(ROCR::performance(svmprediction,"spec")@y.values), na.rm = TRUE)
    #mccv = max(unlist(ROCR::performance(svmprediction,"mat")@y.values), na.rm = TRUE)
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
    f1 = unlist(ROCR::performance(svmprediction,"f")@y.values)[2]
    prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)[2]
    recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)[2]
    sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
    mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
    return(list(
      "acc" = acc,
      "f1" = f1,
      "prec" = prec,
      "rec" = recall,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mccv
      ))
  }