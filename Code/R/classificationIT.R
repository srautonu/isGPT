source('base.R')

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

svmCostList = c(0.1, 1, 10, 100);
featureCountList = seq(from=2850, to=5000, by=50); 

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);
  
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, cost = svmC, scale = TRUE);
    svmpred = predict(svmmodel, testSet);
    svmprediction = prediction(as.numeric(svmpred), as.numeric(testSet$protection));
    
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
    f1 = unlist(ROCR::performance(svmprediction,"f")@y.values)[2]
    prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)[2]
    recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)[2]
    sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
    mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
    perf = list(
      "acc" = acc,
      "f1" = f1,
      "prec" = prec,
      "rec" = recall,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mccv
    )
    cat(maxFeatureCount, ",", svmC, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf$acc, perf$sens, perf$spec, perf$mcc));
    write.csv(accData, outFile);
    
    if (!is.nan(perf$mcc)) {
      if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
        bestPerf = perf;
        bestParams = list(
          "maxFeatureCount" = maxFeatureCount,
          "svmC" = svmC
        )
        cat(",<-- BEST");
      }
    }
    
    cat("\n");
  }
}

cat("Best Result for <nF, C> = ", bestParams$maxFeatureCount, bestParams$svmC, "\n");
cat("Accuracy(Test set): ", bestPerf$acc, "\n");
cat("F1-Score (Test set): ", bestPerf$f1, "\n");
cat("Precision(Test set): ", bestPerf$prec, "\n");
cat("Recall   (Test set): ", bestPerf$rec, "\n");
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
