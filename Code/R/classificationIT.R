source('base.R')

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

svmCostList = c(0.01, 0.1, 1, 10, 100);
featureCountList = seq(from=1500, to=2800, by=50);

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features     = featurefiltering(features, rankedFeatures, max(featureCountList));
testFeatures = featurefiltering(testFeatures, rankedFeatures, max(featureCountList));

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);
  
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, scale = TRUE);
    svmpred = predict(svmmodel, testSet);
    svmprediction = prediction(as.numeric(svmpred), as.numeric(testSet$protection));
    
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
    sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
    mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
    perf = list(
      "acc" = acc,
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
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
