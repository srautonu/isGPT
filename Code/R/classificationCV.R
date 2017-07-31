source('base.R')
source('svmCV.R')

# jackknife
nFolds = length(features[,1])

# 10 fold CV
#nFolds = 10

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

maxFeatureCount = 2150
#for (maxFeatureCount in seq(from=2000, to=2500, by=10)) 
{
  filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;

  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  svmC = 3;
  #for (svmC in svmCostList) 
  {
    perf = svmCV(protection ~ ., trainingSet, svmCost = svmC, cross = nFolds);

    cat(maxFeatureCount, ",", svmC, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf$acc, perf$sens, perf$spec, perf$mcc));
    write.csv(accData, outFile);
    
    if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
      bestPerf = perf;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
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
