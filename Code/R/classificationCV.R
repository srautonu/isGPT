source('base.R')
source('svmCV.R')

# jackknife
#nFolds = length(features[,1])

# 10 fold CV
nFolds = 10

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

svmCostList = c(0.01, 0.1, 1, 10, 100);
featureCountList = seq(from=1500, to=2800, by=50); 

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

pseAAC = read.csv("pseAAC.csv");
features = cbind(pseAAC, features);

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features = featurefiltering(features, rankedFeatures, max(featureCountList));


for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  trainingSet = cbind(pseAAC, trainingSet);

  for (svmC in svmCostList) 
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
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
