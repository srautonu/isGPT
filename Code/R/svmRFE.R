library(e1071)
library(ROCR)

source('featurefiltering.R');

timestamp();

set.seed(10);

rankedFeatures = readRDS("newRank.rds");
features       = readRDS("featurized_comb_pseAAC_SMOTED.rds")

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

featureCountList = seq(from=1500, to=2800, by=50);

cat(as.character(Sys.time()),">> Entering SVM-RFE ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
trainingSet = features[,rankedFeatures];
trainingSet$protection = features$protection;

while (length(trainingSet[1,]) > 1)
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  svmmodel = svm(protection ~ ., trainingSet, cost = 1, kernel = "linear", cross = 10, scale = TRUE);

    
  accData = rbind(accData, c(maxFeatureCount, svmC, perf$acc, perf$sens, perf$spec, perf$mcc));
  write.csv(accData, outFile);
    
  # if (!is.nan(perf$mcc)) {
  #   if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
  #     bestPerf = perf;
  #     bestParams = list(
  #       "maxFeatureCount" = maxFeatureCount,
  #       "svmC" = svmC
  #     )
  #     cat(",<-- BEST");
  #     }
  #   }
  #   
  #   cat("\n");
  # }
}

cat("Best Result for <nF, C> = ", bestParams$maxFeatureCount, bestParams$svmC, "\n");
cat("Accuracy(Test set): ", bestPerf$acc, "\n");
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
