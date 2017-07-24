source('base.R');

bestAcc = 0;
bestSVM = NULL;
bestParams = NULL;
accData = NULL;

cat(as.character(Sys.time()),">> Entering to create SVM model ...\n");

cat(as.character(Sys.time()),">> Performing SMOTE ...");
features = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 250)
features = DMwR::SMOTE(protection~., features, perc.over = 25,k=2, perc.under = 505)
cat("DONE\n");

#maxFeatureCount = 2150;
for (maxFeatureCount in seq(from=2000, to=2500, by=50)) 
{
  
  filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;

  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  svmC = 0.3;
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = TRUE);
    perf = svmmodel$tot.accuracy;
    
    cat(maxFeatureCount, ",", svmC, ",", perf);

    accData = rbind(accData, c(maxFeatureCount, svmC, perf));
    write.csv(accData, outFile);

    if (bestAcc < perf) {
      bestAcc = perf;
      bestSVM = svmmodel;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

cat("Best Result:\n");
cat("<nF, C, Acc> = ", bestParams$maxFeatureCount, bestParams$svmC, bestAcc, "\n");
saveRDS(bestSVM, svmFile)