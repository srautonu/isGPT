source('base.R')
library(DMwR)
features = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 200)
#features$protection = unlist(lapply(features$protection,function(x){if(x=="Trans-Golgi"){1}else{-1}}))

bestAcc = 0;
bestSVM = NULL;
bestParams = NULL;
accData = NULL;

#nTrainingSet = read.csv('trainingset.csv')
#nTestSet = read.csv('testingset.csv')
#cat(as.character(Sys.time()),">> Training set entries:", length(nTrainingSet[,1]), "\n");
#cat(as.character(Sys.time()),">> Test set entries:", length(nTestSet[,1]), "\n");

#2150 features best

cat(as.character(Sys.time()),">> Entering to create SVM SMOTE  model", "\n")

maxfeatures = 2250
cat(as.character(Sys.time()),">> Max Features", maxfeatures, "\n")

for (maxFeatureCount in seq(from=maxfeatures, to=550, by=-50)) 
{ 
  filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;
  
  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = TRUE);
    svmpred = predict(svmmodel, trainingSet)
    svmprediction = prediction(as.numeric(svmpred), as.numeric(trainingSet$protection));
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)
    perf = max(acc);
    
    #cat(maxFeatureCount, ",", svmC, ",", perf);
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf));
    write.csv(accData, outFile);
    
    if (bestAcc < perf) {
      bestAcc = perf;
      bestSVM = svmmodel;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      #cat(",<-- BEST");
    }
    
    #cat("\n");
  }
}

cat("Best Result:\n");
cat("<nF, C, Acc> = ", bestParams$maxFeatureCount, bestParams$svmC, bestAcc, "\n");
saveRDS(bestSVM, "regressionmodelsmote-10-fold-crossvalidation.rds")
svmpred = predict(bestSVM, features)
svmprediction = prediction(as.numeric(svmpred), as.numeric(trainingSet$protection));
acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)
F1 = unlist(ROCR::performance(svmprediction,"f")@y.values)
prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)
recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)
sensitiviy = unlist(ROCR::performance(svmprediction,"sens")@y.values)
specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)
mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)

cat("Accuracy(Test set): ", max(acc, na.rm = TRUE), "\n");
cat("F1-Score (Test set): ", max(F1, na.rm = TRUE), "\n");
#cat("Precision(Test set): ", prec, "\n");
#cat("Recall   (Test set): ", recall, "\n");
#cat("Sensitivity(Test set): ", sensitiviy, "\n");
#cat("Specificity(Test set): ", specificity, "\n")
cat("MCC(Test set): ", max(mccv, na.rm = TRUE), "\n")
