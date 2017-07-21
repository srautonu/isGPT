source('base.R')
library(caTools)
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

cat(as.character(Sys.time()),">> Entering to create SVM model", "\n")
ccolors = c("red","black","yellow","green")
i = 1;
maxfeatures = 3500
cat(as.character(Sys.time()),">> Max Features", maxfeatures, "\n")

for (maxFeatureCount in seq(from=maxfeatures, to=2000, by=-500)) 
{ 
  filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;
  
  #svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  svmCostList = c(0.3);
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = TRUE);
    svmpred = predict(svmmodel,testSet)
    svmprediction = prediction(as.numeric(svmpred), as.numeric(testSet$protection));
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)
    perf = max(acc);
    
    cat(maxFeatureCount, ",", svmC, ",", perf,"\n");
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf));
    write.csv(accData, outFile);
    #svmpred = predict(bestSVM, features)
    svmprediction = prediction(as.numeric(svmpred), as.numeric(testSet$protection));
    roccurve <- ROCR::performance(svmprediction,"sens", "spec")
    par(mar = c(4, 5, 1.98, 1))
    plot(
      roccurve,
      main = "Sensitivity Specificity Curve",
      cex.main = 1.7,
      cex.lab = 1.7,
      box.lty = 7,
      box.lwd = 4,
      xaxis.cex.axis = 1.3,
      yaxis.cex.axis = 1.3,
      lwd = 4,
      yaxis.lwd = 4,
      xaxis.lwd = 4,
      yaxis.las = 1,
      col = ccolors[i],
      add = TRUE
    )
    i = i + 1
    auc = performance(svmprediction, "auc")
    #cat(maxFeatureCount,"AUC:",auc@y.values[[1]],"\n")
    perf  <- roccurve #performance(xx.df, "prec", "rec")
    xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
    xy <- subset(xy, !is.nan(xy$precision))
    xy <- rbind(c(0, 0), xy)
    aucpr  <- trapz(xy$recall, xy$precision)
    
    
    cat(maxFeatureCount, "AUPR:",aucpr,"\n")
    # if (bestAcc < perf) {
    #   bestAcc = perf;
    #   bestSVM = svmmodel;
    #   bestParams = list(
    #     "maxFeatureCount" = maxFeatureCount,
    #     "svmC" = svmC
    #   )
    #   cat(",<-- BEST");
    #}
    
    cat("\n");
  }
}

cat("Best Result:\n");
cat("<nF, C, Acc> = ", bestParams$maxFeatureCount, bestParams$svmC, bestAcc, "\n");
saveRDS(bestSVM, "regressionmodel-10-fold-crossvalidation.rds")
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
#source('regressionmodelSMOTE.R')
#source('regressionmodeltest.R')