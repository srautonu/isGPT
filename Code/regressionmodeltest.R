#source('base.R');

cat(as.character(Sys.time()),
    ">> Reading SVM model from",
    svmFile,
    "...\n")

svmmodel = readRDS('regressionmodelsmote-10-fold-crossvalidation.rds')

#svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()), ">> Done\n")


nFeatures = length(svmmodel$SV[1, ])


#trainingSet = features[1:nTrainingSet,];
#testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];


filteringRes = featurefiltering(features, testFeatures, rankedFeatures, nFeatures)

trainingSet = filteringRes$trainingSet

testSet = filteringRes$testSet


#notFound = filteringRes$notFound;

cat(
  as.character(Sys.time()),
  ">> Trained:Test Features",
  length(trainingSet[1, ]),
  ":",
  length(testSet[1, ]),
  "\n"
)


#cat("Not Found:",as.vector(notFound),"\n")

svmpred = predict(svmmodel, testSet)

svmprediction = prediction(as.numeric(svmpred), as.vector(testSet$protection))


#acc = max(unlist(performance(svmprediction,"acc")@y.values), na.rm = TRUE)
#F1 = max(unlist(performance(svmprediction,"f")@y.values), na.rm = TRUE)
#prec = max(unlist(performance(svmprediction,"prec")@y.values), na.rm = TRUE)
#recall = max(unlist(performance(svmprediction,"rec")@y.values), na.rm = TRUE)
#sensitiviy = max(unlist(performance(svmprediction,"sens")@y.values), na.rm = TRUE)
#specificity = max(unlist(performance(svmprediction,"spec")@y.values), na.rm = TRUE)
#mccv = max(unlist(performance(svmprediction,"mat")@y.values), na.rm = TRUE)

#saveRDS(bestSVM, "regressionmodel-10-fold-crossvalidation.rds")
#svmpred = predict(bestSVM, trainingSet)
#svmprediction = prediction(as.numeric(svmpred), as.numeric(trainingSet$protection));
acc = unlist(ROCR::performance(svmprediction, "acc")@y.values)
F1 = unlist(ROCR::performance(svmprediction, "f")@y.values)
prec = unlist(ROCR::performance(svmprediction, "prec")@y.values)
recall = unlist(ROCR::performance(svmprediction, "rec")@y.values)
sensitiviy = unlist(ROCR::performance(svmprediction, "sens")@y.values)
specificity = unlist(ROCR::performance(svmprediction, "spec")@y.values)
mccv = unlist(ROCR::performance(svmprediction, "mat")@y.values)

cat("Accuracy(Test set): ", max(acc, na.rm = TRUE), "\n")

cat("F1-Score (Test set): ", max(F1, na.rm = TRUE), "\n")

#cat("Precision(Test set): ", prec, "\n");
#cat("Recall   (Test set): ", recall, "\n");
#cat("Sensitivity(Test set): ", sensitiviy, "\n");
#cat("Specificity(Test set): ", specificity, "\n")
cat("MCC(Test set): ", max(mccv, na.rm = TRUE), "\n")

roccurve <- ROCR::performance(svmprediction,"tpr","fpr")
par(mar = c(4, 5, 1.98, 1))
plot(
  roccurve,
  main = "ROC-Curve",
  cex.main = 1.7,
  cex.lab = 1.7,
  box.lty = 7,
  box.lwd = 4,
  xaxis.cex.axis = 1.3,
  yaxis.cex.axis = 1.3,
  lwd = 4,
  yaxis.lwd = 4,
  xaxis.lwd = 4,
  yaxis.las = 1
  #add = TRUE
)
