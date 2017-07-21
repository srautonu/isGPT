source('./base.R');

nFeatures = 2510;
svmC = 0.3;
#trainingSet = features[1:nTrainingSet,];
#testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

#svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done\n");

#nFeatures = length(svmmodel$SV[1,]);

#trainingSet = features[1:nTrainingSet,];
#testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];


filteringRes = featurefiltering(features, testFeatures, rankedFeatures, nFeatures);
#trainingSet = filteringRes$trainingSet;
testSet = filteringRes$testSet;

lcData = c("learningSet","ISE","OSE");
cat("learningSet,ISE,OSE\n");

for (learningSet in seq(from=1, to=nTrain, by=1)) {
  #seed(50)
  trainingsample = sample(filteringRes$trainingSet)
  llength = c(1:nTrain)
  samind = sample(llength)[1:learningSet]
  trainingSet = trainingsample[samind,];
  cat(as.character(Sys.time()),">> learning\n");
  # if (sum(trainingSet$protection) == 0) {
  #   next;
  # }
  
  svmmodel = svm(protection~., trainingSet, kernel = "linear", cost = svmC, cross =0, scale = TRUE);
  #cat(learningSet,"\n");
  # out of sample accuracy
  svmtpred = predict(svmmodel, trainingSet)
  svmpred = predict(svmmodel, testSet);
  svmtprediction = prediction(as.numeric(svmtpred), trainingSet$protection);
  svmprediction = prediction(as.numeric(svmpred), testSet$protection);
  acct = unlist(performance(svmtprediction,"acc")@y.values)[2];
  acc = unlist(performance(svmprediction,"acc")@y.values)[2];
  
  lcData = rbind(lcData, c(learningSet,  1 - acct, 1 - acc));
  write.csv(lcData, lcFile);
  cat(learningSet, "," , 1 - acct, ",", 1 - acc, "\n");
}

cat(as.character(Sys.time()), ">> Done.\n")

#par(mar = c(4, 5, 1.98, 1))
plot(
  x=lcData[,1],
  y=lcData[,2],
  main = "Learning Curve",
  xlab = "Number of Training Samples",
  ylab = "Error in Accuracy",
  cex.main = 2,
  cex.lab = 2,
  box.lty = 2,
  box.lwd = 2,
  xaxis.cex.axis = 2,
  yaxis.cex.axis = 2,
  lwd = 2,
  yaxis.lwd = 2,
  xaxis.lwd = 2,
  yaxis.las = 2,
  xaxis.las = 2,
  ylim = c(0,0.40),
  type="o"
)
lines(
  x=lcData[,1],
  y=lcData[,3],
  lwd = 2,
  col = "blue",
  type="o"
)
legend('topright', c("Train error", "Test error"), lty = c(2,2), lwd = c(2, 2),
       col = c("black", "blue"))

#trainingSet$value = as.numeric(unlist(lapply(trainingSet$protection,function(x){if(x=="Trans-Golgi"){1}else{-1}})))