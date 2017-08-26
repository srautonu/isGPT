library(e1071)
source('./filteredFeaturization.R');

# Customize parameters here. Predictions will be written to
# predicted.csv file.
#
# sequenceFile: 
#   Where the query sequences are given. The sequences must be in
#   a column called "Sequence". Also, there should be an "Id" column
# threshold:
#   Class discriminating threshold.
sequenceFile = "testingset.csv";
threshold    = 0.5;

predictionFile = "predicted.csv";
svmFile        = "svmIsGPT.rds";

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Reading sequence file from", sequenceFile, "...\n");
data = read.csv(sequenceFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Generating features ...\n");
querySet = filteredFeaturization(data$Sequence, colnames(svmmodel$SV));
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Predicting subGolgi protein type ...\n");
svmpred = predict(svmmodel, querySet);

predictions = data.frame(
                Id = data$Id, 
                Score = svmpred, 
                Class = ifelse (svmpred >= threshold, "Cis-Golgi", "Trans-Golgi")
                );

write.csv(predictions, predictionFile, row.names = FALSE);
cat(as.character(Sys.time()),">> Done. Predictions saved to", predictionFile, "\n");
