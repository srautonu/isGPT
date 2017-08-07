source('featurization.R');

timestamp();

fScheme = "_nGDip";

seqFile   = "trainingset.csv";
featureFile = paste("featurized", fScheme, ".rds", sep = "");

amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  cat(as.character(Sys.time()),">> Reading file (", seqFile, ") ...\n");
  data = read.csv(seqFile);
  cat(as.character(Sys.time()),">> Done\n");

  cat(as.character(Sys.time()),">> data set entries:", length(data[,1]), "\n");

  features = featurization(data$Sequence, data$Class, amins, nGramOrder = 0, nGDipOrder = 25, psfOrder = 0);
  
  saveRDS(features, featureFile);

  cat(as.character(Sys.time()),">> Featurizing Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");
