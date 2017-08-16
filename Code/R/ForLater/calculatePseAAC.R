library(protr)

inputFile  = "testingset.csv";
outputFile = "testPseAAC.csv";

input = read.csv(inputFile)

cat(as.character(Sys.time()),">> Input set entries:", length(input[,1]), "\n");

pseAAC = NULL;
for (proteinSeq in input$Sequence) {
  t = protr::extractPAAC(proteinSeq)
  pseAAC = rbind(pseAAC, t);
}

pseAAC = cbind(pseAAC, as.character(input$Class));
colnames(pseAAC)[length(colnames(pseAAC))] = "protection";
write.csv(pseAAC, outputFile, row.names = FALSE);
