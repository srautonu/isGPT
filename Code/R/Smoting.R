source('base.R');

smoteFilePrefix = "New_trainingset_SMOTE";
smoteFileSuffix = "_Featurized.rds";

cat(as.character(Sys.time()),">> Original dataset Stats:\n");
cisCount = sum(features$protection == "Cis-Golgi");
transCount = sum(features$protection == "Trans-Golgi");
cat(as.character(Sys.time()),">> Cis-Golgi: ", cisCount, "\n");
cat(as.character(Sys.time()),">> Trans-Golgi: ", transCount, "\n")
cat(as.character(Sys.time()),">> Imbalance ratio: ", cisCount/transCount, "\n")

cat(as.character(Sys.time()),">> Performing SMOTE ...");
smoted = DMwR::SMOTE(protection~., features, perc.over = 300,k=2, perc.under = 200)
cat("DONE\n");

#SMOTE 217 + 217
#Below script worked on Windows machine, but not on Linux
# cat(as.character(Sys.time()),">> Performing SMOTE ...");
# smoted = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 250)
# smoted = DMwR::SMOTE(protection~., smoted, perc.over = 25,k=2, perc.under = 505)
# cat("DONE\n");

# SMOTE 174 + 174
# Below script worked on Windows machine, but not on certain Linux R
# cat(as.character(Sys.time()),">> Performing SMOTE ...\n");
# smoted = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 200)
# cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> SMOTEd dataset Stats:\n");
cisCount = sum(smoted$protection == "Cis-Golgi");
transCount = sum(smoted$protection == "Trans-Golgi");
cat(as.character(Sys.time()),">> Cis-Golgi: ", cisCount, "\n");
cat(as.character(Sys.time()),">> Trans-Golgi: ", transCount, "\n");
cat(as.character(Sys.time()),">> Imbalance ratio: ", cisCount/transCount, "\n");

SmotedFile = paste(smoteFilePrefix, as.character(cisCount), "_", as.character(transCount), smoteFileSuffix, sep = "");
cat(as.character(Sys.time()),">> Saving dataset to ", SmotedFile, " ...\n");
saveRDS(smoted, SmotedFile);
cat(as.character(Sys.time()),">> Done\n");
