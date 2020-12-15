# Remove following negative controls: 

# Pruritic rash - 135618
# Pruritus of skin - 136184
# Ataxia as sequela of cerebrovascular disease - 43531622
# Palpitations - 315078

# negative controls csv
pathToCsv <- system.file("settings", "NegativeControls.csv", package = "DoacsWarfarinSub")
negativeControls <- read.csv(pathToCsv)
write.csv(negativeControls, "inst/settings/NegativeControls_original.csv", row.names = FALSE)
drops <- negativeControls$outcomeId %in% c(135618, 136184, 43531622, 315078)
negativeControls <- negativeControls[!drops, ]
write.csv(negativeControls, "inst/settings/NegativeControls.csv", row.names = FALSE)

# rebuild package


# all controls from each database
studyFolder <- "G:/StudyResults/epi_680"
timeAtRisks <- c("OnTreatment", "IntentToTreat") 
outputFolders <- c(file.path(studyFolder, "CCAE"),
                   file.path(studyFolder, "MDCR"),
                   file.path(studyFolder, "MDCD"),
                   file.path(studyFolder, "Optum"))

for (outputFolder in outputFolders) { 
  for (timeAtRisk in timeAtRisks) { 
    pathToCsv <- file.path(outputFolder, timeAtRisk, "AllControls.csv")
    allControls <- read.csv(pathToCsv)
    write.csv(allControls, file.path(outputFolder, timeAtRisk, "AllControls_original.csv"), row.names = FALSE)
    drops <- allControls$oldOutcomeId %in% c(135618, 136184, 43531622, 315078)
    allControls <- allControls[!drops, ]
    write.csv(allControls, pathToCsv, row.names = FALSE)
  }
}
# rerun export function for all databases for re-calibrate with 4 negative controls removed
# rerun prepare for shiny function