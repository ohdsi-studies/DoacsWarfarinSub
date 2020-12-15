source("S:/MiscCode/SetEnvironmentVariables.R")
baseUrl <- Sys.getenv("baseUrl")
databases <- data.frame(database = c(database = c("cdm_ibm_ccae_v1103.ohdsi_results",
                                                  "cdm_ibm_mdcr_v1104.ohdsi_results",
                                                  "cdm_ibm_mdcd_v1105.ohdsi_results",
                                                  "cdm_optum_extended_dod_v1107.ohdsi_results")),
                        name = c("IBM CCAE",
                                 "IBM MDCR",
                                 "IBM MDCD",
                                 "Optum SES"),
                        sourceKey = c("IBM_CCAE",
                                      "IBM_MDCR",
                                      "IBM_MDCD",
                                      "OPTUM_EXTENDED_DOD"),
                        stringsAsFactors = FALSE)

doseTestIds <- c(11547, # riva afib
                 11548, # riva vte
                 11551, # riva thr
                 11561, # apix afib built
                 11562, # apix vte
                 11564, # apix thr
                 11565, # dabi afib
                 11566, # dabi vte
                 11568) # dabi thr not built
doseTestCounts <- ROhdsiWebApi::getCohortGenerationStatuses(baseUrl = baseUrl, definitionIds = doseTestIds, sourceKeys = databases$sourceKey)
doseTestNames <- unique(doseTestCounts[, c("definitionId", "definitionName")])
doseTestNames$definitionId <- as.numeric(doseTestNames$definitionId)

keeps <- c("description", "indexPersonCount", "rulePersonCount", "rulePercentSatisfied")

dbRows <- data.frame()
for (i in 1:nrow(databases)) { # i = 1
  sourceKey <- databases$sourceKey[i]
  databaseName <- databases$name[i]
  cohortRows <- data.frame()
  for (j in doseTestIds) { # j = 2
    cohortId <- j
    ruleCounts <- ROhdsiWebApi::getCohortInclusionRulesAndCounts(baseUrl = baseUrl, 
                                                                 cohortId = cohortId, 
                                                                 sourceKey = sourceKey)
    ruleCounts <- ruleCounts[, keeps]
    ruleCounts <- cbind(cohortId, ruleCounts)
    ruleCounts <- merge(doseTestNames, ruleCounts, by.x = "definitionId", by.y = "cohortId")
    ruleCounts <- cbind(databaseName, ruleCounts)
    ruleCounts$doseEqualsTotal <- ifelse(sum(ruleCounts$rulePersonCount) == ruleCounts$indexPersonCount[1], 1, 0)
    cohortRows <- rbind(cohortRows, ruleCounts)
  }
  dbRows <- rbind(dbRows, cohortRows)
}
startingDoses <- dbRows
write.csv(startingDoses, "S:/Git/Bitbucket/epi_680/Protocol/Tables_Annex/Annex_C_starting_doses.csv", row.names = FALSE)
