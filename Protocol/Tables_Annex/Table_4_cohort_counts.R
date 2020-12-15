source("S:/MiscCode/SetEnvironmentVariables.R")
baseUrl <- Sys.getenv("baseUrl")
databases <- data.frame(database = c(database = c("cdm_ibm_ccae_v1103.ohdsi_results",
                                                  "cdm_ibm_mdcr_v1104.ohdsi_results",
                                                  "cdm_ibm_mdcd_v1105.ohdsi_results",
                                                  "cdm_optum_extended_dod_v1107.ohdsi_results")),
                        name = c("IBM CCAE",
                                 "IBM MDCR",
                                 "IBM MDCD",
                                 "Optum DOD"),
                        sourceKey = c("IBM_CCAE",
                                      "IBM_MDCR",
                                      "IBM_MDCD",
                                      "OPTUM_EXTENDED_DOD"),
                        stringsAsFactors = FALSE)
cohortIds <- c(11400, 11401, 11402, 11403, 11404, 11405, 11406, 11407, 11412, 11413, 11414, 11415)

table <- data.frame(CohortId = cohortIds,
                    CohortName = sapply(cohortIds, ROhdsiWebApi::getCohortDefinitionName, baseUrl = baseUrl, formatName = TRUE))
getFinalCount <- function(baseUrl, cohortId, sourceKey) {
  url <- sprintf("%s/cohortdefinition/%d/report/%s?mode=0", baseUrl, cohortId, sourceKey)
  json <- httr::GET(url)
  json <- httr::content(json)
  finalCount <- json$summary$finalCount[1]
  return(finalCount)
}
for (i in 1:nrow(databases)) { # i=1
  counts <- sapply(cohortIds, getFinalCount, baseUrl = baseUrl, sourceKey = databases$sourceKey[i])
  table[, databases$name[[i]]] <- counts
}
write.csv(table, "S:/Git/Bitbucket/epi_680/Protocol/Tables_Annex/Table_4_cohort_counts.csv", row.names = FALSE)
# statuses <- ROhdsiWebApi::getCohortGenerationStatuses(baseUrl = baseUrl, definitionIds = cohortIds, sourceKeys = sourceKey)
