start <- Sys.time()

studyFolder <- "G:/StudyResults/epi_680"
reportFolder <- "./report"
source("global.R")
source("report/ReportPlotsAndTables.R")
library("magrittr")

# Global parameters ------------------------------------------------------------

databaseIds <- database$databaseId
outcomeId <- 14347
indicationLabels <- c("NVAF", "VTE", "TKR/THR")
nvafCohortIds <- c(11400, 11403, 11401, 11402, 13033, 13234, 13199, 13200)
vteCohortIds <- c(11404, 11407, 11405, 11406, 13203, 13235, 13204, 13205)
thrTkrCohortIds <- c(11412, 11415, 11413, 11414, 13211, 13237, 13212, 13213)
indicationGroups <- list(list(indication = "NVAF", cohortIds = nvafCohortIds),
                         list(indication = "VTE", cohortIds = vteCohortIds),
                         list(indication = "THR/TKR", cohortIds = thrTkrCohortIds))
blank <- ""
blankPlot <- ggplot2::ggplot() + ggplot2::theme_void()

headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")

indicationTitles <- list()
indicationTitles[[1]] <- officer::fpar(officer::ftext("NVAF", prop = titleFormat))
indicationTitles[[7]] <- officer::fpar(officer::ftext("VTE", prop = titleFormat))
indicationTitles[[13]] <- officer::fpar(officer::ftext("TKR/THR", prop = titleFormat))

# Exposure cohort counts--------------------------------------------------------

countsFolder <- file.path(reportFolder, "countsFolder")
if (!file.exists(countsFolder)) {
  dir.create(countsFolder)
}

section1 <- "Study population counts"
section1 <- officer::fpar(officer::ftext(section1, prop = headingFormat))

counts <- getPatientCounts(attrition, "Original cohorts")
counts$subjects <- formatC(counts$subjects, big.mark = ",", format = "d")
counts$exposureSubjects <- formatC(counts$exposureSubjects, big.mark = ",", format = "d")
counts$databaseSubjects <- formatC(counts$databaseSubjects, big.mark = ",", format = "d")

# by exposure
exposureCountTitle <- "Counts by exposure cohort"
exposureCountTitle <- officer::fpar(officer::ftext(exposureCountTitle, prop = titleFormat))
countsByExposure <- counts
countsByExposure$indicationName <- sub('\\:.*', "", countsByExposure$exposureName)
countsByExposure$exposureName <-  capitalize(sub(".*? ", "", countsByExposure$exposureName))
countsByExposure$totalExposureCount <- sprintf("%s (n = %s)", countsByExposure$exposureName, counts$exposureSubjects)
countsByExposure <- countsByExposure[, c("indicationName", "totalExposureCount", "databaseId", "subjects", "exposurePercent")]
countsByExposure$subjects <- gsub("^-", "<", countsByExposure$subjects)
countsByExposure$exposurePercent <- gsub("^-", "<", countsByExposure$exposurePercent)
header <- c("Indication", "Exposure", "Database", "Patients", "Percent")
countsByExposure <- rbind(header, countsByExposure)
countsByExposure <- createCountsFlextable(countsByExposure)

# by database
countsByDatabase <- counts
databaseCountTitle <- "Counts by database"
databaseCountTitle <- officer::fpar(officer::ftext(databaseCountTitle, prop = titleFormat))
countsByDatabase$databaseOrder <- match(countsByDatabase$databaseId, countsByDatabase$databaseId)
countsByDatabase$exposureOrder <- match(countsByDatabase$exposureName, countsByDatabase$exposureName)
countsByDatabase <- countsByDatabase[order(countsByDatabase$databaseOrder, countsByDatabase$exposureOrder), ]
countsByDatabase[, c("databaseOrder", "exposureOrder")] <- NULL
countsByDatabase$totalDatabaseCount <- sprintf("%s (n = %s)", countsByDatabase$databaseId, countsByDatabase$databaseSubjects)
countsByDatabase$indicationName <- sub('\\:.*', "", countsByDatabase$exposureName)
countsByDatabase$exposureName <-  capitalize(sub(".*? ", "", countsByDatabase$exposureName))
countsByDatabase <- countsByDatabase[, c("totalDatabaseCount", "indicationName", "exposureName", "subjects", "databasePercent")]
countsByDatabase$subjects <- gsub("^-", "<", countsByDatabase$subjects)
countsByDatabase$databasePercent <- gsub("^-", "<", countsByDatabase$databasePercent)
header <- c("Database", "Indication", "Exposure", "Patients", "Percent")
countsByDatabase <- rbind(header, countsByDatabase)
countsByDatabase <- createCountsFlextable(countsByDatabase)

doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section1, style = "heading 1") %>%
  officer::body_add_fpar(exposureCountTitle, style = "heading 3") %>%
  flextable::body_add_flextable(countsByExposure) %>%
  officer::body_add_break() %>%
  officer::body_add_fpar(databaseCountTitle, style = "heading 3") %>%
  flextable::body_add_flextable(countsByDatabase) %>%
  print(target = file.path(countsFolder, "studyPopCounts.docx"))

# Baseline characteristics -----------------------------------------------------

table1sFolder <- file.path(reportFolder, "table1sFolder")
if (!file.exists(table1sFolder)) {
  dir.create(table1sFolder)
}

# age, continuous distribution
# source("S:/MiscCode/SetEnvironmentVariables.R")
# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
#                                                                 server = Sys.getenv("server"),
#                                                                 user = NULL,
#                                                                 password = NULL,
#                                                                 port = Sys.getenv("port"))
# 
# covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE)
# 
# dbs <- data.frame(name = c("CCAE", "MDCD", "MDCR", "Optum"), 
#                   cdmDatabaseSchema = c("CDM_IBM_CCAE_V1103.dbo", "CDM_IBM_MDCD_V1105.dbo", "CDM_IBM_MDCR_V1104.dbo", "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"),
#                   cohortDatabaseSchema = rep("scratch.dbo", 4),
#                   cohortTable = c("epi_680_ccae", "epi_680_mdcd", "epi_680_mdcr", "epi_680_optum"),
#                   stringsAsFactors = FALSE)
# 
# ageDistResults <- data.frame()
# exposureOfInterestPrimary <- exposureOfInterest[1:12, ]
#   
# for (i in 1:nrow(exposureOfInterestPrimary)) { # i=1
#   cohortId <- exposureOfInterestPrimary$exposureId[i]
#     
#   for (j in 1:nrow(dbs)) { # j=1
#     cdmDatabaseSchema <- dbs$cdmDatabaseSchema[j]
#     cohortDatabaseSchema <- dbs$cohortDatabaseSchema[j]
#     cohortTable <- dbs$cohortTable[j]
#     dbName <- dbs$name[j]
#     covariateData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
#                                                            cdmDatabaseSchema = cdmDatabaseSchema,
#                                                            cohortDatabaseSchema = cohortDatabaseSchema,
#                                                            cohortTable = cohortTable,
#                                                            cohortId = cohortId,
#                                                            covariateSettings = covariateSettings)
#     ageData <- as.data.frame(covariateData$covariates)
#     ageDist <- data.frame(cohort = exposureOfInterestPrimary$exposureName[exposureOfInterestPrimary$exposureId == cohortId],
#                           source = dbName,
#                           n = nrow(ageData),
#                           mean = round(mean(ageData$covariateValue), 2),
#                           sd = round(sd(ageData$covariateValue), 2),
#                           min = quantile(ageData$covariateValue)[[1]],
#                           p25 = quantile(ageData$covariateValue)[[2]],
#                           p50 = quantile(ageData$covariateValue)[[3]],
#                           p75 = quantile(ageData$covariateValue)[[4]],
#                           max = quantile(ageData$covariateValue)[[5]])
#     ageDistResults <- rbind(ageDistResults, ageDist)
#   }
# }
# write.csv(ageDistResults, file.path(table1sFolder, "ageDist.csv"), row.names = FALSE)

# table 1s
section2 <- "Patient baseline characteristics"
section2 <- officer::fpar(officer::ftext(section2, prop = headingFormat))

table1s <- list()
table1Titles <- list()

for (analysisId in c(1)) {  # analysisId <- 1
  analysisName <- cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == analysisId]
  analysisName <- sub(".*? ", "", analysisName)
  tcs <- tcos[tcos$targetId %in% primaryTarCohortIds | tcos$comparatorId %in% primaryTarCohortIds, ]
  
  for (databaseId in c("Optum")){ #databaseIds[databaseIds != "Meta-analysis"]) { # databaseId <- "MDCD"
    for (i in 1:nrow(tcs)) { # i=14
      targetLabel <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == tcos$targetId[i]]
      targetLabel <- capitalize(sub(".*? ", "", targetLabel))
      comparatorLabel <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == tcos$comparatorId[i]]
      comparatorLabel <- capitalize(sub(".*? ", "", comparatorLabel))
      balance <- getCovariateBalance(targetId = tcs$targetId[i],
                                     comparatorId = tcs$comparatorId[i],
                                     databaseId = databaseId,
                                     analysisId = analysisId,
                                     outcomeId = outcomeId) 
      if (!is.null(balance)) {
        table1 <- prepareTable1(balance,
                                targetLabel = sub(".*? ", "", targetLabel),
                                comparatorLabel = comparatorLabel)
        facs <- sapply(table1, is.factor)
        table1[facs] <- lapply(table1[facs], as.character)
        rm(facs)
        table1 <- rbind(c(blank, "Before matching", blank, blank, "After matching", blank, blank), table1)
        colnames(table1) <- letters[1:length(colnames(table1))]
        
        table1 <- flextable::qflextable(table1)
        table1 <- flextable::delete_part(table1, part = "header")
        table1 <- flextable::fontsize(table1, part = "all", size = 6)
        table1 <- flextable::align(table1, j = 1, align = "left", part = "all")
        table1 <- flextable::merge_h(table1, i = 1, part = "body")
        table1 <- flextable::autofit(table1, add_w = 0, add_h = 0)
        table1 <- flextable::padding(table1, padding = 0, part = "all")
        border <- officer::fp_border(color = "black", width = 1)
        table1 <- flextable::border_inner(table1, border = border, part = "all")
        table1 <- flextable::border_outer(table1, border = border, part = "all")
      } else {
        table1 <- data.frame(a = NA, b = NA, c = NA)
        table1 <- flextable::qflextable(table1)
      }
      table1s[[length(table1s) + 1]] <- table1
      title <- sprintf("%s vs. %s", targetLabel, comparatorLabel)
      #title <- sprintf("%s (%s), %s vs. %s", databaseId, analysisName, targetLabel, comparatorLabel)
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))  
      table1Titles[[length(table1Titles) + 1]] <- title
    }
  }
}
table1Pairs <- list(table1Titles, table1s)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section2, style = "heading 1")
for(i in 1:length(table1s)) { #i=1
  if (i %in% c(1, 7, 13)) {
    doc <- doc %>% 
      officer::body_add_fpar(indicationTitles[[i]], style = "heading 2") %>%
      officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 3") %>%
      flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
      officer::body_add_break()
    
  } else {
    doc <- doc %>% 
      officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 3") %>%
      flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
      officer::body_add_break()
  }
}
print(doc, target = file.path(table1sFolder, "table1s.docx"))

# Crude IRs -------------------------------------------------------------------------

crudeIrFolder <- file.path(reportFolder, "crudeIrFolder")
if (!file.exists(crudeIrFolder)) {
  dir.create(crudeIrFolder)
}

section3a <- "Time-at-risk distribution"
section3a <- officer::fpar(officer::ftext(section3a, prop = headingFormat))
section3b <- "Unadjusted incidence rates"
section3b <- officer::fpar(officer::ftext(section3b, prop = headingFormat))

source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))

covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE,
                                                                useDemographicsIndexYear = TRUE)

dbs <- data.frame(name = c("CCAE", "MDCD", "MDCR", "Optum"), 
                  cdmDatabaseSchema = c("CDM_IBM_CCAE_V1103.dbo", "CDM_IBM_MDCD_V1105.dbo", "CDM_IBM_MDCR_V1104.dbo", "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"),
                  cohortDatabaseSchema = rep("scratch.dbo", 4),
                  cohortTable = c("epi_680_ccae", "epi_680_mdcd", "epi_680_mdcr", "epi_680_optum"),
                  stringsAsFactors = FALSE)

irResults <- data.frame()
attritionResults <- data.frame()
tarResults <- data.frame()

for (i in 1:nrow(dbs)) { # i=2
  cdmDatabaseSchema <- dbs$cdmDatabaseSchema[i]
  cohortDatabaseSchema <- dbs$cohortDatabaseSchema[i]
  cohortTable <- dbs$cohortTable[i]
  dbName <- dbs$name[i]
  
  for (j in 1:nrow(exposureOfInterest)) { # j=1
    if (exposureOfInterest$exposureId[j] %in% primaryTarCohortIds) {
      analysisName <- "On-treatment"
      tarId <- 1
    } else {
      analysisName <- "On-treatment (30-day gap)"
      tarId <- 2
    }
    exposureId <- exposureOfInterest$exposureId[j]
    exposureName <- exposureOfInterest$exposureName[j]
    indication <- sub('\\:.*', "", exposureName)
    exposureName <- capitalize(sub(".*? ", "", exposureName))
    studyPopFile <- sprintf("studyPop_e%s_tar%s_%s.rds", exposureId, tarId, dbName)
    studyPopFile <- file.path(crudeIrFolder, studyPopFile)
    if (!file.exists(studyPopFile)) {
      cmData <- CohortMethod::getDbCohortMethodData(connectionDetails = connectionDetails,
                                                    studyStartDate = "",
                                                    studyEndDate = "",
                                                    excludeDrugsFromCovariates = FALSE,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    targetId = exposureId,
                                                    comparatorId = 0,
                                                    outcomeIds = outcomeId,
                                                    exposureDatabaseSchema = cohortDatabaseSchema,
                                                    exposureTable = cohortTable,
                                                    outcomeDatabaseSchema = cohortDatabaseSchema,
                                                    outcomeTable = cohortTable,
                                                    covariateSettings = covariateSettings)
      studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData, 
                                                      outcomeId = outcomeId,
                                                      removeSubjectsWithPriorOutcome = TRUE,
                                                      minDaysAtRisk = 1,
                                                      startAnchor = "cohort start",
                                                      riskWindowStart = 1,
                                                      endAnchor = "cohort end",
                                                      riskWindowEnd = 5)
      saveRDS(studyPop, studyPopFile)
    } else {
      studyPop <- readRDS(studyPopFile)
    }
    # ir/ip table
    irIpTable <- getIrIp(studyPop)
    irIpTable <- cbind(analysisName, indication, exposureName, dbName, irIpTable)
    irResults <- rbind(irResults, irIpTable)
    
    studyPopAttrition <- attr(studyPop, "metaData")$attrition
    studyPopAttrition <- studyPopAttrition[, c(1,2)]
    studyPopAttrition <- t(studyPopAttrition)
    colNames <- studyPopAttrition[1, ]
    studyPopAttrition <- as.data.frame(studyPopAttrition, row.names = FALSE)
    studyPopAttrition <- studyPopAttrition[-1, ]
    names(studyPopAttrition) <- colNames
    studyPopAttrition <- cbind(analysisName, indication, exposureName, dbName, studyPopAttrition)
    attritionResults <- rbind(attritionResults, studyPopAttrition)
    
    # tar table
    tarTable <- getTarDist(studyPop)
    tarTable <- cbind(analysisName, indication, exposureName, dbName, tarTable)
    tarResults <- rbind(tarResults, tarTable)
    
    if (exposureOfInterest$exposureId[j] %in% primaryTarCohortIds) {
      analysisName <- "ITT"
      tarId <- 3
      studyPopFile <- sprintf("studyPop_e%s_tar%s_%s.rds", exposureId, tarId, dbName)
      studyPopFile <- file.path(crudeIrFolder, studyPopFile)
      if (!file.exists(studyPopFile)) {
        studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData, 
                                                        outcomeId = outcomeId,
                                                        removeSubjectsWithPriorOutcome = TRUE,
                                                        minDaysAtRisk = 1,
                                                        startAnchor = "cohort start",
                                                        riskWindowStart = 1,
                                                        endAnchor = "cohort start",
                                                        riskWindowEnd = 9999)
        saveRDS(studyPop, studyPopFile)
      } else {
        studyPop <- readRDS(studyPopFile)
      }
      irIpTable <- getIrIp(studyPop)
      irIpTable <- cbind(analysisName, indication, exposureName, dbName, irIpTable)
      irResults <- rbind(irResults, irIpTable)
      
      studyPopAttrition <- attr(studyPop, "metaData")$attrition
      studyPopAttrition <- studyPopAttrition[, c(1,2)]
      studyPopAttrition <- t(studyPopAttrition)
      colNames <- studyPopAttrition[1, ]
      studyPopAttrition <- as.data.frame(studyPopAttrition, row.names = FALSE)
      studyPopAttrition <- studyPopAttrition[-1, ]
      names(studyPopAttrition) <- colNames
      studyPopAttrition <- cbind(analysisName, indication, exposureName, dbName, studyPopAttrition)
      attritionResults <- rbind(attritionResults, studyPopAttrition)
      
      # tar table
      tarTable <- getTarDist(studyPop)
      tarTable <- cbind(analysisName, indication, exposureName, dbName, tarTable)
      tarResults <- rbind(tarResults, tarTable)
    }
  }
}
crudeIrTable <- irResults
crudeIrTable$exposureName <- gsub(" (30d gap)", "", crudeIrTable$exposureName, fixed = TRUE)
crudeIrTable$analysisOrder <- match(crudeIrTable$analysisName, c("On-treatment", "On-treatment (30-day gap)", "ITT"))
crudeIrTable$indicationOrder <- match(crudeIrTable$indication, c("NVAF", "VTE", "TKR/THR"))
crudeIrTable$exposureOrder <- match(crudeIrTable$exposureName, c("Rivaroxaban", "Apixaban", "Dabigatran", "Warfarin"))
crudeIrTable$dbOrder <- match(crudeIrTable$dbName, databaseIds)
crudeIrTable <- crudeIrTable[order(crudeIrTable$analysisOrder, crudeIrTable$indicationOrder, crudeIrTable$exposureOrder, crudeIrTable$dbOrder), ]
crudeIrTable[, c("analysisOrder", "indicationOrder", "exposureOrder", "dbOrder")] <- NULL
keeps <- c("events", "pys", "ir1k", "ip1k")
onTreatment <- crudeIrTable[crudeIrTable$analysisName == "On-treatment", -1]
onTreatment30 <- crudeIrTable[crudeIrTable$analysisName == "On-treatment (30-day gap)", keeps]
itt <- crudeIrTable[crudeIrTable$analysisName == "ITT", keeps]
crudeIrTable <- cbind(onTreatment, onTreatment30, itt)
facs <- sapply(crudeIrTable, is.factor)
crudeIrTable[facs] <- lapply(crudeIrTable[facs], as.character)
rm(facs)
header1 <- c(blank, blank, blank, blank, rep("On-treatment", 4), rep("On-treatment (30-day gap)", 4), rep("Intent-to-treat", 4))
header2 <- c("Indication", "Exposure", "Database", "Patients", rep(c("Events", "PYs", "IR/1,000 PYs", "IP/1,000 P"), 3))
crudeIrTable <- rbind(header1, header2, crudeIrTable)
names(crudeIrTable) <- letters[1:ncol(crudeIrTable)]

crudeIrTable <- flextable::qflextable(crudeIrTable)
crudeIrTable <- flextable::delete_part(crudeIrTable, part = "header")
crudeIrTable <- flextable::fontsize(crudeIrTable, part = "all", size = 6)
crudeIrTable <- flextable::merge_v(crudeIrTable, j = 1:3, part = "body")
crudeIrTable <- flextable::merge_h(crudeIrTable, i = 1, part = "body")
border <- officer::fp_border(color = "black", width = 1)
crudeIrTable <- flextable::border_inner(crudeIrTable, border = border, part = "all")
crudeIrTable <- flextable::border_outer(crudeIrTable, border = border, part = "all")
crudeIrTable <- flextable::align(crudeIrTable, j = 1:3, align = "left", part = "all")
crudeIrTable <- flextable::align(crudeIrTable, i = 1:2, align = "left", part = "all")
crudeIrTable <- flextable::autofit(crudeIrTable, add_w = 0.1, add_h = 0.1)
crudeIrTable <- flextable::padding(crudeIrTable, padding = 0, part = "all")

fullTarTable <- tarResults
fullTarTable$exposureName <- gsub(" (30d gap)", "", fullTarTable$exposureName, fixed = TRUE)
fullTarTable$analysisOrder <- match(fullTarTable$analysisName, c("On-treatment", "On-treatment (30-day gap)", "ITT"))
fullTarTable$indicationOrder <- match(fullTarTable$indication, c("NVAF", "VTE", "TKR/THR"))
fullTarTable$exposureOrder <- match(fullTarTable$exposureName, c("Rivaroxaban", "Apixaban", "Dabigatran", "Warfarin"))
fullTarTable$dbOrder <- match(fullTarTable$dbName, databaseIds)
fullTarTable <- fullTarTable[order(fullTarTable$analysisOrder, fullTarTable$indicationOrder, fullTarTable$exposureOrder, fullTarTable$dbOrder), ]
fullTarTable[, c("analysisOrder", "indicationOrder", "exposureOrder", "dbOrder")] <- NULL
keeps <- c("meanTar", "sdTar", "minTar", "p25Tar", "p50Tar", "p75Tar", "maxTar")
onTreatment <- fullTarTable[fullTarTable$analysisName == "On-treatment", -1]
onTreatment30 <- fullTarTable[fullTarTable$analysisName == "On-treatment (30-day gap)", keeps]
itt <- fullTarTable[fullTarTable$analysisName == "ITT", keeps]
fullTarTable <- cbind(onTreatment, onTreatment30, itt)
facs <- sapply(fullTarTable, is.factor)
fullTarTable[facs] <- lapply(fullTarTable[facs], as.character)
rm(facs)
header1 <- c(blank, blank, blank, blank, rep("On-treatment", 7), rep("On-treatment (30-day gap)", 7), rep("Intent-to-treat", 7))
header2 <- c("Indication", "Exposure", "Database", "Patients", rep(c("Mean", "SD", "Min", "25%ile", "Median", "75%ile", "Max"), 3))
fullTarTable <- rbind(header1, header2, fullTarTable)
names(fullTarTable) <- letters[1:ncol(fullTarTable)]

fullTarTable <- flextable::qflextable(fullTarTable)
fullTarTable <- flextable::delete_part(fullTarTable, part = "header")
fullTarTable <- flextable::fontsize(fullTarTable, part = "all", size = 6)
fullTarTable <- flextable::merge_v(fullTarTable, j = 1:3, part = "body")
fullTarTable <- flextable::merge_h(fullTarTable, i = 1, part = "body")
border <- officer::fp_border(color = "black", width = 1)
fullTarTable <- flextable::border_inner(fullTarTable, border = border, part = "all")
fullTarTable <- flextable::border_outer(fullTarTable, border = border, part = "all")
fullTarTable <- flextable::align(fullTarTable, j = 1:3, align = "left", part = "all")
fullTarTable <- flextable::align(fullTarTable, i = 1:2, align = "left", part = "all")
fullTarTable <- flextable::autofit(fullTarTable, add_w = 0.1, add_h = 0.1)
fullTarTable <- flextable::padding(fullTarTable, padding = 0, part = "all")

doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section3a, style = "heading 1") %>%
  flextable::body_add_flextable(fullTarTable) %>%
  officer::body_add_fpar(section3b, style = "heading 1") %>%
  flextable::body_add_flextable(crudeIrTable) %>%
  print(target = file.path(reportFolder, "crudeIrTable.docx"))

write.csv(attritionResults, file.path(reportFolder, "crudeIrAttrition.csv"), row.names = FALSE)

# After matching IRs ----------------------------------------------------------

section4 <- "After matching incidence rates"
section4 <- officer::fpar(officer::ftext(section4, prop = headingFormat))

eventTables <- list()
eventTableTitles <- list()

header1 <- c(blank, blank, blank, "Patients", blank, "TAR", blank, "Events", blank, "IR", blank, blank)
header2 <- c("Indication", "T vs C", "Database", rep(c("T", "C"), 4), "MDRR")

for (analysisId in c(1, 8, 2, 3, 9, 4)) { # analysisId <- 8
  if (analysisId == 8) { # restrict to primary comparisons for short gap follow-up and ITT follow-up
    titleSuffix <- " (30d)"
    analysisId <- 1
    tarCohortIds <- sensitivityTarCohortIds
  } else if (analysisId == 9) {
    titleSuffix <- " (30d)"
    analysisId <- 3
    tarCohortIds <- sensitivityTarCohortIds
  } else {
    titleSuffix <- ""
    tarCohortIds <- primaryTarCohortIds
  }
  analysisName <- cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == analysisId]
  analysisName <- paste0(analysisName, titleSuffix)
  
  mainResults <- getMainResults(targetIds = tarCohortIds, 
                                comparatorIds = tarCohortIds, 
                                outcomeIds = outcomeId, 
                                databaseIds = databaseIds,
                                analysisIds = analysisId)
  mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
  mainResults <- mainResults[!(mainResults$databaseId == "Meta-analysis" & mainResults$sources == "Optum"), ]
  irTable <- prepareReportIrTable(mainResults, exposureOfInterest)
  irTable <- reorderTable(irTable, indicationLabels)
  irTable <- rbind(header1, header2, irTable)
  irTable <- createFlextable(irTable)
  title <- analysisName
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  eventTables[[length(eventTables) + 1]] <- irTable
  eventTableTitles[[length(eventTableTitles) + 1]] <- title 
}
eventTablePairs <- list(eventTableTitles, eventTables)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section4, style = "heading 1")
for(i in 1:length(eventTables)) { #i=1
  doc <- doc %>% 
    officer::body_add_fpar(eventTablePairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(eventTablePairs[[2]][[i]]) %>%
    officer::body_add_break()
}
print(doc, target = file.path(reportFolder, "irTables.docx"))


# Primary comparison diagnostics -----------------------------------------------

primaryFolder <- file.path(reportFolder, "primaryFolder")
  if (!file.exists(primaryFolder)) {
    dir.create(primaryFolder)
}

section5 <- "Primary analysis dianosticis"
section5 <- officer::fpar(officer::ftext(section5, prop = headingFormat))

# restrict to primary comparisons for one analysisId
tcds <- comparisonSummary[comparisonSummary$targetId %in% primaryTarCohortIds & comparisonSummary$comparatorId %in% primaryTarCohortIds, ]
primaryAnalysisId <- 1

primaryTitles <- list()
primaryFileNames <- list()

for (indicationGroup in indicationGroups) { # indicationGroup <- indicationGroups[[1]]
  indication <- indicationGroup$indication
  indicationCohortIds <- indicationGroup$cohortIds
  tcs <- unique(tcds[tcds$targetId %in% indicationCohortIds & tcds$comparatorId %in% indicationCohortIds, c("targetId", "comparatorId")])
  
  for (i in 1:nrow(tcs)) { # i = 1
    targetId <- tcs$targetId[i]
    targetLabel <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == targetId]
    targetLabel <- capitalize(sub(".*? ", "", targetLabel))
    comparatorId <- tcs$comparatorId[i]
    comparatorLabel <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparatorId]
    comparatorLabel <- capitalize(sub(".*? ", "", comparatorLabel))
    
    fileName <- file.path(primaryFolder, sprintf("primary_diagnostics_%s_%s_%s.png", indication, targetLabel, comparatorLabel))
    fileName <- sub("THR/TKR", "THRTKR", fileName)
    primaryFileNames[[length(primaryFileNames) + 1]] <- fileName
    
    primaryTitle <- sprintf("%s vs %s", targetLabel, comparatorLabel)
    primaryTitle <- officer::fpar(officer::ftext(primaryTitle, prop = titleFormat))
    primaryTitles[[length(primaryTitles) + 1]] <- primaryTitle 
    
    if (!file.exists(fileName)) {
      psPlots <- list()
      balPlots <- list()
      calPlots <- list()
      
      databases <- databaseIds[databaseIds != "Meta-analysis"]
      for (databaseId in databases) { # databaseId <- "CCAE"
  
        ps <- getPs(connection, targetId, comparatorId, primaryAnalysisId, databaseId)
        if (!is.null(ps)) {
          tcSizes <- getAttrition(connection, targetId, comparatorId, outcomeId, primaryAnalysisId, databaseId)
          targetSize <- tcSizes$targetPersons[tcSizes$description == "Have at least 1 days at risk"]
          comparatorSize <- tcSizes$comparatorPersons[tcSizes$description == "Have at least 1 days at risk"]
          psPlot <- plotReportPs(ps, targetSize = targetSize, comparatorSize = comparatorSize)
        } else {
          psPlot <- blankPlot
        }
        psPlots[[length(psPlots) + 1]] <- psPlot
  
        bal <- getCovariateBalance(connection, targetId, comparatorId, databaseId, primaryAnalysisId, outcomeId)
        if (!is.null(bal)) {
          balPlot <- plotReportCovariateBalanceScatterPlot(bal)
        } else {
          balPlot <- blankPlot
        }
        balPlots[[length(balPlots) + 1]] <- balPlot
        
        controlResults <- getControlResults(connection = connection,
                                            targetId = targetId,
                                            comparatorId = comparatorId,
                                            analysisId = primaryAnalysisId,
                                            databaseId = databaseId)
        if (nrow(controlResults) > 0) {
          controlEstimates <- controlResults[controlResults$effectSize == 1, ]
          calPlot <- plotReportScatter(controlEstimates)
        } else {
          calPlot <- blankPlot
        }
        calPlots[[length(calPlots) + 1]] <- calPlot
      }
      col0 <- grid::textGrob("")
      col1 <- grid::textGrob("PS distribution", gp = grid::gpar(fontsize = 16))
      col2 <- grid::textGrob("Covariate balanace", gp = grid::gpar(fontsize = 16))
      col3 <- grid::textGrob("Empirical null distribution", gp = grid::gpar(fontsize = 16))
      row1 <- grid::textGrob(databaseIds[1], rot = 90, gp = grid::gpar(fontsize = 16))
      row2 <- grid::textGrob(databaseIds[2], rot = 90, gp = grid::gpar(fontsize = 16))
      row3 <- grid::textGrob(databaseIds[3], rot = 90, gp = grid::gpar(fontsize = 16))
      row4 <- grid::textGrob(databaseIds[4], rot = 90, gp = grid::gpar(fontsize = 16))
      plotGrid <- gridExtra::grid.arrange(col0, col1, col2, col3,
                                          row1, psPlots[[1]], balPlots[[1]], calPlots[[1]],
                                          row2, psPlots[[2]], balPlots[[2]], calPlots[[2]],
                                          row3, psPlots[[3]], balPlots[[3]], calPlots[[3]],
                                          row4, psPlots[[4]], balPlots[[4]], calPlots[[4]],
                                          nrow = 5,
                                          heights = c(0.25, 4, 4, 4, 4),
                                          widths = c(0.25, 4, 4, 4))
      ggplot2::ggsave(fileName, plotGrid, width = 12.5, height = 12.5, dpi = 400)
    }
  }
}
primaryPlotPairs <- list(primaryTitles, primaryFileNames)
doc <- officer::read_docx() %>%
  officer::body_add_fpar(section5, style = "heading 1")
for(i in 1:length(primaryFileNames)) { #i=1
  if (i %in% c(1, 7, 13)) {
    doc <- doc %>%
      officer::body_add_fpar(indicationTitles[[i]], style = "heading 2") %>%
      officer::body_add_fpar(primaryPlotPairs[[1]][[i]], style = "heading 3") %>%
      officer::body_add_img(primaryPlotPairs[[2]][[i]], width = 6, height = 6) %>%
      officer::body_add_break()
  } else {
    doc <- doc %>%
      officer::body_add_fpar(primaryPlotPairs[[1]][[i]], style = "heading 3") %>%
      officer::body_add_img(primaryPlotPairs[[2]][[i]], width = 6, height = 6) %>%
      officer::body_add_break()
  }
}
print(doc, target = file.path(reportFolder, "primaryDiagnosticPlots.docx"))


# HR forest plots long; one comparisons, all analyses---------------------------

hrFolder <- file.path(reportFolder, "hrFolder")
if (!file.exists(hrFolder)) {
  dir.create(hrFolder)
}

section6 <- "Calibrated hazard ratios"
section6 <- officer::fpar(officer::ftext(section6, prop = headingFormat))

hrTitles <- list()
hrFileNames <- list()

for (indicationGroup in indicationGroups[1:2]) { # indicationGroup <- indicationGroups[[1]] # drop THR/TKR from forest plots
  indication <- indicationGroup$indication
  indicationCohortIds <- indicationGroup$cohortIds
  indicationComparisons <- comparisonSummary[comparisonSummary$targetId %in% indicationCohortIds & comparisonSummary$comparatorId %in% indicationCohortIds, ]

  comparisonCohortIds <- unique(indicationComparisons[, c("targetId", "comparatorId")])
  comparisonCohortIds <- cbind(comparisonCohortIds[1:6, ], comparisonCohortIds[7:12, ])

  for (psStrategy in c("1:1", "1:100")) { # psStrategy = "1:100"
    if (psStrategy == "1:1") {
      analysisIds <- c(1, 2)
    } else {
      analysisIds <- c(3, 4)
    }
    for (i in 1:nrow(comparisonCohortIds)) { # i = 4
      targetIds <- as.numeric(comparisonCohortIds[i, c(1, 3)])
      comparatorIds <- as.numeric(comparisonCohortIds[i, c(2, 4)])
      mainResults <- getMainResults(targetIds = targetIds,
                                    comparatorIds = comparatorIds,
                                    outcomeIds = outcomeId,
                                    databaseIds = databaseIds,
                                    analysisIds = analysisIds)
      mainResults <- merge(mainResults, cohortMethodAnalysis)
      mainResults$tar <- gsub(",.*","", mainResults$description)
      mainResults$tar[mainResults$targetId %in% sensitivityTarCohortIds] <- paste(mainResults$tar[mainResults$targetId %in% sensitivityTarCohortIds], "(30d)")
      mainResults$dbOrder <- match(mainResults$databaseId, databaseIds)
      mainResults <- mainResults[order(mainResults$analysisId, mainResults$targetId, mainResults$dbOrder), ]
      
      if (!all(is.na(mainResults$calibratedRr))) {
        forestPlot <- plotReportForest(mainResults)
        targetName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == targetIds[1]]
        targetName <- capitalize(sub(".*? ", "", targetName))
        comparatorName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparatorIds[1]]
        comparatorName <- capitalize(sub(".*? ", "", comparatorName))
        fileName <- file.path(hrFolder, sprintf("hr_plots_%s_%s_%s_%s.png", indication, tolower(targetName), tolower(comparatorName), paste(analysisIds, collapse = "_")))
        ggplot2::ggsave(fileName, forestPlot, width = 8.5, height = 5, dpi = 400)
        hrFileNames[[length(hrFileNames) + 1]] <- fileName
        title <- paste0(targetName, " vs ", comparatorName, ", ", psStrategy, " PS match")
        title <- officer::fpar(officer::ftext(title, prop = titleFormat))
        hrTitles[[length(hrTitles) + 1]] <- title
      }
    }
  }
}

hrPlotPairs <- list(hrTitles, hrFileNames)
doc <- officer::read_docx() %>%
  officer::body_add_fpar(section6, style = "heading 1")
for(i in 1:length(hrFileNames)) { #i=1
  if (i %in% c(1, 13)) {
    if (i == 13) {
      indicationTitle <- indicationTitles[[7]]
    } else {
      indicationTitle <- indicationTitles[[i]]
    }
    doc <- doc %>%
      officer::body_add_fpar(indicationTitle, style = "heading 2") %>%
      officer::body_add_fpar(hrPlotPairs[[1]][[i]], style = "heading 3") %>%
      officer::body_add_img(hrPlotPairs[[2]][[i]], width = 6, height = 4) %>%
      officer::body_add_break()
  } else {
    doc <- doc %>%
      officer::body_add_fpar(hrPlotPairs[[1]][[i]], style = "heading 3") %>%
      officer::body_add_img(hrPlotPairs[[2]][[i]], width = 6, height = 4) %>%
      officer::body_add_break()
  }
}
print(doc, target = file.path(reportFolder, "hrPlots.docx"))


# HR table with p-value correction for multiple testing ------------------------

section7 <- "Calibrated hazard ratio tables"
section7 <- officer::fpar(officer::ftext(section7, prop = headingFormat))

hrTableTitles <- list()
hrTables <- list()

for (analysisId in c(1, 3, 8, 9, 2, 4)) { # analysisId <- 1
  if (analysisId == 8) { # restrict to primary comparisons for short gap follow-up and ITT follow-up
    fileNameAnalysisId <- 8
    titleSuffix <- " (30d)"
    analysisId <- 1
    tarCohortIds <- sensitivityTarCohortIds
  } else if (analysisId == 9) {
    fileNameAnalysisId <- 9
    titleSuffix <- " (30d)"
    analysisId <- 3
    tarCohortIds <- sensitivityTarCohortIds
  } else {
    fileNameAnalysisId <- analysisId
    titleSuffix <- ""
    tarCohortIds <- primaryTarCohortIds
  }
  analysisName <- cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == analysisId]
  analysisName <- paste0(analysisName, titleSuffix)
  mainResults <- getMainResults(targetIds = tarCohortIds,
                                comparatorIds = tarCohortIds,
                                outcomeIds = outcomeId,
                                databaseIds = "Meta-analysis",
                                analysisIds = analysisId)
  mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
  hrs <- createHrTable(mainResults, exposureOfInterest, comparisonSummary)
  
  hrsTable <- hrs
  hrsTable <- flextable::qflextable(hrsTable)
  hrsTable <- flextable::fontsize(hrsTable, part = "all", size = 6)
  hrsTable <- flextable::merge_v(hrsTable, j = 1:2, part = "body")
  border <- officer::fp_border(color = "black", width = 1)
  hrsTable <- flextable::border_inner(hrsTable, border = border, part = "all")
  hrsTable <- flextable::border_outer(hrsTable, border = border, part = "all")
  hrsTable <- flextable::align(hrsTable, j = 1:2, align = "left", part = "body")
  hrsTable <- flextable::align(hrsTable, j = 3:7, align = "right", part = "body")
  hrsTable <- flextable::autofit(hrsTable, add_w = 0.1, add_h = 0.1)
  hrsTable <- flextable::padding(hrsTable, padding = 0, part = "all")
  
  title <- officer::fpar(officer::ftext(analysisName, prop = titleFormat))
  hrTableTitles[[length(hrTableTitles) + 1]] <- title
  hrTables[[length(hrTables) + 1]] <- hrsTable
}

hrTablePairs <- list(hrTableTitles, hrTables)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section7, style = "heading 1")
for(i in 1:length(hrTables)) { #i=1
  doc <- doc %>% 
    officer::body_add_fpar(hrTablePairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(hrTablePairs[[2]][[i]])
}
print(doc, target = file.path(reportFolder, "hrTables.docx"))


# Post-hoc hysterectomy characterization ---------------------------------------

section8 <- "SUB events with hysterectomy"
section8 <- officer::fpar(officer::ftext(section8, prop = headingFormat))

source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))
connection <- DatabaseConnector::connect(connectionDetails)
databaseRef <- data.frame(databaseId = c("CCAE", "MDCR", "MDCD", "Optum"),
                          cdmDatabaseSchema = c("CDM_IBM_CCAE_V1103.dbo", "CDM_IBM_MDCR_V1104.dbo", "CDM_IBM_MDCD_V1105.dbo", "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"),
                          cohortDatabaseSchema = rep("scratch.dbo", 4),
                          cohortTable = c("epi_680_ccae", "epi_680_mdcr", "epi_680_mdcd", "epi_680_optum"),
                          stringsAsFactors = FALSE)

hysterectomyConceptIds <- c(195599, 2101000,2101015,2101592,2101808,2101809,2101815,2109758,2110175,2110176,2110177,2110178,2110179,2110181,2110182,2110183,2110184,2110185,2110186,2110187,2110188,2110189,2110190,2110191,2110192,2110193,2110209,2110210,2110211,2110212,2110215,2110216,2110218,2110219,2110220,2110229,2110230,2110231,2110232,2110266,2110268,2110269,2110270,2110292,2110296,2110318,2721100,4021531,4032622,4032735,4072416,4127886,4138738,4146777,4262130,4294805,40480864,40488851)
covarSettings <- FeatureExtraction::createCovariateSettings(useProcedureOccurrenceMediumTerm = TRUE,
                                                            includedCovariateConceptIds = hysterectomyConceptIds,
                                                            endDays = 60,
                                                            mediumTermStartDays = 0,
                                                            addDescendantsToInclude = FALSE)
refs <- split(databaseRef, databaseRef$databaseId)
result <- lapply(refs, characterizeHysterectomy, primaryTarCohortIds, sensitivityTarCohortIds)
result <- do.call(rbind, result)
row.names(result) <- NULL

hystTables <- list()
hystTitles <- list()
for (analysisId in c(1, 8, 2, 3, 9, 4)) { # analysisId <- 1
  if (analysisId == 8) { # restrict to primary comparisons for short gap follow-up and ITT follow-up
    titleSuffix <- " (30d)"
    analysisId <- 1
    tarCohortIds <- sensitivityTarCohortIds
  } else if (analysisId == 9) {
    titleSuffix <- " (30d)"
    analysisId <- 3
    tarCohortIds <- sensitivityTarCohortIds
  } else {
    titleSuffix <- ""
    tarCohortIds <- primaryTarCohortIds
  }
  analysisName <- cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == analysisId]
  analysisName <- paste0(analysisName, titleSuffix)
  analysisResult <- result[result$analysisId == analysisId & result$targetId %in% tarCohortIds & result$comparatorId %in% tarCohortIds, ]
  analysisResult <- analysisResult[analysisResult$outcomes > 0, ]
  analysisResult <- merge(analysisResult, exposureOfInterest, by.x = "targetId", by.y = "exposureId", all.x = TRUE)
  names(analysisResult)[names(analysisResult) == "exposureName"] <- "targetName"
  analysisResult <- merge(analysisResult, exposureOfInterest, by.x = "comparatorId", by.y = "exposureId", all.x = TRUE)
  names(analysisResult)[names(analysisResult) == "exposureName"] <- "comparatorName"
  analysisResult$comparison <- paste(capitalize(analysisResult$targetName), capitalize(analysisResult$comparatorName), sep = " vs ")
  analysisResult <- reorderTable(analysisResult, indicationLabels)
  analysisResult <- analysisResult[, c("indication", "comparison", "databaseId", 
                                       "outcomes", "outcomesWithHysterectomy", "propHysterectomy",
                                       "tOutcomes", "tOutcomesWithHysterectomy", "tPropHysterectomy",
                                       "cOutcomes", "cOutcomesWithHysterectomy", "cPropHysterectomy")]
  header1 <- c(blank, blank, blank, "Overall outcomes", blank, blank, "Target outcomes", blank, blank, "Comparator outcomes", blank, blank)
  header2 <- c("Indication", "T vs C", "Database", rep(c("SUB events", "Hysterectomy", "Proportion"), 3))
  analysisResult <- rbind(header1, header2, analysisResult)
  analysisResult <- flextable::qflextable(analysisResult)
  analysisResult <- flextable::delete_part(analysisResult, part = "header")
  analysisResult <- flextable::fontsize(analysisResult, part = "all", size = 6)
  analysisResult <- flextable::merge_v(analysisResult, j = 1:2, part = "body")
  analysisResult <- flextable::merge_h(analysisResult, i = 1, part = "body")
  border <- officer::fp_border(color = "black", width = 1)
  analysisResult <- flextable::border_inner(analysisResult, border = border, part = "all")
  analysisResult <- flextable::border_outer(analysisResult, border = border, part = "all")
  analysisResult <- flextable::align(analysisResult, j = 1:3, align = "left", part = "all")
  analysisResult <- flextable::align(analysisResult, i = 1:3, align = "left", part = "all")
  analysisResult <- flextable::autofit(analysisResult, add_w = 0.1, add_h = 0.1)
  analysisResult <- flextable::padding(analysisResult, padding = 0, part = "all")
  title <- analysisName
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  hystTables[[length(hystTables) + 1]] <- analysisResult
  hystTitles[[length(hystTitles) + 1]] <- title 
}
hystTablePairs <- list(hystTitles, hystTables)
doc <- officer::read_docx() %>% 
  officer::body_add_fpar(section8, style = "heading 1")
for(i in 1:length(hystTables)) { #i=1
  doc <- doc %>% 
    officer::body_add_fpar(hystTablePairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(hystTablePairs[[2]][[i]]) %>%
    officer::body_add_break()
}
print(doc, target = file.path(reportFolder, "hystTables.docx"))


# build report -----------------------------------------------------------------
source("report/buildReport.R")

delta <- Sys.time() - start
paste("Creating document took", signif(delta, 3), attr(delta, "units"))
