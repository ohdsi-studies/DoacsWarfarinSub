# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of DoacsWarfarinSub
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @export
doMetaAnalysis <- function(studyFolder,
                           outputFolders,
                           maOutputFolder,
                           maxCores) {
  
  ParallelLogger::logInfo("Performing meta-analysis")
  shinyDataFolder <- file.path(maOutputFolder, "shinyData")
  if (!file.exists(shinyDataFolder)) {
    dir.create(shinyDataFolder, recursive = TRUE)
  }
  
  # get main results
  loadResults <- function(outputFolder) {  # outputFolder <- outputFolders[13]
    database <- basename(outputFolder)
    file <- list.files(file.path(outputFolder, "shinyData"), pattern = sprintf("cohort_method_result_%s.rds", database), full.names = TRUE)
    result <- readRDS(file)
    colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
    ParallelLogger::logInfo("Loading ", file, " for meta-analysis")
    return(result)
  }
  allResults <- lapply(outputFolders, loadResults)
  allResults <- do.call(rbind, allResults)
  
  # drop cad/pad rows
  cadPadExposureIds <- c(11408, 11409, 11410, 11411, 13207, 13208, 13209, 13236)
  drops <- allResults$targetId %in% cadPadExposureIds | allResults$comparatorId %in% cadPadExposureIds # drop CAD/PAD rows
  allResults <- allResults[!drops, ]
  
  # drop redundant rows
  primaryTarCohortIds <- c(11400, 11403, 11401, 11402, 11404, 11407, 11405, 11406, 11412, 11415, 11413, 11414)
  sensitivityTarCohortIds <- c(13033, 13234, 13199, 13200, 13203, 13235, 13204, 13205, 13211, 13237, 13212, 13213)
  
  primaryTar <- allResults$targetId %in% primaryTarCohortIds & allResults$comparatorId %in% primaryTarCohortIds & allResults$analysisId %in% c(1, 3)
  sensitivityTar <- allResults$targetId %in% sensitivityTarCohortIds & allResults$comparatorId %in% sensitivityTarCohortIds & allResults$analysisId %in% c(1, 3)
  ittTar <- allResults$targetId %in% primaryTarCohortIds & allResults$comparatorId %in% primaryTarCohortIds & allResults$analysisId %in% c(2, 4)
  allResults <- allResults[primaryTar | sensitivityTar | ittTar, ] # this drops 1:1 SENS ITT amd 1:100 SENS ITT (since the same as 1:1 ITT and 1:100 ITT)
  
  # blind results that don't pass diagnostics
  blinds <-
    (allResults$targetId %in% c(11400, 13033) & allResults$comparatorId %in% c(11403, 13234) & allResults$databaseId == "MDCD") |  # NVAF: rivaroxaban vs NVAF: warfarin
    (allResults$targetId %in% c(11402, 13200) & allResults$comparatorId %in% c(11403, 13234) & allResults$databaseId == "MDCD") |  # NVAF: dabigatran  vs NVAF: warfarin
    (allResults$targetId %in% c(11400, 13033) & allResults$comparatorId %in% c(11402, 13200) & allResults$databaseId == "MDCD") |  # NVAF: rivaroxaban vs NVAF: dabigatran
    (allResults$targetId %in% c(11401, 13199) & allResults$comparatorId %in% c(11402, 13200) & allResults$databaseId == "MDCD") |  # NVAF: apixaban    vs NVAF: dabigatran
    
    (allResults$targetId %in% c(11405, 13204) & allResults$comparatorId %in% c(11407, 13235) & allResults$databaseId == "MDCR") |                                # VTE: apixaban    vs VTE: warfarin
    (allResults$targetId %in% c(11406, 13205) & allResults$comparatorId %in% c(11407, 13235) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum")) |  # VTE: dabigatran  vs VTE: warfarin 
    (allResults$targetId %in% c(11404, 13203) & allResults$comparatorId %in% c(11406, 13205) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum")) |  # VTE: rivaroxaban vs VTE: dabigatran
    (allResults$targetId %in% c(11405, 13204) & allResults$comparatorId %in% c(11406, 13205) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum")) |  # VTE: apixaban    vs VTE: dabigatran
    
    (allResults$targetId %in% c(11412, 13211) & allResults$comparatorId %in% c(11415, 13212) & allResults$databaseId == "MDCD") |                                # TKR/THR: rivaroxaban vs TKR/THR: warfarin
    (allResults$targetId %in% c(11413, 13212) & allResults$comparatorId %in% c(11415, 13237) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR")) |           # TKR/THR: apixaban    vs TKR/THR: warfarin
    (allResults$targetId %in% c(11414, 13213) & allResults$comparatorId %in% c(11415, 13237) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum")) |  # TKR/THR: dabigatran  vs TKR/THR: warfarin
    (allResults$targetId %in% c(11412, 13211) & allResults$comparatorId %in% c(11413, 13212) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR")) |           # TKR/THR: rivaroxaban vs TKR/THR: apixaban
    (allResults$targetId %in% c(11412, 13211) & allResults$comparatorId %in% c(11414, 13213) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum")) |  # TKR/THR: rivaroxaban vs TKR/THR: dabigatran
    (allResults$targetId %in% c(11413, 13212) & allResults$comparatorId %in% c(11414, 13213) & allResults$databaseId %in% c("CCAE", "MDCD", "MDCR", "Optum"))    # TKR/THR: apixaban    vs TKR/THR: dabigatran
  
  allResults$rr[blinds] <- NA
  allResults$ci95Ub[blinds] <- NA
  allResults$ci95Lb[blinds] <- NA
  allResults$logRr[blinds] <- NA
  allResults$seLogRr[blinds] <- NA
  allResults$p[blinds] <- NA
  allResults$calibratedRr[blinds] <- NA
  allResults$calibratedCi95Ub[blinds] <- NA
  allResults$calibratedCi95Lb[blinds] <- NA
  allResults$calibratedLogRr[blinds] <- NA
  allResults$calibratedSeLogRr[blinds] <- NA
  allResults$calibratedP[blinds] <- NA
  
  allControls <- list()
  timeAtRisks <- c("OnTreatment", "IntentToTreat") 
  for (timeAtRisk in timeAtRisks) {  # timeAtRisk <- "IntentToTreat"
    outputFoldersTar <- paste(outputFolders, timeAtRisk, sep = "/")
    controls <- lapply(outputFoldersTar, getAllControls)
    controls <- do.call(rbind, controls)
    controls <- controls[, c("targetId", "comparatorId", "outcomeId", "targetEffectSize")]
    allControls[[length(allControls) + 1]] <- controls
  }
  allControls <- do.call(rbind, allControls)
  allControls <- allControls[!duplicated(allControls), ]
  
  ncIds <- allControls$outcomeId[allControls$targetEffectSize == 1]
  allResults$type[allResults$outcomeId %in% ncIds] <- "Negative control"
  pcIds <- allControls$outcomeId[allControls$targetEffectSize != 1]
  allResults$type[allResults$outcomeId %in% pcIds] <- "Positive control"
  allResults$type[is.na(allResults$type)] <- "Outcome of interest"
  
  groups <- split(allResults, paste(allResults$targetId, allResults$comparatorId, allResults$analysisId), drop = TRUE)
  cluster <- ParallelLogger::makeCluster(min(maxCores, 12))
  results <- ParallelLogger::clusterApply(cluster, 
                                          groups,
                                          computeGroupMetaAnalysis,
                                          shinyDataFolder = shinyDataFolder,
                                          allControls = allControls)
  ParallelLogger::stopCluster(cluster)
  results <- do.call(rbind, results)
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  
  fileName <- file.path(maOutputFolder, "cohort_method_results_Meta-analysis.csv")
  write.csv(results, fileName, row.names = FALSE, na = "")
  fileName <- file.path(shinyDataFolder, "cohort_method_result_Meta-analysis.rds")
  results <- subset(results, select = -c(type, mdrr))
  saveRDS(results, fileName)
  
  database <- data.frame(database_id = "Meta-analysis",
                         database_name = "Meta-analysis",
                         description = "Meta-analysis",
                         is_meta_analysis = 1)
  fileName <- file.path(shinyDataFolder, "database_Meta-analysis.rds")
  saveRDS(database, fileName)
}

computeGroupMetaAnalysis <- function(group,
                                     shinyDataFolder,
                                     allControls) {
  
  # group <- groups[["11400 11401 3"]]
  analysisId <- group$analysisId[1]
  targetId <- group$targetId[1]
  comparatorId <- group$comparatorId[1]
  OhdsiRTools::logTrace("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis", analysisId)
  outcomeGroups <- split(group, group$outcomeId, drop = TRUE)
  outcomeGroupResults <- lapply(outcomeGroups, computeSingleMetaAnalysis)
  groupResults <- do.call(rbind, outcomeGroupResults)
  
  ncs <- groupResults[groupResults$type == "Negative control", ]
  ncs <- ncs[!is.na(ncs$seLogRr), ]
  if (nrow(ncs) > 5) {
    null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = groupResults$logRr,
                                                    seLogRr = groupResults$seLogRr)
    groupResults$calibratedP <- calibratedP$p
  } else {
    groupResults$calibratedP <- rep(NA, nrow(groupResults))
  }
  pcs <- groupResults[groupResults$type == "Positive control", ]
  pcs <- pcs[!is.na(pcs$seLogRr), ]
  if (nrow(pcs) > 5) {
    controls <- merge(groupResults, allControls)
    model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controls$logRr,
                                                           seLogRr = controls$seLogRr,
                                                           trueLogRr = log(controls$targetEffectSize),
                                                           estimateCovarianceMatrix = FALSE)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$logRr,
                                                                      seLogRr = groupResults$seLogRr,
                                                                      model = model)
    groupResults$calibratedRr <- exp(calibratedCi$logRr)
    groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    groupResults$calibratedLogRr <- calibratedCi$logRr
    groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
  } else {
    groupResults$calibratedRr <- rep(NA, nrow(groupResults))
    groupResults$calibratedCi95Lb <- rep(NA, nrow(groupResults))
    groupResults$calibratedCi95Ub <- rep(NA, nrow(groupResults))
    groupResults$calibratedLogRr <- rep(NA, nrow(groupResults))
    groupResults$calibratedSeLogRr <- rep(NA, nrow(groupResults))
  }
  return(groupResults)
}

computeSingleMetaAnalysis <- function(outcomeGroup) {
  # outcomeGroup <- outcomeGroups[[1]]
  
  # print(outcomeGroup$outcomeId[1])
  
  maRow <- outcomeGroup[1, ]
  outcomeGroup <- outcomeGroup[!is.na(outcomeGroup$seLogRr), ]
  outcomeGroup <- outcomeGroup[outcomeGroup$seLogRr < 50, ]
  if (nrow(outcomeGroup) == 0) {
    maRow$targetSubjects <- 0
    maRow$comparatorSubjects <- 0
    maRow$targetDays <- 0
    maRow$comparatorDays <- 0
    maRow$targetOutcomes <- 0
    maRow$comparatorOutcomes <- 0
    maRow$rr <- NA
    maRow$ci95Lb <- NA
    maRow$ci95Ub <- NA
    maRow$p <- NA
    maRow$logRr <- NA
    maRow$seLogRr <- NA
    maRow$i2 <- NA
  } else if (nrow(outcomeGroup) == 1) {
    maRow <- outcomeGroup[1, ]
    maRow$i2 <- 0
  } else {
    maRow$targetSubjects <- sumMinCellCount(outcomeGroup$targetSubjects)
    maRow$comparatorSubjects <- sumMinCellCount(outcomeGroup$comparatorSubjects)
    maRow$targetDays <- sum(outcomeGroup$targetDays)
    maRow$comparatorDays <- sum(outcomeGroup$comparatorDays)
    maRow$targetOutcomes <- sumMinCellCount(outcomeGroup$targetOutcomes)
    maRow$comparatorOutcomes <- sumMinCellCount(outcomeGroup$comparatorOutcomes)
    meta <- meta::metagen(outcomeGroup$logRr, outcomeGroup$seLogRr, sm = "RR", hakn = TRUE)
    s <- summary(meta)
    maRow$i2 <- s$I2$TE
    if (maRow$i2 < .40) {
      rnd <- s$random
      maRow$rr <- exp(rnd$TE)
      maRow$ci95Lb <- exp(rnd$lower)
      maRow$ci95Ub <- exp(rnd$upper)
      maRow$p <- rnd$p
      maRow$logRr <- rnd$TE
      maRow$seLogRr <- rnd$seTE
    } else {
      maRow$rr <- NA
      maRow$ci95Lb <- NA
      maRow$ci95Ub <- NA
      maRow$p <- NA
      maRow$logRr <- NA
      maRow$seLogRr <- NA
    }
  }
  if (is.na(maRow$logRr)) {
    maRow$mdrr <- NA
  } else {
    alpha <- 0.05
    power <- 0.8
    z1MinAlpha <- qnorm(1 - alpha/2)
    zBeta <- -qnorm(1 - power)
    pA <- maRow$targetSubjects / (maRow$targetSubjects + maRow$comparatorSubjects)
    pB <- 1 - pA
    totalEvents <- abs(maRow$targetOutcomes) + abs(maRow$comparatorOutcomes)
    maRow$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  }
  maRow$databaseId <- "Meta-analysis"
  maRow$sources <- paste(outcomeGroup$databaseId[order(outcomeGroup$databaseId)], collapse = ", ")
  return(maRow)
}

sumMinCellCount <- function(counts) {
  total <- sum(abs(counts))
  if (any(counts < 0)) {
    total <- -total
  }
  return(total)
}

getAllControls <- function(outputFolder) {
  allControlsFile <- file.path(outputFolder, "AllControls.csv")
  if (file.exists(allControlsFile)) {
    # Positive controls must have been synthesized. Include both positive and negative controls.
    allControls <- read.csv(allControlsFile)
  } else {
    # Include only negative controls
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "DoacsWarfarinSub")
    allControls <- read.csv(pathToCsv)
    allControls$oldOutcomeId <- allControls$outcomeId
    allControls$targetEffectSize <- rep(1, nrow(allControls))
  }
  return(allControls)
}