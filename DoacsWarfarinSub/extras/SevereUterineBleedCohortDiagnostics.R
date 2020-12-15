options(fftempdir = "S:/FFTemp")

source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))

mailSettings <- list(from = Sys.getenv("emailAddress"),
                     to = c(Sys.getenv("emailAddress")),
                     smtp = list(host.name = Sys.getenv("emailHost"), port = 25,
                                 user.name = Sys.getenv("emailAddress"),
                                 passwd = Sys.getenv("emailPassword"), ssl = FALSE),
                     authenticate = FALSE,
                     send = TRUE)

studyFolder <- "G:/StudyResults/epi_680/subDiagnostics"
if (!file.exists(studyFolder)) {
  dir.create(studyFolder, recursive = TRUE)
}
oracleTempSchema <- NULL
baseUrl <- Sys.getenv("baseUrl")

cohortSetReference  <- data.frame(atlasId = 14347,
                                  atlasName = "[680] Severe uterine bleed (surgical or transfusion management)",
                                  cohortId = 14347,
                                  name = "[680] Severe uterine bleed (surgical or transfusion management)")

# CCAE settings ----------------------------------------------------------------
db1 <- data.frame(databaseId = "CCAE",
                  cdmDatabaseSchema = "CDM_IBM_CCAE_V1103.dbo",
                  exportFolder = file.path(studyFolder, "CCAE"),
                  inclusionStatisticsFolder = file.path(studyFolder, "CCAE", "incStats"),
                  cohortDatabaseSchema = "scratch.dbo",
                  cohortTable = "epi_680_ccae",
                  stringsAsFactors = FALSE)

# Optum DOD settings -----------------------------------------------------------
db2 <- data.frame(databaseId = "Optum",
                  cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V1107.dbo",
                  exportFolder = file.path(studyFolder, "Optum"),
                  inclusionStatisticsFolder = file.path(studyFolder, "Optum", "incStats"),
                  cohortDatabaseSchema = "scratch.dbo",
                  cohortTable = "epi_680_optum",
                  stringsAsFactors = FALSE)

# MDCR settings ----------------------------------------------------------------
db3 <- data.frame(databaseId = "MDCR",
                  cdmDatabaseSchema = "CDM_IBM_MDCR_V1104.dbo",
                  exportFolder = file.path(studyFolder, "MDCR"),
                  inclusionStatisticsFolder = file.path(studyFolder, "MDCR", "incStats"),
                  cohortDatabaseSchema = "scratch.dbo",
                  cohortTable = "epi_680_mdcr",
                  stringsAsFactors = FALSE)

# MDCD settings ----------------------------------------------------------------
db4 <- data.frame(databaseId = "MDCD",
                  cdmDatabaseSchema = "CDM_IBM_MDCD_V1105.dbo",
                  exportFolder = file.path(studyFolder, "MDCD"),
                  inclusionStatisticsFolder = file.path(studyFolder, "MDCD", "incStats"),
                  cohortDatabaseSchema = "scratch.dbo",
                  cohortTable = "epi_680_mdcd",
                  stringsAsFactors = FALSE)

# database reference -----------------------------------------------------------
databaseRef <- rbind(db1, db2, db3, db4) 

for (i in 1:nrow(databaseRef)) {
  databaseId <- databaseRef$databaseId[i]
  cdmDatabaseSchema <- databaseRef$cdmDatabaseSchema[i]
  exportFolder <- databaseRef$exportFolder[i]
  inclusionStatisticsFolder <- databaseRef$inclusionStatisticsFolder[i]
  cohortDatabaseSchema <- databaseRef$cohortDatabaseSchema[i]
  cohortTable <- databaseRef$cohortTable[i]
  
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  if (!file.exists(inclusionStatisticsFolder)) {
    dir.create(inclusionStatisticsFolder, recursive = TRUE)
  }
  
  OhdsiRTools::runAndNotify(expression = {
    CohortDiagnostics::runCohortDiagnostics(baseUrl = baseUrl,
                                            cohortSetReference = cohortSetReference,
                                            connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = oracleTempSchema,
                                            cohortDatabaseSchema = cohortDatabaseSchema,
                                            cohortTable = cohortTable,
                                            inclusionStatisticsFolder = NULL,
                                            exportFolder = exportFolder,
                                            databaseId = databaseId,
                                            runInclusionStatistics = FALSE,
                                            runIncludedSourceConcepts = TRUE,
                                            runOrphanConcepts = TRUE,
                                            runTimeDistributions = TRUE,
                                            runBreakdownIndexEvents = TRUE,
                                            runIncidenceRate = TRUE,
                                            runCohortOverlap = FALSE,
                                            runCohortCharacterization = TRUE,
                                            minCellCount = 5)
  }, mailSettings = mailSettings, label = paste0("Severe uterine bleed cohort diagnostics: ", databaseId), stopOnWarning = FALSE)
}

shinyDataFolder <- file.path(studyFolder, "diagnosticsShinyData")
if (!file.exists(shinyDataFolder)) {
  dir.create(shinyDataFolder)
}
file.copy(from = c(file.path(studyFolder, "CCAE/Results_CCAE.zip"),
                   file.path(studyFolder, "Optum/Results_Optum.zip"),
                   file.path(studyFolder, "MDCR/Results_MDCR.zip"),
                   file.path(studyFolder, "MDCD/Results_MDCD.zip")),
          to = shinyDataFolder,
          overwrite = TRUE)

CohortDiagnostics::preMergeDiagnosticsFiles(shinyDataFolder)
CohortDiagnostics::launchDiagnosticsExplorer(shinyDataFolder)









