# Global settings --------------------------------------------------------------

library(DoacsWarfarinSub)

options(fftempdir = "S:/FFTemp")
maxCores <- parallel::detectCores()
studyFolder <- "G:/StudyResults/epi_680"

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

# CCAE settings ----------------------------------------------------------------
databaseId <- "CCAE"
databaseName <- "CCAE"
databaseDescription <- "CCAE"
cdmDatabaseSchema <- "CDM_IBM_CCAE_V1103.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema = "scratch.dbo"
cohortTable = "epi_680_ccae"

# MDCR settings ----------------------------------------------------------------
databaseId <- "MDCR"
databaseName <- "MDCR"
databaseDescription <- "MDCR"
cdmDatabaseSchema <- "CDM_IBM_MDCR_V1104.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "epi_680_mdcr"

# MDCD settings ----------------------------------------------------------------
databaseId <- "MDCD"
databaseName <- "MDCD"
databaseDescription <- "MDCD"
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1105.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "epi_680_mdcd"

# Optum DOD settings -----------------------------------------------------------
databaseId <- "Optum"
databaseName <- "Optum"
databaseDescription <- "Optum DOD"
cdmDatabaseSchema <- "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "epi_680_optum"

# Run cohort diagnostics -------------------------------------------------------

# OhdsiRTools::runAndNotify(expression = {
#   runCohortDiagnostics(connectionDetails = connectionDetails,
#                        cdmDatabaseSchema = cdmDatabaseSchema,
#                        cohortDatabaseSchema = cohortDatabaseSchema,
#                        cohortTable = cohortTable,
#                        oracleTempSchema = oracleTempSchema,
#                        outputFolder = outputFolder,
#                        databaseId = databaseId,
#                        databaseName = databaseName,
#                        databaseDescription = databaseDescription,
#                        createCohorts = TRUE,
#                        minCellCount = 5)
# }, mailSettings = mailSettings, label = paste0("DoacsWarfarinSub cohort diagnostics: ", databaseId), stopOnWarning = FALSE)

# Run Study---------------------------------------------------------------------
OhdsiRTools::runAndNotify(expression = {
        execute(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortTable = cohortTable,
                oracleTempSchema = NULL,
                outputFolder = outputFolder,
                databaseId = databaseId,
                databaseName = databaseName,
                databaseDescription = databaseDescription,
                createCohorts = FALSE,
                synthesizePositiveControls = FALSE,
                runAnalyses = FALSE,
                runDiagnostics = FALSE,
                packageResults = TRUE,
                maxCores = maxCores)
}, mailSettings = mailSettings, label = paste0("DoacsWarfarinSub study execution: ", databaseId), stopOnWarning = FALSE)

resultsZipFile <- file.path(outputFolder, "exportFull", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)

# Meta-analysis ----------------------------------------------------------------
doMetaAnalysis(outputFolders = c(file.path(studyFolder, "CCAE"),
                                 file.path(studyFolder, "MDCR"),
                                 file.path(studyFolder, "MDCD"),
                                 file.path(studyFolder, "Optum")), 
               maOutputFolder = file.path(studyFolder, "MetaAnalysis"),
               maxCores = maxCores)

# copy export objects to one directory ------------------------------------------
fullShinyDataFolder <- file.path(studyFolder, "shinyDataAll")
if (!file.exists(fullShinyDataFolder)) {
  dir.create(fullShinyDataFolder)
}
file.copy(from = c(list.files(file.path(studyFolder, "CCAE", "shinyData"), full.names = TRUE),
                   list.files(file.path(studyFolder, "MDCR", "shinyData"), full.names = TRUE),
                   list.files(file.path(studyFolder, "MDCD", "shinyData"), full.names = TRUE),
                   list.files(file.path(studyFolder, "Optum", "shinyData"), full.names = TRUE),
                   list.files(file.path(studyFolder, "MetaAnalysis", "shinyData"), full.names = TRUE)),
          to = fullShinyDataFolder,
          overwrite = TRUE)



