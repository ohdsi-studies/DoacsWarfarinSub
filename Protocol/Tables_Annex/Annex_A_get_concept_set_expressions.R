source("S:/MiscCode/SetEnvironmentVariables.R")
OhdsiRTools::createConceptSetWorkbook(conceptSetIds = c(8424:8431, 8433, 8434, 
                                                        8439, 8442, 8443, 8452,
                                                        8456:8459, 8460, 8489,
                                                        8490, 8491, 8483, 8494), 
                                      workFolder = "S:/Git/Bitbucket/epi_680/Protocol",
                                      baseUrl = Sys.getenv("baseUrl"),
                                      included = TRUE,
                                      mapped = TRUE)