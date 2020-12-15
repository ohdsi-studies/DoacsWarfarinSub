source("S:/MiscCode/SetEnvironmentVariables.R")
baseUrl <- Sys.getenv("baseUrl")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))
connection <- DatabaseConnector::connect(connectionDetails)

databases <- data.frame(database = c(database = c("cdm_ibm_ccae_v1103.ohdsi_results",
                                                  "cdm_ibm_mdcr_v1104.ohdsi_results",
                                                  "cdm_ibm_mdcd_v1105.ohdsi_results",
                                                  "cdm_optum_extended_dod_v1107.ohdsi_results")),
                        name = c("IBM CCAE",
                                 "IBM MDCR",
                                 "IBM MDCD",
                                 "Optum SES"),
                        sourceKey = c("CDM_IBM_CCAE_V1103",
                                      "CDM_IBM_MDCR_V1104",
                                      "CDM_IBM_MDCD_V1105",
                                      "CDM_OPTUM_EXTENDED_DOD_V1107"),
                        stringsAsFactors = FALSE)

#cohortIds <- c(11400, 11404, 11412, 11403, 11407, 11415) # riva and warfarin only
cohortIds <- c(11400, # riva afib
               11401, # apix afib
               11402, # dabi afib
               11403, # warf afib
               11404, # riva vte
               11405, # apix vte
               11406, # dabi vte
               11407, # warf vte
               11412, # riva thr
               11413, # apix thr
               11414, # dabi thr
               11415) # warf thr

# build cohorts with varying gap days
for (i in 1:nrow(databases)) { # i <- 1
  
  cdmDatabaseSchema <- paste0(databases$sourceKey[i], ".dbo")
  cohortDatabaseSchema <- "scratch.dbo"
  cohortTable <- paste0("epi_680_follow_up_test_", databases$sourceKey[i])
  
  # createCohortTableSql <- "
  #   IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
  #   DROP TABLE @cohort_database_schema.@cohort_table;
  #   CREATE TABLE @cohort_database_schema.@cohort_table (
  #     cohort_definition_id INT,
  #     subject_id BIGINT,
  #     cohort_start_date DATE,
  #     cohort_end_date DATE);"
  # 
  # # Create study cohort table structure:
  # createCohortTableSql <- SqlRender::render(sql = createCohortTableSql, cohort_database_schema = cohortDatabaseSchema, cohort_table = cohortTable)
  # createCohortTableSql <- SqlRender::translate(sql = createCohortTableSql, targetDialect = attr(connection, "dbms"))
  # DatabaseConnector::executeSql(connection = connection, sql = createCohortTableSql, progressBar = FALSE, reportOverallTime = FALSE)
  
  for (cohortId in cohortIds) { # cohortId = 11400
    for (gapDays in c(3, 7, 15, 30)) { # gapDays <- 3
      
      checkSql <- "select count(*) from @cohort_database_schema.@cohort_table where cohort_definition_id = @cohort_definition_id"
      checkSql <- SqlRender::render(sql = checkSql,
                                    cohort_database_schema = cohortDatabaseSchema,
                                    cohort_table = cohortTable,
                                    cohort_definition_id = cohortId * 100 + gapDays)
      checkSql <- SqlRender::translate(sql = checkSql, targetDialect = attr(connection, "dbms"))
      checkRows <- DatabaseConnector::querySql(connection = connection, sql = checkSql)
      checkRows <- as.numeric(checkRows)
      
      if (checkRows == 0) {
        
        buildCohortSql <- ROhdsiWebApi::getCohortDefinitionSql(baseUrl = baseUrl, definitionId = cohortId, generateStats = FALSE)
        if (gapDays != 3) {
          buildCohortSql <- gsub(pattern = "DATEADD(day,-1 * 3,EVENT_DATE)", 
                                 replacement = paste0("DATEADD(day,-1 * ", gapDays, ",EVENT_DATE)"), 
                                 fixed = TRUE, 
                                 x = buildCohortSql)
          buildCohortSql <- gsub(pattern = "DATEADD(day,3,DRUG_EXPOSURE_END_DATE)", 
                                 replacement = paste0("DATEADD(day,", gapDays, ",DRUG_EXPOSURE_END_DATE)"), 
                                 fixed = TRUE, 
                                 x = buildCohortSql) 
        }
        buildCohortSql <- SqlRender::render(sql = buildCohortSql,
                                            cdm_database_schema = cdmDatabaseSchema,
                                            vocabulary_database_schema = cdmDatabaseSchema,
                                            target_database_schema = cohortDatabaseSchema,
                                            target_cohort_table = cohortTable,
                                            target_cohort_id = cohortId * 100 + gapDays)
        buidCohortSql <- SqlRender::translate(sql = buildCohortSql, targetDialect = attr(connection, "dbms"))
        writeLines(paste0("Database: ", databases$sourceKey[i]))
        writeLines(paste0("CohortId: ", cohortId * 100 + gapDays))
        DatabaseConnector::executeSql(connection = connection, sql = buidCohortSql)
      }
    }
  }
}


evalSql <- "
  with
  percentiles as (
    select 
      distinct c.cohort_definition_id,
      percentile_disc(0.1) within group (order by datediff(d, c.cohort_start_date, c.cohort_end_date)) over (partition by c.cohort_definition_id) p10_ot,
      percentile_disc(0.25) within group (order by datediff(d, c.cohort_start_date, c.cohort_end_date)) over (partition by c.cohort_definition_id) p25_ot,
      percentile_disc(0.5) within group (order by datediff(d, c.cohort_start_date, c.cohort_end_date)) over (partition by c.cohort_definition_id) p50_ot,
      percentile_disc(0.75) within group (order by datediff(d, c.cohort_start_date, c.cohort_end_date)) over (partition by c.cohort_definition_id) p75_ot,
      percentile_disc(0.9) within group (order by datediff(d, c.cohort_start_date, c.cohort_end_date)) over (partition by c.cohort_definition_id) p90_ot,
      percentile_disc(0.1) within group (order by datediff(d, c.cohort_start_date, op.observation_period_end_date)) over (partition by c.cohort_definition_id) p10_itt,
      percentile_disc(0.25) within group (order by datediff(d, c.cohort_start_date, op.observation_period_end_date)) over (partition by c.cohort_definition_id) p25_itt,
      percentile_disc(0.5) within group (order by datediff(d, c.cohort_start_date, op.observation_period_end_date)) over (partition by c.cohort_definition_id) p50_itt,
      percentile_disc(0.75) within group (order by datediff(d, c.cohort_start_date, op.observation_period_end_date)) over (partition by c.cohort_definition_id) p75_itt,
      percentile_disc(0.9) within group (order by datediff(d, c.cohort_start_date, op.observation_period_end_date)) over (partition by c.cohort_definition_id) p90_itt
    from @cohort_database_schema.@cohort_table c
    join @cdm_database_schema.observation_period op
      on c.subject_id = op.person_id
      and c.cohort_start_date >= op.observation_period_start_date
      and c.cohort_start_date <= op.observation_period_end_date
    where c.cohort_definition_id in (
      1140003, 1140007, 1140015, 1140030, 1140103, 1140107, 1140115, 1140130, 1140203, 1140207, 
      1140215, 1140230, 1140303, 1140307, 1140315, 1140330, 1140403, 1140407, 1140415, 1140430, 
      1140503, 1140507, 1140515, 1140530, 1140603, 1140607, 1140615, 1140630, 1140703, 1140707, 
      1140715, 1140730, 1141203, 1141207, 1141215, 1141230, 1141303, 1141307, 1141315, 1141330, 
      1141403, 1141407, 1141415, 1141430, 1141503, 1141507, 1141515, 1141530
    )
    --and datediff(d, c.cohort_start_date, c.cohort_end_date) >= 1
  ),
  stats as (
    select
      c.cohort_definition_id,
      count(c.subject_id) n,
      sum(datediff(d, c.cohort_start_date, c.cohort_end_date)) / 365.25 sum_ot_years,
      min(datediff(d, c.cohort_start_date, c.cohort_end_date)) min_ot,
      max(datediff(d, c.cohort_start_date, c.cohort_end_date)) max_ot,
      1.0 * avg(datediff(d, c.cohort_start_date, c.cohort_end_date)) avg_ot,
      1.0 * stdev(datediff(d, c.cohort_start_date, c.cohort_end_date)) stddev_ot,
      sum(datediff(d, c.cohort_start_date, op.observation_period_end_date)) / 365.25 sum_itt_years,
      min(datediff(d, c.cohort_start_date, op.observation_period_end_date)) min_itt,
      max(datediff(d, c.cohort_start_date, op.observation_period_end_date)) max_itt,
      1.0 * avg(datediff(d, c.cohort_start_date, op.observation_period_end_date)) avg_itt,
      1.0 * stdev(datediff(d, c.cohort_start_date, op.observation_period_end_date)) stddev_itt
    from @cohort_database_schema.@cohort_table c
    join @cdm_database_schema.observation_period op
      on c.subject_id = op.person_id
      and c.cohort_start_date >= op.observation_period_start_date
      and c.cohort_start_date <= op.observation_period_end_date
    where c.cohort_definition_id in (
      1140003, 1140007, 1140015, 1140030, 1140103, 1140107, 1140115, 1140130, 1140203, 1140207, 
      1140215, 1140230, 1140303, 1140307, 1140315, 1140330, 1140403, 1140407, 1140415, 1140430, 
      1140503, 1140507, 1140515, 1140530, 1140603, 1140607, 1140615, 1140630, 1140703, 1140707, 
      1140715, 1140730, 1141203, 1141207, 1141215, 1141230, 1141303, 1141307, 1141315, 1141330, 
      1141403, 1141407, 1141415, 1141430, 1141503, 1141507, 1141515, 1141530
    )
    --and datediff(d, c.cohort_start_date, c.cohort_end_date) >= 1
    group by
      c.cohort_definition_id
  )
  select
    s.cohort_definition_id,
    s.n,
    s.sum_ot_years,
    s.avg_ot,
    s.stddev_ot,
    s.min_ot,
    p.p10_ot,
    p.p25_ot,
    p.p50_ot,
    p.p75_ot,
    p.p90_ot,
    s.max_ot, 
    s.sum_itt_years,
    s.avg_itt,
    s.stddev_itt,
    s.min_itt,
    p.p10_itt,
    p.p25_itt,
    p.p50_itt,
    p.p75_itt,
    p.p90_itt,
    s.max_itt
  from stats s
  join percentiles p
    on s.cohort_definition_id = p.cohort_definition_id
  order by s.cohort_definition_id"

dbRows <- data.frame()
for (i in 1:nrow(databases)) { # i = 1
  cohortDatabaseSchema <- "scratch.dbo"
  cohortTable <- paste0("epi_680_follow_up_test_", databases$sourceKey[i])
  cdmDatabaseSchema = paste0(databases$sourceKey[i], ".dbo")
  databaseName <- databases$name[i]
  renderedEvalSql <- SqlRender::render(sql = evalSql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   cdm_database_schema = cdmDatabaseSchema)
  dbRow <- DatabaseConnector::querySql(connection, renderedEvalSql)
  dbRow <- cbind(databaseName, dbRow)
  dbRows <- rbind(dbRows, dbRow)
}
followUpTimes <- dbRows


newCohortIds <- merge(cohortIds, c(3, 7, 15, 30))
newCohortIds$newCohortId <- newCohortIds$x * 100 + newCohortIds$y
newCohortIds$newCohortName <- unlist(lapply(X = newCohortIds$x, ROhdsiWebApi::getCohortDefinitionName, baseUrl = baseUrl, formatName = TRUE))
newCohortIds$newCohortName <- paste0(newCohortIds$newCohortName, ": ", newCohortIds$y, " follow-up gap days")
followUpTimes <- merge(newCohortIds[, c("newCohortId", "newCohortName")], followUpTimes, by.x = "newCohortId", by.y = "COHORT_DEFINITION_ID")
followUpTimes$dbOrder <- match(followUpTimes$databaseName, databases$name)
followUpTimes <- followUpTimes[order(followUpTimes$dbOrder, followUpTimes$newCohortId), ]
followUpTimes$dbOrder <- NULL
write.csv(followUpTimes, "S:/Git/Bitbucket/epi_680/Protocol/Tables_Annex/Annex_D_follow_up_time_distribution_3_7_15_30_day_gaps.csv", row.names = FALSE)
