# PRE-PROCESSING 3: REM Data
## Yields sdtrem.csv. Do not run. Computationally intensive and glead not provided. 
## Use sdtrem.csv instead.

## Load packages
library(Rcpp)
library(rem)
library(survival)
library(parallel)
library(doParallel)
library(dplyr)

## Prepare rem data out of graph object
dt <- dlead
dt <- as_long_data_frame(glead)
dt$from_year <- as.numeric(dt$from_year)
dt <- dt[order(dt$from_year), ]
dt <- dt[dt$from_year >= 1924 & dt$from_year <= 2003, ]

# Create event sequence with year variable
dt <- eventSequence(datevar = dt$from_year, dateformat = "%Y",
                    data = dt, type = "continuous",
                    byTime = "yearly", returnData = TRUE,
                    sortData = TRUE)

# Create rem dataset
rdt <- createRemDataset(data = dt, 
                        sender = dt$to_name, 
                        target = dt$from_name, 
                        eventSequence = dt$event.seq.cont,
                        atEventTimesOnly = TRUE, 
                        untilEventOccurrs = TRUE,
                        returnInputData = TRUE)

rdtprotected <- rdt

# Divide up the results: counting process data = 1, original data = 2
dtrem <- rdt[[1]]
dt <- rdt[[2]]

# Merge event attribute variables back in
dtrem$type <- dt$type[match(dtrem$eventID, dt$eventID)]
dtrem$important <- dt$important[match(dtrem$eventID, dt$eventID)]
dtrem$weight <- NA
dtrem$weight <- dt$weight[match(dtrem$target, dt$from_name)]
dtrem$media <- NA
dtrem$media <- dt$media[match(dtrem$eventTime, dt$event.seq.cont)]

# Code sectors

## Public and private research institutions
sdtrem$science <- ifelse(sdtrem$sender %in% c("res_pri", "res_soc", "res_u", "dr"), 1, 0)
  sdtrem$res_pri <- ifelse(sdtrem$sender == "res_pri", 1, 0)
  sdtrem$res_soc <- ifelse(sdtrem$sender == "res_soc", 1, 0)
  sdtrem$res_u <- ifelse(sdtrem$sender == "res_u", 1, 0)
  sdtrem$dr <- ifelse(sdtrem$sender %in% c("ind_dr", "dr"), 1, 0)

## Regulatory expert institutions
sdtrem$regex <- ifelse(sdtrem$sender %in% c("int_ex", "fed_ex", "state_ex"), 1, 0)
  sdtrem$int_ex <- ifelse(sdtrem$sender == "int_ex", 1, 0)
  sdtrem$fed_ex <- ifelse(sdtrem$sender == "fed_ex", 1, 0)
  sdtrem$state_ex <- ifelse(sdtrem$sender == "state_ex", 1, 0)

## Corporate actors
sdtrem$corp <- ifelse(sdtrem$sender %in% c("ind_ex", "ind", "ind_soc"), 1, 0)
  sdtrem$ind_ex <- ifelse(sdtrem$sender == "ind_ex", 1, 0)
  sdtrem$ind <- ifelse(sdtrem$sender == "ind", 1, 0)
  sdtrem$ind_soc <- ifelse(sdtrem$sender == "ind_soc", 1, 0)

## Other
sdtrem$other <- ifelse(sdtrem$sender %in% c("union", "media", "civil_soc", "int_gov", "int_pa", "jud", "fed_pol", "state_pol"), 1, 0)  

## Proportion by sector
sdtrem$prop <- ifelse(sdtrem$science == 1, 0.24, 
                     ifelse(sdtrem$regex == 1, 0.05, 
                            ifelse(sdtrem$corp == 1, 0.46, 0.25)))
  
## Proportion by subsector
sdtrem$prop_sector <- ifelse(sdtrem$sender == "ind", 0.28, 
                                ifelse(sdtrem$sender == "ind_soc", 0.11,
                                ifelse(sdtrem$sender == "ind_ex", 0.07,
                                ifelse(sdtrem$sender == "fed_pa", 0.10,
                                ifelse(sdtrem$sender == "state_pa", 0.04,
                                ifelse(sdtrem$sender == "fed_ex", 0.03,
                                ifelse(sdtrem$sender == "state_ex", 0.01,
                                ifelse(sdtrem$sender == "int_ex", 0.01,
                                ifelse(sdtrem$sender == "fed_pol", 0.04,
                                ifelse(sdtrem$sender == "state_pol", 0.01,
                                ifelse(sdtrem$sender == "res_pri", 0.08,
                                ifelse(sdtrem$sender == "res_soc", 0.03,
                                ifelse(sdtrem$sender == "res_u", 0.10,
                                ifelse(sdtrem$sender == "dr", 0.02,
                                ifelse(sdtrem$sender == "union", 0.01,
                                ifelse(sdtrem$sender == "media", 0.03,
                                ifelse(sdtrem$sender == "civil_soc", 0.01,
                                ifelse(sdtrem$sender == "int_gov", 0.01,
                                ifelse(sdtrem$sender == "int_pa", 0.01,
                                ifelse(sdtrem$sender == "jud", 0.01, NA)
                                )))))))))))))))))))

# Predictor category
sdtrem <- sdtrem |>
  mutate(knowledge = case_when(
    science == 1 ~ "science",
    regex == 1   ~ "regex",
    TRUE         ~ "other"
  ))

# Covariates

## Sender outdegree
sdtrem <- sdtrem[order(sdtrem$eventTime), ]
sdtrem$outdegreehl5 <- degreeStat(data = sdtrem, 
                            time = sdtrem$eventTime,
                            degreevar = sdtrem$sender,
                            halflife = 5, # we can use 2, 5, 10, 20 etc.
                            #eventvar = sdtrem$eventDummy,
                            weight = sdtrem$weight,
                            variablename = "degree",
                            returnData = FALSE,
                            inParallel = TRUE,
                            cluster = makeCluster(12, type="FORK"))
sdtrem$outdegree_scale <- scale(sdtrem$outdegree)

# Fourcycle
cl <- makeCluster(12, type = "FORK")
sdtrem$four <- fourCycleStat(data = sdtrem, 
                          time = sdtrem$eventTime, 
                          sender = sdtrem$sender, 
                          target = sdtrem$target,
                          #eventvar = sdtrem$eventDummy,
                          halflife = 5,
                          weight = sdtrem$weight,
                          eventtypevar = NULL,
                          eventtypevalue = 'standard',
                          cluster = cl,
                          inParallel = TRUE,
                          showprogressbar = FALSE,
                          returnData = FALSE)
sdtrem$four_scale <- scale(sample_sdtrem$four)


# Inertia
sdtrem <- sdtrem[order(sdtrem$eventTime), ]
sdtrem$inertia_unscale <- inertiaStat(data = sdtrem, 
                       time = sdtrem$eventTime, 
                       sender = sdtrem$sender, 
                       target = sdtrem$target,
                       halflife = 5,
                       weight = sdtrem$weight,
                       eventtypevar = NULL,
                       eventtypevalue = "valuematch",
                       inParallel = TRUE,
                       cluster = makeCluster(2, type="FORK"))
sdtrem$inertia_scale <- scale(sdtrem$inertia)

## Media salience
lead_media <- read.csv("~/lead_media.csv")
lead_media$eventTime <- lead_media$X
sdtrem <- merge(sdtrem, lead_media, by = "eventTime")
sdtrem <- sdtrem %>% rename(news_count = Freq)
sdtrem$news_count <- as.numeric(sdtrem$news_count)

sdtrem <- sdtrem %>%
  group_by(eventID) %>%
  arrange(eventTime) %>%      
  mutate(lag_news_count = lag(news_count, n = 1)) %>%  
  ungroup()

sdtrem$s_lag_news_count <- scale(sdtrem$lag_news_count)

# Write to csv
write.csv(sdtrem, file = "sdtrem.csv")



