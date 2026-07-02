# PRE-PROCESSING 2: Network data
## Do not run. lead_entities_1.csv and lead_entities_2.csv not provided.

library(igraph)
library(dplyr)

#Load data
dlead1 <- read.csv2("lead_entities_1.csv")
dlead2 <- read.csv2("lead_entities_2.csv")
dlead <- rbind(dlead1, dlead2)

# Clean up
dlead <- dlead %>%
  mutate(sector = recode(sector, "med" = "media"))

dlead <- dlead %>%
  mutate(sector = recode(sector, "ind " = "ind"))

dlead <- dlead[nchar(dlead$doc_id)>0,]

dleadmiss <- dlead[nchar(dlead$sector)==0,]

# Make vertices dataframe
lead_vertices <- unique(dlead[c("doc_id", "year")])
names(lead_vertices)[1] <- "vname"
lead_vertices$type <- TRUE

lead_vertices2 <- unique(dlead[c("sector")])
names(lead_vertices2)[1] <- "vname"
lead_vertices2$year <- NA
lead_vertices2$type <- FALSE

lead_vertices <- rbind(lead_vertices, lead_vertices2)

rm(dleadmiss)

# Create sectors
lead_vertices$state <- NA
lead_vertices[lead_vertices$type==FALSE,]$state <- 
  lead_vertices[lead_vertices$type==FALSE,]$vname %in%
  c("fed_gov", "state_gov", "fed_gov", "state_gov", "state_ex", "fed_ex")

lead_vertices$us_gov <- NA
lead_vertices[lead_vertices$type==FALSE,]$us_gov <- 
  lead_vertices[lead_vertices$type==FALSE,]$vname %in%
  c("fed_gov", "state_gov")

lead_vertices$us_pa <- NA
lead_vertices[lead_vertices$type==FALSE,]$us_pa <- 
  lead_vertices[lead_vertices$type==FALSE,]$vname %in%
  c("fed_pa", "state_pa")

lead_vertices$corp <- NA
lead_vertices[lead_vertices$type==FALSE,]$corp <- 
  lead_vertices[lead_vertices$type==FALSE,]$vname %in%
  c("ind", "ind_soc")

lead_vertices$gov_ex <- NA
lead_vertices[lead_vertices$type==FALSE,]$gov_ex <- 
  lead_vertices[lead_vertices$type==FALSE,]$vname %in%
  c("state_ex", "fed_ex")

tail(lead_vertices, 30)

# Create edges dataframe
lead_edges <- aggregate(dlead["doc_id"], dlead[c("doc_id", "sector")],
                          length)
names(lead_edges)[3] <- "weight"

# Create graph object
glead <- graph_from_data_frame(lead_edges, vertices = lead_vertices, directed=FALSE)



