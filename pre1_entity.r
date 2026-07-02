# PRE-PROCESSING 1: Entity recognition
## Do not run. SQLite database of Toxic Docs is not shared with the public. 
## Visit toxicdocs.org to contact the database host

library("spacyr")
library(RSQLite)
con <- dbConnect(SQLite(), "documents.sqlite")

Lead <- dbGetQuery(con, "
   SELECT id as doc_id, body as text, year FROM documents
   WHERE Lead = 1 AND LENGTH(text)<=1000000
   ")

for(i in seq(46151, nrow(Lead), by=50)){
  path <- paste("parsedlead/lead", i, ".csv", sep="")
  print(path)
  chunk_results <- spacy_extract_entity(Lead[i:(i+49),], type="named")
  write.csv2(chunk_results, file=path)
}


flead <- dir("parsedlead", "csv", full.names = TRUE)
dlead <- rbindlist(lapply(flead, fread), use.names=FALSE)
dlead <- dlead[ent_type %in% c("ORG")] 
dlead$cleantext <- trimws(tolower(gsub("\\s+"," ", gsub("[^A-Za-z ]", "", dlead$text)))) 
dlead$cleantext <- gsub("^the ", "", dlead$cleantext)
dlead <- dlead[nchar(dlead$cleantext)>2]
dlead <- dlead[order(cleantext)]
for(i in seq(from=0, to=nrow(dlead), by=1000000-1)){
  fwrite(dlead[(i+1):(i+1000000-1)], sprintf("lead_entities_%d.csv",i))
}

dlead <- fread("lead_entities_0.csv")
termcount <- dlead[, .(doccount=length(unique(doc_id))), by=cleantext]
termcount <- termcount[doccount>2,]
dlead <- dlead[cleantext %in% termcount$cleantext,]
Lead <- data.table(Lead)
Lead <- Lead[,.(doc_id, year)]
dlead <- dlead[Lead, on=.(doc_id=doc_id), nomatch=NULL]
dlead <- na.omit(dlead, cols="year")
dlead <- fwrite(dlead, "lead_entities_1.csv")

dlead <- fread("lead_entities_999999.csv")
termcount <- dlead[, .(doccount=length(unique(doc_id))), by=cleantext]
termcount <- termcount[doccount>3,]
dlead <- dlead[cleantext %in% termcount$cleantext,]
Lead <- data.table(Lead)
Lead <- Lead[,.(doc_id, year)]
dlead <- dlead[Lead, on=.(doc_id=doc_id), nomatch=NULL]
dlead <- na.omit(dlead, cols="year")
dlead <- fwrite(dlead, "lead_entities_2.csv")







