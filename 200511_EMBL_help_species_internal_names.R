library(biomaRt)
mart <- useMart("parasite_mart", dataset="wbps_gene", host="https://parasite.wormbase.org", port=443)

retrieve_ids <- function(clade){
  ids <- getBM(mart=mart, filters=c("nematode_clade_1010"), values=clade, attributes=c("species_id_key","production_name_1010"), uniqueRows=TRUE)
  return(ids)}

clades<-c("I","III","IV","V","C")

all_ids<-lapply(clades,retrieve_ids)
#applies the function 'retrieve_ids' to all elements of the vector 'clades'

all_ids<-(do.call(rbind, all_ids))

write.table(all_ids, file = "200512_Species_internal_id_export.txt", row.names = TRUE)