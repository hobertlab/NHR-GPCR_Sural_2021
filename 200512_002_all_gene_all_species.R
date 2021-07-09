all.species.ids <- read.table("200512_001_Nematode species internal ids_BioMart.txt", header = TRUE)
#reads a table containing all species' bioMart internal ids in alphabetical order (of species, not of ids)

num.species <- length(all.species.ids$species_id)
#counts total number of species in analysis

dataout <- data.frame("Species_ID" = rep(NA, num.species), "total_genes" = rep(NA, num.species))
#creates an empty data frame - CHANGE COLUMN TITLE HERE)

for (i in 1:num.species) {genes.temp <- getBM(mart = mart, 
                                         filters = "species_id_1010",
                                         value = all.species.ids$species_id[i],
                                         attributes = "wbps_gene_id")
                          
                          dataout$Species_ID[i] <- gettext(all.species.ids$species_id[i])
                          #enters species ID in dataframe column
                          
                          dataout$total_genes[i] <- length(genes.temp$wbps_gene_id)
                          #enters number of genes in dataframe column - CHANGE COLUMN TITLE HERE
                          
                          rm(genes.temp)
}

head(dataout)
#prints the header of dataout

write.table(dataout, file = "200512_all_species_all_genes.txt", row.names = TRUE)
##CHANGE OUTPUT FILENAME HERE

rm(list = c("all.species.ids", "num.species", "dataout", "i"))