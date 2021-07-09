Interpro.gene.num <- function(interpro, title) {
  #Creates a function where the InterPro term(s) and title of output file needs to be input
  
  all.species.ids <- read.table("200512_001_Nematode species internal ids_BioMart.txt", header = TRUE)
  #reads a table containing all species' bioMart internal ids in alphabetical order (of species, not of ids)
  
  num.species <- length(all.species.ids$species_id)
  #counts total number of species in analysis
  
  dataout <- data.frame("Species_ID" = rep(NA, num.species), "num_genes" = rep(NA, num.species))
  #creates an empty data frame
  
  for (i in 1:num.species) {genes.temp <- getBM(mart = mart, 
                                                filters = c("species_id_1010", "interpro_id"),
                                                value = list(all.species.ids$species_id[i], interpro),
                                                attributes = "wbps_gene_id")
  ##get all genes containing the InterPro IDs in species
  
  dataout$Species_ID[i] <- gettext(all.species.ids$species_id[i])
  #enters species ID in dataframe column
  
  dataout$num_genes[i] <- length(genes.temp$wbps_gene_id)
  #enters number of genes in dataframe column - CHANGE COLUMN TITLE HERE
  
  rm(genes.temp)
  }
  
  head(dataout)
  #prints the header of dataout
  
  write.csv(dataout, file = title, row.names = FALSE)
  ##filename input at function level
  
  rm(list = c("all.species.ids", "num.species", "dataout", "i"))
}