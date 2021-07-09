Subset.Cengen.data <- function(Cengen.data.whole, gene.stable.ID.list, output.title){
  ##this function can extract data for any subset of genes from the Cengen data raw file
  
  Cengen.data.whole.temp <- read.csv(Cengen.data.whole, header = TRUE, row.names = 1)
  ##reads whole Cengen data file for all genes
  
  gene.stable.IDs.temp <- read.csv(gene.stable.ID.list, header = FALSE)
  ##reads Wormbase stable IDs for genes to be extracted
  
  Cengen.sub.temp <- Cengen.data.whole.temp[which(row.names(Cengen.data.whole.temp) %in% gettext(gene.stable.IDs.temp$V1)), ]
  ##creates a subset of the Cengen file that contains the Wormbase stable IDs as row names
  
  write.csv(Cengen.sub.temp, file = output.title, row.names = FALSE)
  ##exports the subset data for the required gene family, BUT EXCLUDES the Wormbase Stable IDs
  
  rm(list = c("Cengen.data.whole.temp", "gene.stable.IDs.temp", "Cengen.sub.temp"))
  
}
