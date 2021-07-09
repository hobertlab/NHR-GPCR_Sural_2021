Subset.MaZheng.data <- function(MaZheng.data.whole, gene.stable.ID.list, output.title){
  ##this function can extract data for any subset of genes from the MaZheng Table S4 Pi D H data raw file
  
  MaZheng.data.whole.temp <- read.csv(MaZheng.data.whole, header = TRUE, row.names = 1)
  ##reads whole MaZheng data file for all genes
  
  gene.stable.IDs.temp <- read.csv(gene.stable.ID.list, header = FALSE)
  ##reads Wormbase stable IDs for genes to be extracted
  
  MaZheng.sub.temp <- MaZheng.data.whole.temp[which(row.names(MaZheng.data.whole.temp) %in% gettext(gene.stable.IDs.temp$V1)), ]
  ##creates a subset of the MaZheng file that contains the Wormbase stable IDs as row names
  
  write.csv(MaZheng.sub.temp, file = output.title, row.names = TRUE)
  ##exports the subset data for the required gene family, BUT EXCLUDES the Wormbase Stable IDs
  
  rm(list = c("MaZheng.data.whole.temp", "gene.stable.IDs.temp", "MaZheng.sub.temp"))
  
}

##EXAMPLES
##Subset.MaZheng.data("ref_210607_Ma Zheng_2021_Table S4_each gene Pi D H.csv", "200603_006_Ce_NHR_stable IDs.csv", "210608_001_Ma Zheng_2021_Table S4_NHR genes Pi D H.csv")
##Subsets MaZheng Table S4 Pi D H data raw file for NHR genes