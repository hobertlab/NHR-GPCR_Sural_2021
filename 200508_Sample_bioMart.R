##Install the biomaRt package for R version 4.0 or greater:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("biomaRt")

library(biomaRt)
##accesses bioMart package

mart <- useMart("parasite_mart", dataset = "wbps_gene", host = "https://parasite.wormbase.org", port = 443)
##connects to Wormbase Parasite bioMart package

listFilters(mart) ##List the available filters

listAttributes(mart) ##List the available attributes

genes <- getBM(mart = mart, 
               filters = c("species_id_1010", "only_caelegprjna13758_homologue"),
               value = list("scmansprjea36577", TRUE),
               attributes = c("wbps_gene_id", "caelegprjna13758_gene", "caelegprjna13758_gene_name"))
               ##get all the S. mansoni genes with a C. elegans orthologue
head(genes)
##prints the header of this large dataset

rm(genes)
##removes sample 'genes' list

##Install the biomaRt package for R version 3.4 or older:
##source("http://bioconductor.org/biocLite.R")
##biocLite("biomaRt")

##Install the biomaRt package for R version 3.5 or greater:
##if (!requireNamespace("BiocManager", quietly = TRUE))
  ##install.packages("BiocManager")
##BiocManager::install(version = "3.11")