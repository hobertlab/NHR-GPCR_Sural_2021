hypdiver <- read.csv("ref_210427_Lee Andersen_2021_Nature Ecol Evol_hyperdiverg_chr_location.csv", header = TRUE)
##reads the raw file that has the genomic coordinates of all the hyperdivergent regions on C. elegans chromosomes

gene.loc <- read.csv("210429_004_Ce_insulins_Chr posit.csv", header = TRUE)
##gene.loc <- read.csv("210429_003_Ce_GPCRs_Chr posit.csv", header = TRUE)
##gene.loc <- read.csv("210427_Ce_NHRs_Chr posit.csv", header = TRUE)
##reads a file with names and chromosomal coordinates of all genes of a gene family

gene.loc$numregion <- rep(0, nrow(gene.loc))
##adds a new column to the data frame gene.loc with a string of zero values that will be replaced with no. of hyperdivergent regions
##the gene spans

gene.loc$spanregion <- rep(0, nrow(gene.loc))
##adds a new column to the data frame gene.loc with a string of zero values that will be replaced with no. of bases of the gene
##that resides within hyperdivergent regions

## "i" follows genes
for (i in 1:nrow(gene.loc)){
  
  ## "j" follows hyperdivergent regions
  for (j in 1:nrow(hypdiver)){
   
    ##type A - gene lies within hyperdivergent region
     if ((gettext(hypdiver$Chromosome[j]) == gettext(gene.loc$Chromosome[i])) &
        (hypdiver$Start[j] <= gene.loc$Gene_start[i]) &
        (hypdiver$End[j] >= gene.loc$Gene_end[i])) {
       ##add one to numregion
       gene.loc$numregion[i] <- gene.loc$numregion[i] + 1
       ##enter location of overlap
       gene.loc$spanregion[i] <- gene.loc$spanregion[i] + gene.loc$Gene_end[i] - gene.loc$Gene_start[i]
     }
    
    ##type B - gene’s start lies in a hyperdivergent region, but gene’s end lies outside that hyperdivergent region
    if ((gettext(hypdiver$Chromosome[j]) == gettext(gene.loc$Chromosome[i])) &
        (hypdiver$Start[j] <= gene.loc$Gene_start[i]) &  
        (hypdiver$End[j] < gene.loc$Gene_end[i]) &
        (hypdiver$End[j] > gene.loc$Gene_start[i])) {
         ##add one to numregion
         gene.loc$numregion[i] <- gene.loc$numregion[i] + 1
         ##enter location of overlap
         gene.loc$spanregion[i] <- gene.loc$spanregion[i] + hypdiver$End[j] - gene.loc$Gene_start[i]
    }
    
    ##type C - Gene’s start lies outside any hyperdivergent region, but gene’s end lies in a hyperdivergent region
    if ((gettext(hypdiver$Chromosome[j]) == gettext(gene.loc$Chromosome[i])) &
        (hypdiver$Start[j] > gene.loc$Gene_start[i]) &
        (hypdiver$Start[j] < gene.loc$Gene_end[i]) &
        (hypdiver$End[j] >= gene.loc$Gene_end[i])) {
        ##add one to numregion
      gene.loc$numregion[i] <- gene.loc$numregion[i] + 1
      ##enter location of overlap
      gene.loc$spanregion[i] <- gene.loc$spanregion[i] + gene.loc$Gene_end[i] - hypdiver$Start[j]
      }
    
    ##type D - Gene’s start and end span a hyperdivergent region, i.e., gene’s length is > hyperdivergent region
    if ((gettext(hypdiver$Chromosome[j]) == gettext(gene.loc$Chromosome[i])) &
        (hypdiver$Start[j] > gene.loc$Gene_start[i]) &
        (hypdiver$End[j] < gene.loc$Gene_end[i])) { 
      ##add one to numregion
      gene.loc$numregion[i] <- gene.loc$numregion[i] + 1
      ##enter location of overlap
      gene.loc$spanregion[i] <- gene.loc$spanregion[i] + hypdiver$End[j] - hypdiver$Start[j]
    }
  }
}

write.csv(gene.loc, file = "210429_011_typeABCD_insulin genes in hyperdivergent region.csv", row.names = FALSE)
##write.csv(gene.loc, file = "210429_008_typeABCD_gpcr genes in hyperdivergent region.csv", row.names = FALSE)
##write.csv(gene.loc, file = "210429_005_typeABCD_nhr genes in hyperdivergent region.csv", row.names = FALSE)
##outputs CSV file containing length of each gene within hyperdivergent regions

rm(list = c("hypdiver", "gene.loc", "i", "j"))