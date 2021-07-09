Jaccard.column.stats <- function(gene.family.1.binary, gene.family.2.binary, output.Jaccard.title, output.Stats.title) {
  ##function to find Jaccard similarity index b/w all members of two gene families and performs column statistics on the similarity values
  ##Number of columns (neuron classes) need to be identical in both input files
  
  gene.1.binary <- read.csv(gene.family.1.binary, header = TRUE, row.names = 1)
  ##reads Cengen binary file for 1st gene category and assigns first column as row.names
  
  gene.1.num <- nrow(gene.1.binary)
  ##identifies total number of genes of 1st category from number of rows
  
  gene.2.binary <- read.csv(gene.family.2.binary, header = TRUE, row.names = 1)
  ##reads Cengen binary file for 2nd gene category and assigns first column as row.names
  
  gene.2.num <- nrow(gene.2.binary)
  ##identifies total number of genes of 2nd category from number of rows
  
  Jaccard.values <- data.frame("Gene1" = rep(NA, gene.1.num*gene.2.num),
                               "Gene2" = rep(NA, gene.1.num*gene.2.num),
                               "Jaccard" = rep(NA, gene.1.num*gene.2.num))
  ##creates an empty data frame with gene names from 1st and 2nd category and their Jaccard similarity value as columns
  
  index <- 1
  ##index identifies which row to fill in output file
  
  ##'i' follows genes in 1st category 
  for (i in 1:gene.1.num){
    
    ##'j' follows genes in 2nd category
    for (j in 1:gene.2.num){
      
      Jaccard.values$Gene1[index] <- row.names(gene.1.binary)[i]
      ##inputs name of gene of 1st category
      
      Jaccard.values$Gene2[index] <- row.names(gene.2.binary)[j]
      ##inputs name of gene of 2nd category
      
      sums <- gene.1.binary[i, ] + gene.2.binary[j, ]
      ##adds the binary values for two genes - one from each category
      
      intersection = length(sums[sums==2])
      ##finds number of columns (neurons) that have a value of 1 for BOTH genes
      
      union = length(sums[sums==1]) + intersection
      ##finds number of columns (neurons) that have a value of 1 for ONLY ONE (out of the two) genes
      
      Jaccard.values$Jaccard[index] <- intersection/union
      ##enters the Jaccard value for both genes
      
      index <- index + 1
      ##moves to next row of output file
  
      rm(list = c("sums", "intersection", "union"))
    } 
  }
  
  write.csv(Jaccard.values, file = output.Jaccard.title, row.names = TRUE)
  ##outputs CSV file containing Jaccard values
  
  sim <- 1
  ##ENTER NUMBER OF SIMULATIONS HERE
  
  Jaccard.stats <- data.frame("Mean" = rep(NA, sim),
                               "Median" = rep(NA, sim),
                               "upto10th_percentile" = rep(NA, sim),
                               "upto20th_percentile" = rep(NA, sim),
                               "upto25th_percentile" = rep(NA, sim),
                               "upto30th_percentile" = rep(NA, sim),
                               "upto40th_percentile" = rep(NA, sim),
                               "upto60th_percentile" = rep(NA, sim),
                               "upto70th_percentile" = rep(NA, sim),
                               "upto75th_percentile" = rep(NA, sim),
                               "upto80th_percentile" = rep(NA, sim),
                               "upto90th_percentile" = rep(NA, sim),
                               "upto95th_percentile" = rep(NA, sim),
                               "upto99th_percentile" = rep(NA, sim))
  ##creates an empty data frame for statistics of Jaccard similarity values
  
  ##'s' follows the count of simulations 
  for (s in 1:sim){
    
    Jaccard.stats$Mean[s] <- mean(Jaccard.values$Jaccard)
    ##enters mean value for all Jaccard similarity coefficients
    
    Jaccard.stats$Median[s] <- median(Jaccard.values$Jaccard)
    ##enters median value for all Jaccard similarity coefficients
    
    temp.percentile <- quantile(Jaccard.values$Jaccard, c(0.1, 0.2, 0.25, 0.3, 0.4, 0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99))
    ##calculates percentile values for all Jaccard similarity coefficients
    
    Jaccard.stats$upto10th_percentile[s] <- unname(temp.percentile["10%"])
    ##enters 10th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto20th_percentile[s] <- unname(temp.percentile["20%"])
    ##enters 20th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto25th_percentile[s] <- unname(temp.percentile["25%"])
    ##enters 25th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto30th_percentile[s] <- unname(temp.percentile["30%"])
    ##enters 30th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto40th_percentile[s] <- unname(temp.percentile["40%"])
    ##enters 40th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto60th_percentile[s] <- unname(temp.percentile["60%"])
    ##enters 60th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto70th_percentile[s] <- unname(temp.percentile["70%"])
    ##enters 70th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto75th_percentile[s] <- unname(temp.percentile["75%"])
    ##enters 75th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto80th_percentile[s] <- unname(temp.percentile["80%"])
    ##enters 80th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto90th_percentile[s] <- unname(temp.percentile["90%"])
    ##enters 90th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto95th_percentile[s] <- unname(temp.percentile["95%"])
    ##enters 95th percentile value for all Jaccard similarity coefficients
    
    Jaccard.stats$upto99th_percentile[s] <- unname(temp.percentile["99%"])
    ##enters 99th percentile value for all Jaccard similarity coefficients
    
    rm(temp.percentile)
  }
  
  write.csv(Jaccard.stats, file = output.Stats.title, row.names = TRUE)
  ##outputs CSV file containing Jaccard values
  
  rm(list = c("i", "j", "index", "sim", "s"))
}

############################################################################################################
############################################################################################################
##TAKES TOO LONG TO RUN

##Jaccard.column.stats("210527_083_Cengen medium_nhr-84_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_086_Cengen med_nhr-84 all GPCR genes_Jaccard similarity.csv", "210527_087_Cengen med_nhr-84 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-84 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_083_Cengen medium_nhr-84_binary values_raw.csv", "210527_082_Cengen med_nhr-84 TarOrt2 GPCR genes_binary values.csv", "210527_084_Cengen med_nhr-84 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_085_Cengen med_nhr-84 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-84 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_073_Cengen medium_nhr-66_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_076_Cengen med_nhr-66 all GPCR genes_Jaccard similarity.csv", "210527_077_Cengen med_nhr-66 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-66 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_073_Cengen medium_nhr-66_binary values_raw.csv", "210527_072_Cengen med_nhr-66 TarOrt2 GPCR genes_binary values.csv", "210527_074_Cengen med_nhr-66 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_075_Cengen med_nhr-66 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-66 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_064_Cengen medium_nhr-6_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_067_Cengen med_nhr-6 all GPCR genes_Jaccard similarity.csv", "210527_068_Cengen med_nhr-6 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-6 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_064_Cengen medium_nhr-6_binary values_raw.csv", "210527_063_Cengen med_nhr-6 TarOrt2 GPCR genes_binary values.csv", "210527_065_Cengen med_nhr-6 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_066_Cengen med_nhr-6 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-6 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_056_Cengen medium_nhr-47_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_059_Cengen med_nhr-47 all GPCR genes_Jaccard similarity.csv", "210527_060_Cengen med_nhr-47 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-47 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_056_Cengen medium_nhr-47_binary values_raw.csv", "210527_055_Cengen med_nhr-47 TarOrt2 GPCR genes_binary values.csv", "210527_057_Cengen med_nhr-47 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_058_Cengen med_nhr-47 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-47 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_049_Cengen medium_nhr-46_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_052_Cengen med_nhr-46 all GPCR genes_Jaccard similarity.csv", "210527_053_Cengen med_nhr-46 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-46 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_049_Cengen medium_nhr-46_binary values_raw.csv", "210527_048_Cengen med_nhr-46 TarOrt2 GPCR genes_binary values.csv", "210527_050_Cengen med_nhr-46 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_051_Cengen med_nhr-46 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-46 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_042_Cengen medium_nhr-45_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_045_Cengen med_nhr-45 all GPCR genes_Jaccard similarity.csv", "210527_046_Cengen med_nhr-45 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-45 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_042_Cengen medium_nhr-45_binary values_raw.csv", "210527_041_Cengen med_nhr-45 TarOrt2 GPCR genes_binary values.csv", "210527_043_Cengen med_nhr-45 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_044_Cengen med_nhr-45 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-45 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_035_Cengen medium_nhr-44_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_038_Cengen med_nhr-44 all GPCR genes_Jaccard similarity.csv", "210527_039_Cengen med_nhr-44 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-44 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_035_Cengen medium_nhr-44_binary values_raw.csv", "210527_034_Cengen med_nhr-44 TarOrt2 GPCR genes_binary values.csv", "210527_036_Cengen med_nhr-44 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_037_Cengen med_nhr-44 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-44 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_027_Cengen medium_nhr-37_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_030_Cengen med_nhr-37 all GPCR genes_Jaccard similarity.csv", "210527_031_Cengen med_nhr-37 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-37 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_027_Cengen medium_nhr-37_binary values_raw.csv", "210527_026_Cengen med_nhr-37 TarOrt2 GPCR genes_binary values.csv", "210527_028_Cengen med_nhr-37 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_029_Cengen med_nhr-37 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-37 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_020_Cengen medium_nhr-36_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_023_Cengen med_nhr-36 all GPCR genes_Jaccard similarity.csv", "210527_024_Cengen med_nhr-36 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-36 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_020_Cengen medium_nhr-36_binary values_raw.csv", "210527_019_Cengen med_nhr-36 TarOrt2 GPCR genes_binary values.csv", "210527_021_Cengen med_nhr-36 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_022_Cengen med_nhr-36 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-36 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_013_Cengen medium_nhr-3_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_016_Cengen med_nhr-3 all GPCR genes_Jaccard similarity.csv", "210527_017_Cengen med_nhr-3 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-3 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_013_Cengen medium_nhr-3_binary values_raw.csv", "210527_012_Cengen med_nhr-3 TarOrt2 GPCR genes_binary values.csv", "210527_014_Cengen med_nhr-3 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_015_Cengen med_nhr-3 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-3 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_004_Cengen medium_nhr-236_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_007_Cengen med_nhr-236 all GPCR genes_Jaccard similarity.csv", "210527_008_Cengen med_nhr-236 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-236 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210527_004_Cengen medium_nhr-236_binary values_raw.csv", "210527_003_Cengen med_nhr-236 TarOrt2 GPCR genes_binary values.csv", "210527_005_Cengen med_nhr-236 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210527_006_Cengen med_nhr-236 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-236 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_045_Cengen medium_nhr-216_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_048_Cengen med_nhr-216 all GPCR genes_Jaccard similarity.csv", "210526_049_Cengen med_nhr-216 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-216 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_045_Cengen medium_nhr-216_binary values_raw.csv", "210526_044_Cengen med_nhr-216 TarOrt2 GPCR genes_binary values.csv", "210526_046_Cengen med_nhr-216 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_047_Cengen med_nhr-216 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-216 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_038_Cengen medium_nhr-213_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_041_Cengen med_nhr-213 all GPCR genes_Jaccard similarity.csv", "210526_042_Cengen med_nhr-213 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-213 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_038_Cengen medium_nhr-213_binary values_raw.csv", "210526_037_Cengen med_nhr-213 TarOrt2 GPCR genes_binary values.csv", "210526_039_Cengen med_nhr-213 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_040_Cengen med_nhr-213 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-213 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_031_Cengen medium_nhr-21_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_034_Cengen med_nhr-21 all GPCR genes_Jaccard similarity.csv", "210526_035_Cengen med_nhr-21 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-21 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_031_Cengen medium_nhr-21_binary values_raw.csv", "210526_030_Cengen med_nhr-21 TarOrt2 GPCR genes_binary values.csv", "210526_032_Cengen med_nhr-21 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_033_Cengen med_nhr-21 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-21 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_024_Cengen medium_nhr-142_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_027_Cengen med_nhr-142 all GPCR genes_Jaccard similarity.csv", "210526_028_Cengen med_nhr-142 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-142 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_024_Cengen medium_nhr-142_binary values_raw.csv", "210526_023_Cengen med_nhr-142 TarOrt2 GPCR genes_binary values.csv", "210526_025_Cengen med_nhr-142 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_026_Cengen med_nhr-142 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-142 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_017_Cengen medium_nhr-138_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_020_Cengen med_nhr-138 all GPCR genes_Jaccard similarity.csv", "210526_021_Cengen med_nhr-138 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-138 and its all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_017_Cengen medium_nhr-138_binary values_raw.csv", "210526_016_Cengen med_nhr-138 TarOrt2 GPCR genes_binary values.csv", "210526_018_Cengen med_nhr-138 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_019_Cengen med_nhr-138 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-138 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_010_Cengen medium_nhr-133_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_013_Cengen med_nhr-133 all GPCR genes_Jaccard similarity.csv", "210526_014_Cengen med_nhr-133 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-133 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_010_Cengen medium_nhr-133_binary values_raw.csv", "210526_009_Cengen med_nhr-133 TarOrt2 GPCR genes_binary values.csv", "210526_011_Cengen med_nhr-133 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_012_Cengen med_nhr-133 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-133 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_003_Cengen medium_nhr-125_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_006_Cengen med_nhr-125 all GPCR genes_Jaccard similarity.csv", "210526_007_Cengen med_nhr-125 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-125 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210526_003_Cengen medium_nhr-125_binary values_raw.csv", "210526_002_Cengen med_nhr-125 TarOrt2 GPCR genes_binary values.csv", "210526_004_Cengen med_nhr-125 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210526_005_Cengen med_nhr-125 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-125 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_045_Cengen medium_nhr-117_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_048_Cengen med_nhr-117 all GPCR genes_Jaccard similarity.csv", "210525_049_Cengen med_nhr-117 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-117 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_045_Cengen medium_nhr-117_binary values_raw.csv", "210525_044_Cengen med_nhr-117 TarOrt2 GPCR genes_binary values.csv", "210525_046_Cengen med_nhr-117 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_047_Cengen med_nhr-117 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-117 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_038_Cengen medium_nhr-100_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_041_Cengen med_nhr-100 all GPCR genes_Jaccard similarity.csv", "210525_042_Cengen med_nhr-100 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-100 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_038_Cengen medium_nhr-100_binary values_raw.csv", "210525_037_Cengen med_nhr-100 TarOrt2 GPCR genes_binary values.csv", "210525_039_Cengen med_nhr-100 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_040_Cengen med_nhr-100 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-100 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_031_Cengen medium_unc-55_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_034_Cengen med_unc-55 all GPCR genes_Jaccard similarity.csv", "210525_035_Cengen med_unc-55 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for unc-55 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_031_Cengen medium_unc-55_binary values_raw.csv", "210525_030_Cengen med_unc-55 TarOrt2 GPCR genes_binary values.csv", "210525_032_Cengen med_unc-55 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_033_Cengen med_unc-55 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for unc-55 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_024_Cengen medium_odr-7_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_027_Cengen med_odr-7 all GPCR genes_Jaccard similarity.csv", "210525_028_Cengen med_odr-7 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for odr-7 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_024_Cengen medium_odr-7_binary values_raw.csv", "210525_023_Cengen med_odr-7 TarOrt2 GPCR genes_binary values.csv", "210525_025_Cengen med_odr-7 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_026_Cengen med_odr-7 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for odr-7 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_017_Cengen medium_nhr-1_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_020_Cengen med_nhr-1 all GPCR genes_Jaccard similarity.csv", "210525_021_Cengen med_nhr-1 all GPCR genes_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-1 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_017_Cengen medium_nhr-1_binary values_raw.csv", "210525_016_Cengen med_nhr-1 TarOrt2 GPCR genes_binary values.csv", "210525_018_Cengen med_nhr-1 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_019_Cengen med_nhr-1 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for nhr-1 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_009_Cengen medium_daf-12_binary values_raw.csv", "210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_012_Cengen med_daf-12 all GPCRs_Jaccard similarity.csv", "210525_013_Cengen med_daf-12 all GPCRs_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for daf-12 and all GPCR genes using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210525_009_Cengen medium_daf-12_binary values_raw.csv", "210525_008_Cengen med_daf-12 TarOrt2 GPCR genes_binary values.csv", "210525_010_Cengen med_daf-12 Target Ortho 2 GPCR hits_Jaccard similarity.csv", "210525_011_Cengen med_daf-12 Target Ortho 2 GPCR hits_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for daf-12 and its Target Ortho 2 GPCR hits using binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210521_010_Cengen med_Non-conserved NHR genes_binary values.csv", "210521_103_Cengen med_GPCR Non-conserved NHR_Jaccard similarity.csv", "210521_104_Cengen med_GPCR Non-conserved NHR_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and Non-conserved NHR binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210521_008_Cengen med_Conserved NHR genes_binary values.csv", "210521_101_Cengen med_GPCR Conserved NHR_Jaccard similarity.csv", "210521_102_Cengen med_GPCR Conserved NHR_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and Conserved NHR binary data from CeNGEN medium dataset

##Jaccard.column.stats("210511_012_Cengen med_Protein kinase genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210520_101_Cengen med_Kinase NHR_Jaccard similarity.csv", "210520_102_Cengen med_Kinase NHR_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for NHR and protein kinase binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210511_012_Cengen med_Protein kinase genes_binary values.csv", "210519_105_Cengen med_GPCR Kinase_Jaccard similarity.csv", "210519_106_Cengen med_GPCR Kinase_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and protein kinase binary data from CeNGEN medium dataset

##Jaccard.column.stats("210329_005_Cengen medium_NHR genes_binary values.csv", "210511_010_Cengen med_F-box genes_binary values.csv", "210519_103_Cengen med_NHR F-box_Jaccard similarity.csv", "210519_104_Cengen med_NHR F-box_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for NHR and F-box binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210511_010_Cengen med_F-box genes_binary values.csv", "210519_101_Cengen med_GPCR F-box_Jaccard similarity.csv", "210519_102_Cengen med_GPCR F-box_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and F-box binary data from CeNGEN medium dataset

##Jaccard.column.stats("210511_008_Cengen med_C-type lectin genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210512_013_Cengen med_C-type lectin NHR_Jaccard similarity.csv", "210512_014_Cengen med_C-type lectin NHR_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for NHR and C-type lectin binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210511_008_Cengen med_C-type lectin genes_binary values.csv", "210512_011_Cengen med_GPCR C-type lectin_Jaccard similarity.csv", "210512_012_Cengen med_GPCR C-type lectin_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and C-type lectin binary data from CeNGEN medium dataset

##Jaccard.column.stats("210329_005_Cengen medium_NHR genes_binary values.csv", "210512_002_Cengen med_C2H2 ZF genes_binary values.csv", "210512_009_Cengen med_NHR C2H2 ZF_Jaccard similarity.csv", "210512_010_Cengen med_NHR C2H2 ZF_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for NHR and C2H2 ZF binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210512_002_Cengen med_C2H2 ZF genes_binary values.csv", "210512_007_Cengen med_GPCR C2H2 ZF_Jaccard similarity.csv", "210512_008_Cengen med_GPCR C2H2 ZF_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and C2H2 ZF binary data from CeNGEN medium dataset

##Jaccard.column.stats("210329_005_Cengen medium_NHR genes_binary values.csv", "210511_015_Cengen med_Homeodomain genes_binary values.csv", "210512_005_Cengen med_NHR Homeodomain_Jaccard similarity.csv", "210512_006_Cengen med_NHR Homeodomain_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for NHR and Homeodomain binary data from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210511_015_Cengen med_Homeodomain genes_binary values.csv", "210512_003_Cengen med_GPCR Homeodomain_Jaccard similarity.csv", "210512_004_Cengen med_GPCR Homeodomain_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and Homeodomain binary data from CeNGEN medium dataset

##Jaccard.column.stats("210502_013_Cengen medium_nonhyper GPCR genes_binary values.csv", "210502_011_Cengen medium_nonhyper NHR genes_binary values.csv", "210504_014_Cengen medium_nonhyper gpcr nhr_Jaccard similarity.csv", "210504_015_Cengen medium_nonhyper gpcr nhr_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and NHR binary data in NON-HYPERDIVERGENT regions from CeNGEN medium dataset

##Jaccard.column.stats("210502_007_Cengen medium_hyperdiv GPCR genes_binary values.csv", "210430_002_Cengen medium_hyperdiv NHR genes_binary values.csv", "210504_011_Cengen medium_hyperdiv gpcr nhr_Jaccard similarity.csv", "210504_012_Cengen medium_hyperdiv gpcr nhr_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and NHR binary data in HYPERDIVERGENT regions from CeNGEN medium dataset

##Jaccard.column.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210504_006_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210504_007_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Lists all Jaccard values and performs column stats for GPCR and NHR binary data from CeNGEN medium dataset
