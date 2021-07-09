Simulate.Jaccard.stats <- function(gene.family.1.binary, gene.family.2.binary, output.Jaccard.title, output.Stats.title) {
  ##function to simulate Jaccard similarity index b/w two gene families using simulated binary data
  ##and performs column statistics on the similarity values
  ##Number of columns (neuron classes) need to be identical in both input files
  
  gene.1.binary <- read.csv(gene.family.1.binary, header = TRUE, row.names = 1)
  ##reads Cengen binary file for 1st gene category and assigns first column as row.names
  
  gene.1.num <- nrow(gene.1.binary)
  ##identifies total number of genes of 1st category from number of rows
  
  neuron.num <- ncol(gene.1.binary)
  ##identifies total number of neurons from number of columns
  
  gene.2.binary <- read.csv(gene.family.2.binary, header = TRUE, row.names = 1)
  ##reads Cengen binary file for 2nd gene category and assigns first column as row.names
  
  gene.2.num <- nrow(gene.2.binary)
  ##identifies total number of genes of 2nd category from number of rows
  
  sim.gene.1 <- data.frame(matrix(rep(NA, neuron.num*gene.1.num), nrow = gene.1.num, ncol = neuron.num))
  ##creates an empty data frame that is the same size as gene family 1 binary data
  
  ##'x' follows rows of the genes in 1st category
  for (x in 1:gene.1.num){
    
    sim.gene.1[x, ] <- as.numeric(sample(gene.1.binary[x, ]))
    ##shuffles the neuronal expression pattern (column data) for each gene (row) in 1st category
  }
  
  sim.gene.2 <- data.frame(matrix(rep(NA, neuron.num*gene.2.num), nrow = gene.2.num, ncol = neuron.num))
  ##creates an empty data frame that is the same size as gene family 2 binary data
  
  rm(x)
  
  ##'y' follows rows of the genes in 2nd category
  for (y in 1:gene.2.num){
    
    sim.gene.2[y, ] <- as.numeric(sample(gene.2.binary[y, ]))
    ##shuffles the neuronal expression pattern (column data) for each gene (row) in 2nd category
  }
  
  rm(y)
  
  Jaccard.values <- data.frame("Jaccard" = rep(NA, gene.1.num*gene.2.num))
  ##creates an empty data frame for Jaccard similarity values for genes from 1st and 2nd category as columns
  
  index <- 1
  ##index identifies which row to fill in output file
  
  ##'i' follows genes in 1st category 
  for (i in 1:gene.1.num){
    
    ##'j' follows genes in 2nd category
    for (j in 1:gene.2.num){
      
      sums <- sim.gene.1[i, ] + sim.gene.2[j, ]
      ##adds the simulated binary values for two genes - one from each category
      
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
  
  rm(list = c("i", "j", "index", "sim", "s", "sim.gene.1", "sim.gene.2"))
}

############################################################################################################
############################################################################################################

##Simulate.Jaccard.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210507_SIM 005_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210507_SIM 005_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Performs column stats on simulated Cengen medium binary data for NHR and GPCR genes

##Simulate.Jaccard.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210506_SIM 004_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210506_SIM 004_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Performs column stats on simulated Cengen medium binary data for NHR and GPCR genes

##Simulate.Jaccard.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210506_SIM 003_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210506_SIM_003_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Performs column stats on simulated Cengen medium binary data for NHR and GPCR genes

##Simulate.Jaccard.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210506_SIM 002_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210506_SIM_002_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Performs column stats on simulated Cengen medium binary data for NHR and GPCR genes

##Simulate.Jaccard.stats("210330_002_Cengen medium_GPCR genes_binary values.csv", "210329_005_Cengen medium_NHR genes_binary values.csv", "210506_SIM 001_Cengen medium_gpcr nhr_Jaccard similarity.csv", "210506_SIM_001_Cengen medium_gpcr nhr_Jaccard stat summary.csv")
##Performs column stats on simulated Cengen medium binary data for NHR and GPCR genes
