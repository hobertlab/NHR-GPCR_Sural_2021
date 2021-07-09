  heat.bidirec <- read.csv("210330_012_Cengen medium_GPCR NHR_Spearman rho_heatmap_raw.csv", header = TRUE, row.names = 1, check.names = FALSE)
  ##reads file for similarity between gene expresion for gene pairs across neurons in Cengen data
  
  gpcr.num <- ncol(heat.bidirec)
  ##identifies total number of genes of 1st category from number of columns
  
  nhr.num <- nrow(heat.bidirec)
  ##identifies total number of genes of 2nd category from number of rows
  
  uniq.pairs <- data.frame("NHR" = rep(NA, 1000000), "GPCR" = rep(NA, 1000000), "rho" = rep(NA, 1000000))
  #creates an empty data frame of three columns and 1000000 rows - CHANGE VALUES HERE
  
  index <- 1
  ##index idenfies which row to fill in output file
  
  ##'j' follows genes in 2nd category 
  for (j in 1:nhr.num){
    
    ##'i' follows genes in 1st category
    for (i in 1:gpcr.num){
      
      if(heat.bidirec[j , i] >= 0.5){
        ##Values greater than or equal to 0.5 - CHANGE VALUES HERE 
        
        uniq.pairs$NHR[index] <- row.names(heat.bidirec)[j]
        ##inputs name of gene of 2nd category
        
        uniq.pairs$GPCR[index] <- colnames(heat.bidirec)[i]
        ##inputs name of gene of 1st category
        
        uniq.pairs$rho[index] <- heat.bidirec[j , i]
        ##inputs the rho correlation value - CHANGE VECTOR NAME HERE
        
        index <- index + 1
        
      }else {
        index <- index  
        
      }
    }
  }
  
write.csv(uniq.pairs, file = "210401_007_Cengen medium_correl rho greater equal 0.5 nhr gpcr pairs.csv", row.names = FALSE)
##outputs CSV file containing unique pair names
  
rm(list = c("heat.bidirec", "uniq.pairs", "gpcr.num", "nhr.num", "i", "j", "index"))
  
##rm(list = ls())