Jaccard.similar.2.gene.family <- function(gene.family.1.Cengen.binary.file, gene.family.2.Cengen.binary.file, output.file.title) {
##function to find Jaccard similarity index b/w all members of two gene families and extract data as a matrix
  
gene.1.binary <- read.csv(gene.family.1.Cengen.binary.file, header = TRUE, row.names = 1)
##reads Cengen binary file for 1st gene category and assigns first column as row.names

gene.1.num <- nrow(gene.1.binary)
##identifies total number of genes of 1st category from number of rows

neuron.num <- ncol(gene.1.binary)
##identifies total number of neurons from number of columns

gene.2.binary <- read.csv(gene.family.2.Cengen.binary.file, header = TRUE, row.names = 1)
##reads Cengen binary file for 2nd gene category and assigns first column as row.names

gene.2.num <- nrow(gene.2.binary)
##identifies total number of genes of 2nd category from number of rows

heatmap.fract <- data.frame(matrix(rep(NA, gene.2.num*gene.1.num), nrow = gene.2.num, ncol = gene.1.num, dimnames = list(gettext(row.names(gene.2.binary)), gettext(row.names(gene.1.binary)))),
                            check.names = FALSE)
##creates an empty data frame with gene names from 1st and 2nd category as columns and rows, respectively. Check.names = FALSE ensures that '-' is not removed from gene names

##'i' follows genes in 1st category 
for (i in 1:gene.1.num){
  
  ##'j' follows genes in 2nd category
  for (j in 1:gene.2.num){
    
    either <- 0
    ##either identifies number of neurons in which either of two genes is expressed
    
    both <- 0
    ##both identifies number of neurons in which both genes are expressed
    
    ##'k' follows neuron number
    for (k in 1:neuron.num){
      
      if (gene.1.binary[i, k] + gene.2.binary[j, k] == 2){
        
        either <- either + 1
        both <- both + 1
        
      }else if (gene.1.binary[i, k] + gene.2.binary[j, k] == 1){
        
        either <- either + 1
        both <- both + 0
        
      }else {
        either <- either + 0
        both <- both + 0
      }
    }
    
    if (either == 0){
      heatmap.fract[j, i] = 0
      
    }else {
      heatmap.fract[j, i] = both / either  
      
    }
    
    rm(list = c("either", "both"))
  } 
  
}

write.csv(heatmap.fract, file = output.file.title, row.names = TRUE)
##outputs CSV file containing heatmap fractions

rm(list = c("i", "j", "k"))
}

############################################################################################################################################
