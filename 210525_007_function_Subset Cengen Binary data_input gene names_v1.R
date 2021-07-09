Subset.Binary.Cengen <- function(binary.whole, gene.list, output.title){
  ##this function can extract data for any subset of genes from a CeNGEN binary data file
  ##gene list provided in Target Ortho 2 format
  
  binary.whole.temp <- read.csv(binary.whole, header = TRUE, row.names = 1)
  ##reads CeNGEN binary data file and assigns first column (gene names) as row.names
  
  gene.list.temp <- read.csv(gene.list, header = TRUE, row.names = 1)
  ##reads file containing gene names to extract from CeNGEN binary data file
  ##Header = TRUE is used for a gene list file in Target Ortho 2 format
    
  subset.data.temp <- binary.whole.temp[which(row.names(binary.whole.temp) %in% gettext(row.names(gene.list.temp))), ]
  ##subsets original data for the provided gene list
  
  write.csv(subset.data.temp, file = output.title, row.names = TRUE)
  ##outputs a subset of the original Target Ortho 2 file for the specified list of genes
  
  rm(list = c("binary.whole.temp", "gene.list.temp", "subset.data.temp"))
}

##EXAMPLES

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_081_nhr-84 TargetOrtho2 data_GPCR subset_raw.csv", "210527_082_Cengen med_nhr-84 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-84 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_079_nhr-79 TargetOrtho2 data_GPCR subset_raw.csv", "210527_080_Cengen med_nhr-79 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-79 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_071_nhr-66 TargetOrtho2 data_GPCR subset_raw.csv", "210527_072_Cengen med_nhr-66 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-66 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_069_nhr-60 TargetOrtho2 data_GPCR subset_raw.csv", "210527_070_Cengen med_nhr-60 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-60 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_062_nhr-6 TargetOrtho2 data_GPCR subset_raw.csv", "210527_063_Cengen med_nhr-6 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-6 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_054_nhr-47 TargetOrtho2 data_GPCR subset_raw.csv", "210527_055_Cengen med_nhr-47 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-47 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_047_nhr-46 TargetOrtho2 data_GPCR subset_raw.csv", "210527_048_Cengen med_nhr-46 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-46 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_040_nhr-45 TargetOrtho2 data_GPCR subset_raw.csv", "210527_041_Cengen med_nhr-45 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-45 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_033_nhr-44 TargetOrtho2 data_GPCR subset_raw.csv", "210527_034_Cengen med_nhr-44 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-44 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_025_nhr-37 TargetOrtho2 data_GPCR subset_raw.csv", "210527_026_Cengen med_nhr-37 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-37 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_018_nhr-36 TargetOrtho2 data_GPCR subset_raw.csv", "210527_019_Cengen med_nhr-36 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-36 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_011_nhr-3 TargetOrtho2 data_GPCR subset_raw.csv", "210527_012_Cengen med_nhr-3 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-3 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_009_nhr-25 TargetOrtho2 data_GPCR subset_raw.csv", "210527_010_Cengen med_nhr-25 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-25 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210527_002_nhr-236 TargetOrtho2 data_GPCR subset_raw.csv", "210527_003_Cengen med_nhr-236 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-236 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_043_nhr-216 TargetOrtho2 data_GPCR subset_raw.csv", "210526_044_Cengen med_nhr-216 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-216 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_036_nhr-213 TargetOrtho2 data_GPCR subset_raw.csv", "210526_037_Cengen med_nhr-213 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-213 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_029_nhr-21 TargetOrtho2 data_GPCR subset_raw.csv", "210526_030_Cengen med_nhr-21 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-21 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_022_nhr-142 TargetOrtho2 data_GPCR subset_raw.csv", "210526_023_Cengen med_nhr-142 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-142 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_015_nhr-138 TargetOrtho2 data_GPCR subset_raw.csv", "210526_016_Cengen med_nhr-138 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-138 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_008_nhr-133 TargetOrtho2 data_GPCR subset_raw.csv", "210526_009_Cengen med_nhr-133 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-133 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210526_001_nhr-125 TargetOrtho2 data_GPCR subset_raw.csv", "210526_002_Cengen med_nhr-125 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-125 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_043_nhr-117 TargetOrtho2 data_GPCR subset_raw.csv", "210525_044_Cengen med_nhr-117 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-117 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_036_nhr-100 TargetOrtho2 data_GPCR subset_raw.csv", "210525_037_Cengen med_nhr-100 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-100 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("REF_210525_210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_029_unc-55 TargetOrtho2 data_GPCR subset_raw.csv", "210525_030_Cengen med_unc-55 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of unc-55 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("REF_210525_210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_022_odr-7 TargetOrtho2 data_GPCR subset_raw.csv", "210525_023_Cengen med_odr-7 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of odr-7 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("REF_210525_210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_015_nhr-1 TargetOrtho2 data_GPCR subset_raw.csv", "210525_016_Cengen med_nhr-1 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of nhr-1 from CeNGEN binary data file of all GPCR genes

##Subset.Binary.Cengen("REF_210525_210330_002_Cengen medium_GPCR genes_binary values.csv", "210525_006_daf-12 TargetOrtho2 data_GPCR subset_raw.csv", "210525_008_Cengen med_daf-12 TarOrt2 GPCR genes_binary values.csv")
##Subsets GPCR targets of daf-12 from CeNGEN binary data file of all GPCR genes

