Subset.Target.Ortho2 <- function(Tar.Ort2.whole, gene.list, output.title){
  ##this function can extract data for any subset of genes from Lori's Target Ortho 2 output file
  
  Tar.Ort2.temp <- read.csv(Tar.Ort2.whole, header = TRUE, row.names = 1)
  ##reads Target Ortho 2 file and assigns first column (gene names) as row.names
  
  gene.list.temp <- read.csv(gene.list, header = FALSE)
  ##reads file containing gene names to extract from Target Ortho 2 file
  
  subset.data.temp <- Tar.Ort2.temp[which(row.names(Tar.Ort2.temp) %in% gettext(gene.list.temp$V2)), ]
  ##subsets original data for the provided gene list
  
  write.csv(subset.data.temp, file = output.title, row.names = TRUE)
  ##outputs a subset of the original Target Ortho 2 file for the specified list of genes
  
  rm(list = c("Tar.Ort2.temp", "gene.list.temp", "subset.data.temp"))
}

##EXAMPLES

##Subset.Target.Ortho2("data_210527_019_PosProb_nhr-84_M02414_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_081_nhr-84 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-84 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_018_PosProb_nhr-79_M02420_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_079_nhr-79 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-79 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_016_PosProb_nhr-67_M02408_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_078_nhr-67 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-67 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_015_PosProb_nhr-66_M00694_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_071_nhr-66 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-66 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_014_PosProb_nhr-60_M00699_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_069_nhr-60 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-60 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_013_PosProb_nhr-6_M01298_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_062_nhr-6 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-6 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_012_PosProb_nhr-48_M03940_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_061_nhr-48 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-48 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_011_PosProb_nhr-47_M00686_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_054_nhr-47 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-47 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_010_PosProb_nhr-46_M00692_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_047_nhr-46 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-46 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_009_PosProb_nhr-45_M00700_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_040_nhr-45 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-45 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_008_PosProb_nhr-44_M00700_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_033_nhr-44 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-44 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_008_PosProb_nhr-42_M00691_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_032_nhr-42 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-42 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_006_PosProb_nhr-37_M02417_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_025_nhr-37 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-37 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_005_PosProb_nhr-36_M02418_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_018_nhr-36 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-36 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_004_PosProb_nhr-3_M00687_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_011_nhr-3 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-3 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_003_PosProb_nhr-25_M08226_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_009_nhr-25 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-25 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210527_001_PosProb_nhr-236_M02394_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210527_002_nhr-236 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-236 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_007_PosProb_nhr-216_M00705_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_043_nhr-216 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-216 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_006_PosProb_nhr-213_M00703_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_036_nhr-213 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-213 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_005_PosProb_nhr-21_M00690_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_029_nhr-21 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-21 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_004_PosProb_nhr-142_M00703_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_022_nhr-142 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-142 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_003_PosProb_nhr-138_M00690_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_015_nhr-138 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-138 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_002_PosProb_nhr-133_M00701_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_008_nhr-133 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-133 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210526_001_PosProb_nhr-125_M02419_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210526_001_nhr-125 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-125 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_007_PosProb_nhr-117_M02419_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_043_nhr-117 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-117 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_006_PosProb_nhr-100_M00690_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_036_nhr-100 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-100 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_005_PosProb_unc-55_M00184_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_029_unc-55 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets unc-55 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_004_PosProb_odr-7_M00697_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_022_odr-7 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets odr-7 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_003_PosProb_nhr-1_M00686_2.00_2.00_TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_015_nhr-1 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets nhr-1 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_002_PosProb_fax-1_M06432_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_014_fax-1 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets fax-1 Target Ortho2 data to obtain only GPCR genes from the list

##Subset.Target.Ortho2("data_210525_001_PosProb_daf-12_M00685_2.00__TargetOrtho2_ranked_genes_summary.csv", "200603_004_Ce_GPCR_stable IDs.csv", "210525_006_daf-12 TargetOrtho2 data_GPCR subset_raw.csv")
##Subsets daf-12 Target Ortho2 data to obtain only GPCR genes from the list


