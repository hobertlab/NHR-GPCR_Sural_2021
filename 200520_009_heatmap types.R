if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
##Heatmap for SPEARMAN RHO
library(circlize)
col_linear = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
row_labels_temp = structure(gettext(heatmap.col.labels[1, ]), names = paste0("row", 1:ncol(heatmap.fract)))
##creates a named vector for genes to be used as row labels in the heatmap
Heatmap(t(as.matrix(heatmap.fract)),
        name = "Spearman rho", #title of legend
        col = col_linear, #ensures colors are linearly spread
        row_labels = row_labels_temp, ##enters row names from separate vector
        column_title = "NHR genes", row_title = "GPCR genes",
        row_names_gp = gpar(fontsize = 1), # Text size for row names
        column_names_gp = gpar(fontsize = 3),
        heatmap_legend_param = list(at = c(-1, -0.5, 0, 0.5, 1)) #can choose legend axis
)
rm(heatmap.fract)
rm(heatmap.col.labels)
rm(row_labels_temp)
rm(col_linear)

##WORKS GREAT; data is transposed to allow better resolution; allows to pick legend axis, gives red-blue heatmap but does and prints gene names in output PDF
##############################################################################################
library(ComplexHeatmap)
## heatmap for JACCARD SIMILARITY
library(circlize)
col_linear = colorRamp2(c(0, 1), c("white", "red"))
row_labels_temp = structure(gettext(heatmap.col.labels[1, ]), names = paste0("row", 1:ncol(heatmap.fract)))
##creates a named vector for genes to be used as row labels in the heatmap
Heatmap(t(as.matrix(heatmap.fract)),
        name = "Jaccard index", #title of legend
        col = col_linear, #ensures colors are linearly spread
        row_labels = row_labels_temp, ##enters row names from separate vector
        column_title = "NHR genes", row_title = "GPCR genes",
        row_names_gp = gpar(fontsize = 0.5), # Text size for row names
        column_names_gp = gpar(fontsize = 1),
        heatmap_legend_param = list(at = c(0, 0.25, 0.5, 0.75, 1)) #can choose legend axis
)
rm(heatmap.fract)
rm(heatmap.col.labels)
rm(row_labels_temp)
rm(col_linear)

##EXAMPLES - make sure LARGER gene family is columns of the input matrix

heatmap.fract <- read.csv("210401_008_Cengen medium_gpcr nhr_Jaccard similarity_heatmap_raw.csv", header = TRUE, row.names = 1)
##reads file that contains Jaccard index values for all GPCR:NHR pairs from Cengen medium data
heatmap.col.labels <- read.csv("210401_008_Cengen medium_gpcr nhr_Jaccard similarity_heatmap_raw.csv", header = FALSE, row.names = 1)
##reads same file but retains gene names in first row so that '-' is not replaced automatically with '.'
##row font size = 0.5, column font size = 1
