Subset.neuron.type <- function(all.data.file, neuron.type.file, output.file.title){
  all.data.temp <- read.csv(all.data.file, header = TRUE, row.names = 1)
  ##reads file containing gene numbers in neurons and assigns first column as row.names
  
  neuron.type.temp <- read.csv(neuron.type.file, header = FALSE)
  ##reads file containing neuron names for each neuron type
  
  subset.data.temp <- all.data.temp[gettext(neuron.type.temp$V1), ]
  ##subsets original data for specific neuron type
  
  head(subset.data.temp)
  
  write.csv(subset.data.temp, file = output.file.title, row.names = TRUE)
  ##outputs a file for specific neuron type
  
  rm(list = c("all.data.temp", "neuron.type.temp", "subset.data.temp"))
}
