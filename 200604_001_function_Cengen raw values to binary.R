Cengen.raw.to.binary <- function(Input.raw.Cengen.file, Output.binary.file) {
  ##function to convert raw data to binary data
  
  raw.temp <- read.csv(Input.raw.Cengen.file, header = TRUE, row.names = 1)
  ##reads raw Cengen file and assigns first column as row.names
  
  logical.temp <- data.frame(lapply(raw.temp[1:ncol(raw.temp)], as.logical), row.names = row.names(raw.temp))
  ##converts data into (T/F) format based on expression values
  
  binary.temp <- data.frame(lapply(logical.temp[1:ncol(logical.temp)], as.integer), row.names = row.names(logical.temp))
  ##converts data into (1/0) format based on expression values
  
  write.csv(binary.temp, file = Output.binary.file, row.names = TRUE)
  ##outputs a file with data in binary format
  
  rm(list = c("raw.temp", "binary.temp", "logical.temp"))
  
}
