#importar dados
filename <- file.choose()
dadosTB <- readRDS(filename)

filename2 <- file.choose()
POP2013 <- read.csv(filename2, sep = ";")
