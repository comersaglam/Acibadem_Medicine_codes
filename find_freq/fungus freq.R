library(readr)
bacteria4 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/bacteria4.txt", 
                      col_names = FALSE)
View(bacteria4)

library("stringr")

bacteria_vector= unlist(bacteria4, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
b_genome = (unlist(strsplit(bacterium,split="")))
gen_table= table(b_genome)
f_freq_table = table(b_genome)/length(b_genome)*100
f_freq_table

?boxplot
