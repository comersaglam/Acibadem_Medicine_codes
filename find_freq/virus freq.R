bacteria = virus
#install stringr package
library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
b_genome = (unlist(strsplit(bacterium,split="")))
gen_table= table(b_genome)
v_freq_table = table(b_genome)/length(b_genome)*100
v_freq_table