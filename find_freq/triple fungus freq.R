#install stringr package
bacteria = fungus
library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")

g1 = c("A", "G", "C", "T")
g2 = c("A", "G", "C", "T")
g3 = c("A", "G", "C", "T")
c = expand.grid(g1,g2,g3)

i=c()
s1=c()
s2=c()
s3=c()
n=c()

freq = function(i){
  i= 0
  while(i <= 64){
    s1 = c[i,]
    s2 = unlist(s1, use.names = F)
    s3 = paste(as.character(s2) , sep = "", collapse ="")
    n = str_count(bacterium, s3)/(nchar(bacterium)-2)*100
    print(paste(s3, " freq = ", n))
    i = (i+1)
  }
}

freq(0)
