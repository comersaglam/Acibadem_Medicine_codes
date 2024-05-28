library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
b<<- c("A","C","G","T")
ikili<<- function(){
  i <<- 1
  k <<- 0
  while(k <= 3){
    k = k+1
    i = 1
    while(i<=4){
      d <<- paste(b[k],b[i],sep="",collapse = "")
      i =i+1
      print(paste(d, "freq=" ,str_count(bacterium,d)/length(bacterium)))
    }
  }
}
ikili()

