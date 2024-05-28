library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
fungus_vector= unlist(fungi, use.names = F)
fungus <- paste(fungus_vector, sep="",collapse = "")
virus_vector = unlist(viruses, use.names = F)
virus = paste(virus_vector, sep = "", collapse="")

shortest <<- function(data1,data2,data3){
  if(nchar(data1)<= nchar(data2)){
    if(nchar(data1)<= nchar(data3)){
      short <<- data1
      other1 <<- data2
      other2 <<- data3
    }
    else{
      short <<- data3
      other1 <<- data2
      other2 <<- data1
    }
  }
  else{
    if(data2 <= data3){
      short <<- data2
      other1 <<- data1
      other3 <<- data3
    }
    else{
      short <<- data3
      other1 <<- data2
      other2 <<- data1
    }  
  }
}

answer = function(short){
  i <<- 1
  n <<- 0
  while ((k+i) <= nchar(short)) {
    m<<-substr(short,i,k+i)
    if(str_count(other1,m)>0){
      if(str_count(other2,m)>0){
        l <<- m
        n <<- 1
        i <<- i+1
      }
      else{
        i <<- i+1
      }
    }
    else{
      i <<- i+1
    }
    
  }
  print("a")
  if(n>0){
    k <<- k+1
    x <<- l
    y <<- 1
    answer(short)
  }
  else{
    if(y>0){
      print(x)
      stop()
    }
    else{
      k<<-k+1
      answer(short)
    }
  }
}


result = function(){
  data1 <<- c()
  data2 <<- c()
  data3 <<- c()
  short <<- c()
  i<<-1
  k<<-1
  x<<-c()
  m<<-c()
  n<<- 0
  y<<- 0
  shortest(bacterium,fungus,virus)
  answer(short)
}

result()
