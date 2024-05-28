library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
fungus_vector= unlist(fungi, use.names = F)
fungus <- paste(fungus_vector, sep="",collapse = "")
virus_vector = unlist(viruses, use.names = F)
virus = paste(virus_vector, sep = "", collapse="")

data1 = c()
data2 = c()
data3 = c()
short = c()
other1 = c()
other2 = c()
l = c()
h = c()
s = 1
i = c()
n = c()
k = 1
n = 0
b = 1
power = c()
p = 0
#######################################################
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
    if(data2<= data3){
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
  power <<- round(log2(nchar(short)),digits = 0) + 1
}
#####################################################
intervals_longer <<- function(short){
  p <<- p + 1
  l <<- nchar(short)
  b <<- b-(1/(2^s))
  h <<- as.integer(sqrt((l*(l+1)*b)+1/4)-1/2) 
  i <<- l-h
  s <<- s+1
  k <<- 1
  if(p<=power){
    while(k <= (h+1)){
      m <<- substr(short,k,k+i-1)
      if(str_count(other1,m)>0){
        n <<- n+1
        if(str_count(other2,m)>0){
          n <<- n+1
        }
        else{
          n <<- n-1
        }
      }
      k <<- k+1
    }
    if(n>=2){
      n <<- 0
      intervals_longer(short)
    }
    else{
      intervals_shorter(short)
    }
  }
  else{
    print(m)
    print(nchar(m))
    stop()
  }
}  
##########################################
intervals_shorter <<- function(short){
  p <<- p + 1
  l <<- nchar(short)
  b <<- b + (1/(2^s))
  h <<- as.integer(sqrt((l*(l+1)*b)+1/4)-1/2)
  i <<- l-h
  s <<- s+1
  k <<- 1
  if(p <= power){
    while(k <= (h+1)){
      m <<- substr(short,k,k+i-1)
      if(str_count(other1,m)>0){
        n <<- n+1
        if(str_count(other2,m)>0){
          n <<- n+1
        }
        else{
          n <<- n-1
        }
      }
      k <<- k+1
    }
    if(n>=2){
      n <<- 0
      intervals_longer(short)
    }
    else{
      intervals_shorter(short)
    }
  }
  else{
    print(m)
    print(nchar(m))
    stop()
  }
}
#####################################################
result = function(){
  library("stringr")
  data1 <<- c()
  data2 <<- c()
  data3 <<- c()
  short <<- c()
  other1 <<- c()
  other2 <<- c()
  l <<- c()
  h <<- c()
  s <<- 1
  i <<- c()
  n <<- c()
  k <<- 1
  n <<- 0
  b <<- 1
  power <<- c()
  p <<- 0
  shortest(bacterium, fungus, virus)
  intervals_longer(short)
}

result()

