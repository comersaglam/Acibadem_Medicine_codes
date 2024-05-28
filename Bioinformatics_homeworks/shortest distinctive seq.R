library("stringr")
bacteria_vector= unlist(bacteria, use.names = F)
bacterium <- paste(bacteria_vector, sep="",collapse = "")
fungus_vector= unlist(fungi, use.names = F)
fungus <- paste(fungus_vector, sep="",collapse = "")
virus_vector = unlist(viruses, use.names = F)
virus = paste(virus_vector, sep = "", collapse="")

# ben de biliyorum ki rastgele yaz?lm?? genomda bu sorunun cevab? 5-10 u ge?mez
# ama b?t?n dizili?ler bir metabolik olay? ifade etseydi ortak olmayan dizilim y?zlerce uzunlukta olabilirdi.
# (mesela bakteride A1,A2,A3) genleri 3k n?kleotide kar??l?k gelsin. mantar (A1,A2,A4,A5) genlerine sahip olursa ortak olmayan seri A3 olur ki yakla??k 1000 n?kleotid demektir.
# bu y?zden k?sa dizili?ten ba?lamak yerine t?m genomu logaritmik bir ?ekilde taramak daha elveri?li olacakt?r.
#######################################################
preset = function(){  
  data1 <<- c()
  data2 <<- c()
  data3 <<- c()
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
}
#####################################################
intervals_longer <<- function(data1,data2,data3){
  power <<- as.integer(log2(nchar(data1)),digits = 0) + 1
  p <<- p + 1
  l <<- nchar(data1)
  b <<- b-(1/(2^s))
  h <<- as.integer(sqrt((l*(l+1)*b)+1/4)-1/2) #finds the length of the sequence 
  #which stands in the middle of all sequences 
  #(for 1 to n long search, there are n.n+1/2 sequences to search)
  i <<- l-h
  s <<- s+1
  k <<- 1
  if(p <= power){
    while(k <= (h+1)){
      m <<- substr(data1,k,k+i-1)
      if(str_count(data2,m)>0){
        if(str_count(data3,m)>0){
        }
        else{
          d <<- m
          n <<- n + 1
        }
      }
      else{
        d <<- m
        n<<- n + 1
      }
      k <<- k+1
    }
    if(n>=1){#ortak olmayan varsa
      n <<- 0
      intervals_shorter(data1,data2,data3)
    }
    else{
      intervals_longer(data1,data2,data3)
    }
  }
  else{
    print(d)
    print(nchar(d))
    
  }
  
}  
##########################################
intervals_shorter <<- function(data1,data2,data3){
  power <<- as.integer(log2(nchar(data1)),digits = 0) + 1
  p <<- p + 1
  l <<- nchar(data1)
  b <<- b + (1/(2^s))
  h <<- as.integer(sqrt((l*(l+1)*b)+1/4)-1/2)
  i <<- l-h
  s <<- s+1
  k <<- 1
  if(p <= power){
    while(k <= (h+1)){
      m <<- substr(data1,k,k+i-1)
      if(str_count(data2,m)>0){
        if(str_count(data3,m)>0){
        }
        else{
          d <<- m
          n <<- n + 1
        }
      }
      else{
        d <<- m
        n <<- n + 1
      }
      k <<- k+1
    }
    if(n>=1){#ortak olmayan varsa
      n <<- 0
      intervals_shorter(data1,data2,data3)
    }
    else{
      intervals_longer(data1,data2,data3)
    }
  }
  else{
    print(d)
    print(nchar(d))
    
  }
}
#####################################################
result = function(){
  library("stringr")
  preset()
  intervals_longer(bacterium,fungus,virus)
  preset()
  intervals_longer(fungus,bacterium,virus)
  preset()
  intervals_longer(virus,bacterium,fungus)
}

result()

str_count(fungus,"N")#bu projede farkettim ki mantar genomunda "N"diye yaz?lm?? aminoasitler var. Bu ne demek, bir hata m??

