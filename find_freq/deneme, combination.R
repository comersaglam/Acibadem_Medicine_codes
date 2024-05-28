a = c("a","g","c","t")
b = c("a","g","c","t")
c = c("a","g","c","t")
x= expand.grid(a,b,c)
deneme2 = unlist(expand.grid(a,b),use.names = F)
deneme2
y =x[1,]
z= unlist(y,use.names = F)
z= paste(as.character(z),sep="", collapse = "")
t= c()
comb = function (i){
  while(i <= 64){
    i= (i+1)
  step1 =x[i,]
  step2= unlist(step1,use.names = F)
  step3= paste(as.character(step2),sep="", collapse = "")
  t = paste(step3,t)
  
  }
  
  
}
comb(0)
