install.packages("stringr")
greedyalgorithm <- rbind( c(0, 4, 6, 2, 8, 2, 2, 8, 2),
                          c(3, 0, 5, 1, 7, 1, 2, 3, 5),
                          c(8, 2, 0, 4, 6, 3, 2, 6, 2),
                          c(2, 1, 2, 0, 3, 2, 1, 1, 1),
                          c(4, 4, 4, 6, 0, 2, 7, 2, 5),
                          c(3, 8, 3, 1, 1, 0, 1, 0, 1),
                          c(4, 9, 8, 2, 4, 5, 0, 7, 4),
                          c(3, 3, 2, 2, 5, 7, 4, 0, 4),
                          c(1, 2, 0, 1, 8, 2, 8, 4, 0))

greedy
result <<- c()
ax <<- c()
ay <<- c()
a<<- c()
b <<- c()
x <<- 1
exceptx <<- c()
excepty <<- c()



galg <<- function(){
  greedy <<- greedyalgorithm
  result <<- c()
  ax <<- c()
  ay <<- c()
  a<<- c()
  b <<- c()
  x <<- 1
  exceptx <<- c()
  excepty <<- c()
  a<<-max(greedy[1,],
          greedy[2,],
          greedy[3,],
          greedy[4,],
          greedy[5,],
          greedy[6,],
          greedy[7,],
          greedy[8,],
          greedy[9,])
  result <<- append(result,a)
  b <<- which(greedy == a, arr.ind = T)
  print(b)
  ax <<-  b[1,1]
  ay <<-  b[1,2]
  print("hi")
  rowvscol()
  print(result)
}

rowvscol <<- function(){ #durma kodu ekle
  if(x<=7) {
    x <<- x+1
    if(max(greedy[,ax])>max(greedy[ay,])){
      exceptx <<- append(exceptx,ax)
      a <<- max(greedy[-exceptx,ax])
      result <<- append(result,a)
      greedy[ax,] = 0
      b <<- which(greedy[,ax] == a, arr.ind = T)
      ax <<-  b
      print(greedy)
      rowvscol()
    }
    else{
      excepty <<- append(excepty, ay)
      a <<- max(greedy[ay,-excepty])
      result <<- append(result,a)
      greedy[,ay] = 0
      b <<- which(greedy[ay,] == a, arr.ind = T)
      ay <<-  b
      print(greedy)
      rowvscol()
    }
  }
  else{
  }
}
galg()
