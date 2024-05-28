library(readr)
library("stringr")
bacteria1 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/bacteria1.txt", col_names = FALSE)
bacteria2 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/bacteria2.txt", col_names = FALSE)
bacteria3 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/bacteria3.txt", col_names = FALSE)
bacteria4 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/bacteria4.txt", col_names = FALSE)
virus1 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/virus1.txt", col_names = FALSE)
virus2 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/virus2.txt", col_names = FALSE)
virus3 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/virus3.txt", col_names = FALSE)
virus4 <- read_csv("C:/Users/COmerSaglam/Downloads/sequences/virus4.txt", col_names = FALSE)

g1 = c("A", "G", "C", "T")
g2 = c("A", "G", "C", "T")
doublegrid = expand.grid(g1,g2)


freq = function(filetxt){
  i= 0
  list1 = c()
  while(i <= 16){
    rows = doublegrid[i,]
    rows = unlist(rows, use.names = F)
    doubles = paste(as.character(rows) , sep = "", collapse ="")
    n = str_count(filetxt, doubles)
    list1[[length(list1)+1]] = n
    i = (i+1)
  }
  return(list1)
}

bact1 = freq(bacteria1)
bact1 = tail(bact1, -1)

bact2 = freq(bacteria2)
bact2 = tail(bact2, -1)

bact3 = freq(bacteria3)
bact3 = tail(bact3, -1)

bact4 = freq(bacteria4)
bact4 = tail(bact4, -1)

virus1 = freq(virus1)
virus1 = tail(virus1, -1)

virus2 = freq(virus2)
virus2 = tail(virus2, -1)

virus3 = freq(virus3)
virus3 = tail(virus3, -1)

virus4 = freq(virus4)
virus4 = tail(virus4, -1)

df_ta = do.call(rbind, Map(data.frame, bact1=bact1, bact2=bact2, bact3=bact3, bact4=bact4,
                           virus1=virus1, virus2=virus2, virus3=virus3, virus4=virus4))
rownames(df_ta) <- c("AA","GA","CA","TA","AG","GG","CG","TG","AC","GC","CC","TC","AT","GT","CT","TT")

maxdf = max(df_ta)

print(df_ta)
print(maxdf)

