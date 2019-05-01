library(tm)

Entropy_class <- function(table){
  Entropy <- integer()
  for(i in 1: length(table)){
    p <- table[i]/sum(table)
    Entropy <- append(Entropy, -(p*log2(p)))
  }
  Entropy <- sum(Entropy)
  return(Entropy)
}

Lenses <- read.table("lenses.txt", sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, header = TRUE)

Class_lenses <- table(Lenses$contactlenses)

Entropy_class(Class_lenses)

P_table <- data.frame()
