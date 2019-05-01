library(tm)

Entropy <- function(x){
  Entropy <-x/sum(x)*log2(x/sum(x))
  Entropy[x==0] <- 0
  -sum(Entropy)
}

Purity <- function(table){
  length(unique(table[,ncol(table)]))==1
}

Gain <- function(table){
  EntropyBefore <- Entropy(colSums(table))
  
  row_sum <- rowSums(table)
  EntropyAfter <- sum(row_sum/sum(row_sum)*apply(table, MARGIN = 1, FUN = Entropy))
  IGain <- EntropyBefore - EntropyAfter
  return(IGain)
}

Lenses <- read.table("lenses.txt", sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, header = TRUE)

if(Purity(Lenses)) print("+") else print("-")

