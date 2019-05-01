library(tm)
library(data.tree)

Entropy <- function(data) {
  P <- data/sum(data)
  Entropy <- P * log2(P)
  Entropy[data == 0] <- 0
  -sum(Entropy)
}

#"чистота" определяет, один класс или нет
Purity <- function(table){
  length(unique(table[,ncol(table)]))==1
}

#мера информационного выиграша определяется как разность 
#энтропии для исходного набора данных и суммы энтропий всех фрагментов после разбиения
Gain <- function(table){
  EntropyBefore <- Entropy(colSums(table))
  row_sum <- rowSums(table)
  EntropyAfter <- sum(row_sum/sum(row_sum)*apply(table, MARGIN = 1, FUN = Entropy))
  IGain <- EntropyBefore - EntropyAfter
  return(IGain)
}

ID3 <- function(node, data) {
  
  if (Purity(data)) {
    child <- node$AddChild(unique(data[,ncol(data)]))
    } 
  else {
    ig <- sapply(colnames(data)[-ncol(data)], function(x) Gain(table(data[,x], data[,ncol(data)])))

    feature <- names(which.max(ig))
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], data[ ,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      child <- node$AddChild(names(childObs)[i])
      ID3(child, childObs[[i]])
    }
  }
}

Lenses <- read.table("lenses.txt", sep = "\t", check.names = FALSE, header = TRUE)

tree <- Node$new("astegmatism")

ID3(tree, Lenses)
print(tree)
plot(tree)
plot(as.dendrogram(tree))

SetEdgeStyle(tree, color = "blue")
SetNodeStyle(tree, style = "filled, rounded", shape = "box", fillcolor = "red")

