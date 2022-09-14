gene_diversity <- function(samples,clusters,genes) {
  #Random Selection of two isolates in a group
  samples <- sample(samples,2)
  sample.1 <- samples[1]
  sample.2 <- samples[2]
  
  #Create counter variables for protein families unique and shared
  shared <- 0
  unique <- 0
  empy <- 0
  
  for(row in 1:nrow(clusters)){
    proteins <- clusters[row,]
    genes_a <- genes[genes$Gene %in% proteins,sample.1]
    genes_b <- genes[genes$Gene %in% proteins,sample.2]
    
    if(all(is.na(genes_a)) == TRUE & all(is.na(genes_b)) == TRUE){
      empy <- empy + 1
      next
    }
    
    if(all(is.na(genes_a)) == FALSE & all(is.na(genes_b)) == FALSE){
      shared <- shared + 1
    }
    else if(all(is.na(genes_a)) == FALSE & all(is.na(genes_b)) == TRUE){
      unique <- unique + 1
    }
    else if(all(is.na(genes_a)) == TRUE & all(is.na(genes_b)) == FALSE){
      unique <- unique + 1
    }
  }
  return(unique/(shared + unique))
}

genome_fluidity <- function(samples,clusters,genes) {
  x <- list(NA)
  for(i in 1:length(samples)){
    append(x,gene_diversity(samples,clusters,genes)) -> x
  }
  x <- x[-1]
  return(x)
}