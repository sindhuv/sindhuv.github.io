k_means <- function (my.matrix, centers = 4, number_of_iter=100, method="mean_square", plot = T, file.location = "D:/rstuff/cluster_plots"){
  #Convert factors to numbers! 
  centroids <- list()
  clusters <- list()
  errors <- list()
  
  erroneous <- FALSE
  centroids_count <- 0
  centroids <- list()
  #If there are NAs, for now they are -1. How do I apply na.action here?
  my.matrix[!is.finite(my.matrix)] <- -1
  
  if (is.numeric(centers)){
    centroids_count <- centers
    # WHAT CAN WE DO TO CHOOSE WISELY HERE?
    centroids <- sample(my.matrix[my.matrix!=-1],centroids_count)
  } else if (is.list(centers)) {
    centroids_count <- length(centers)
    centroids <- centers
  } else{
    errors[[length(errors)+1]] <- "centroids should either be a list of initial centers you want or a number indicating a count of centroids required."
    erroneous <- TRUE
  }
  
  #Check if num of iter is >= 1. ditto with centroids count
  if (number_of_iter <=0){
    errors[[length(errors)+1]] <- paste(number_of_iter,"is not a valid value for iter count. It should be atleast 1 and an integer")
    erroneous <- TRUE
  }
  if (centroids_count >  dim(my.matrix)[1] * dim(my.matrix)[2]){
    errors[[length(errors)+1]] <- paste("Dimensions of matrix",dim(my.matrix)[1],"*", dim(my.matrix)[2],"is lesser than the count of centroids (",centroids_count,"). What kind of a cluster were you actually expecting?")
    erroneous <- TRUE
  }
  
  if(!erroneous){
    #tmp <- matrix(Inf, dim(my.matrix)[1], dim(my.matrix)[2])
    #clusters <- list()
    
    # use Euclidean, etc n all
    for(count in 1:number_of_iter){
      tmp <- matrix(Inf, dim(my.matrix)[1], dim(my.matrix)[2])
      
      for (i in 1:centroids_count){
        tmp <- pmin(tmp,(my.matrix-centroids[i])^2)
      }
      
      for(i in 1:centroids_count){
        clusters[[i]] <- my.matrix[tmp==(my.matrix-centroids[i])^2 & my.matrix != -1]
        centroids[i] <- mean(clusters[[i]])
      }
    }
  }
  
  #Done irrespective of plot requirement criteria
  clusters <- lapply(clusters, function (Element) cbind(Index = 1, ClusterData = Element))
  if (plot){
    if (length(errors) == 0){
      one.big.cluster <- NULL
      #clusters <- lapply(clusters, function (Element) cbind(Index = 1, ClusterData = Element))
      for ( i in 1:length(clusters)) {
        clusters[[i]][,1] <- i
        one.big.cluster <- rbind(one.big.cluster, clusters[[i]])
      }
      png(filename = paste(file.location, "/ClusterCount",centroids_count, ".png",sep=""))
      plot(one.big.cluster, main = paste("Cluster Count ",centroids_count), col = "blue")
      points(centroids, col="black", pch = 46, cex=10)
      dev.off()
    }
  }
  return(list(centroids,clusters, errors))
}