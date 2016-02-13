#' Build Correlation Vectors
#' @return Corr Vectors of all people offline
#' @export
#'
#' @examples
#' build.correlations()

build.correlations <- function(wd, dfs.ratings, N){
  #TODO: Update preferences with book genre instead of comparing tastes with actual books alone
  library(reshape)
  source(paste(wd,"../sim_score_functions.R",sep=""))
  j<-0
  k<-0
  
  R1 <- c()
  R2 <- c()
  Euclidean <- c()
  Pearson <- c()
  Sorensen_Dist <- c()
  Sor_var <- c()
  
  for(j in 1:(N-1)){
    for(k in (j+1):N){
      R1 <- c(R1, j)
      R2 <- c(R2, k)
      Euclidean <- c(Euclidean, euclidean(dfs.ratings[dfs.ratings$Index==j,],dfs.ratings[dfs.ratings$Index==k,]))
      Pearson <- c(Pearson, pearson_corr_score(dfs.ratings[dfs.ratings$Index==j,],dfs.ratings[dfs.ratings$Index==k,]))
      Sorensen_Dist <- c(Sorensen_Dist, sorensen(dfs.ratings[dfs.ratings$Index==j,], dfs.ratings[dfs.ratings$Index==k,]))
      Sor_var <- c(Sor_var, sorensen_variant(dfs.ratings[dfs.ratings$Index==j,], dfs.ratings[dfs.ratings$Index==k,]))
      
      R1 <- c(R1, k)
      R2 <- c(R2, j)
      Euclidean <- c(Euclidean, euclidean(dfs.ratings[dfs.ratings$Index==j,],dfs.ratings[dfs.ratings$Index==k,]))
      Pearson <- c(Pearson, pearson_corr_score(dfs.ratings[dfs.ratings$Index==j,],dfs.ratings[dfs.ratings$Index==k,]))
      Sorensen_Dist <- c(Sorensen_Dist, sorensen(dfs.ratings[dfs.ratings$Index==j,], dfs.ratings[dfs.ratings$Index==k,]))
      Sor_var <- c(Sor_var, sorensen_variant(dfs.ratings[dfs.ratings$Index==j,], dfs.ratings[dfs.ratings$Index==k,]))
      
    }
  }
  
  output.corrs <- data.frame(R1, R2, Euclidean, Pearson, Sorensen_Dist, Sor_var)
  #write.csv(output.corrs,"./all_corr_vectors.csv")
  return(output.corrs)
}

#example
# build_correlations("~/playground/RPlayground/Goodreads/", dfs.ratings)