euclidean <- function (df1, df2, groupBy='BookId') 
  #each data frame contains 2 columns: Data to be rated, rating. Each df corresponds to a user.
{
  score <- 0.0
  dfs <- merge(df1,df2,by.x=groupBy, by.y = groupBy)
  if(nrow(dfs) == 0){
    return(0)
  }
  score <- 1.0/(1+sum((dfs$Rating.x-dfs$Rating.y)^2))
  return(score)
}

pearson_corr_score <- function(df1,df2,groupBy='BookId'){
  score <- 0.0
  dfs <- merge(df1,df2,by=groupBy)
  #print(dfs)
  n <- nrow(dfs)
  if (n == 0)
    return(score)
  s1 <- sum(df1[df1$BookId %in% dfs$BookId,]$Rating)
  s2 <- sum(df2[df2$BookId %in% dfs$BookId,]$Rating)
  ps1s2 <- sum(df2[df2$BookId %in% dfs$BookId,]$Rating*df1[df1$BookId %in% dfs$BookId,]$Rating)
  s1Sq <- sum(df1[df1$BookId %in% dfs$BookId,]$Rating^2)
  s2Sq <- sum(df2[df2$BookId %in% dfs$BookId,]$Rating^2)
  t <- (ps1s2 - s1*s2/n)
  b <- sqrt((s1Sq-(s1^2)/n)*(s2Sq-(s2^2)/n))
  if (b == 0)
    return(score)
  score = t*1.0/b
  return(score)
}

sorensen <- function(df1, df2, groupBy = 'BookId'){
  dfs <- merge(df1,df2,by=groupBy)
  score <- 2*nrow(dfs)/(nrow(df1)+nrow(df2))
  return(score)
}

sorensen_variant <- function(df1, df2, groupBy = 'BookId'){
  dfs <- merge(df1,df2,by=groupBy)
  n = nrow(dfs)
  if(n == 0)
    return(0)
  s1In <- sum(df1[df1$BookId %in% dfs$BookId,]$Rating)
  s2In <- sum(df2[df2$BookId %in% dfs$BookId,]$Rating)
  s1 <- sum(df1$Rating)
  s2 <- sum(df2$Rating)
  score <- (s1In+s2In)/(s1+s2)
  return(score)
}