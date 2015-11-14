library(reshape)

recommendations <- function(dfs.ratings, person.index, output.corrs, sim.func='Pearson' ){ ## return recommendations of titleids!
  reco.books <- dfs.ratings[dfs.ratings$Index != person.index, ]
  reco.books <- merge(reco.books, output.corrs, by.x =c("Index"), by.y = c("R1"),all.x=TRUE)
  reco.books <- reco.books[reco.books$R2==person.index,]
  
  pivoted.ratings <- cast(reco.books,BookId~Index, value="Rating")
  pivoted.pearson <- cast(reco.books,BookId~Index, value="Pearson")
  pivoted.prod <- pivoted.ratings*pivoted.pearson
  
  op <- c()
  for (i in 1:nrow(pivoted.prod)){ 
    op <- c(op, sum(pivoted.prod[i,2:9], na.rm=TRUE)/sum(pivoted.pearson[i,2:9], na.rm = TRUE))
  }
  pivoted.prod$Value <- op
  #output <- pivoted.prod[,c("BookId","Value")]
  reco.book.details <- na.omit(dfs.ratings[,c("BookId","Title","Author")])
  reco.book.details <- unique(reco.book.details)
  output <- merge(reco.book.details,pivoted.prod[,c("BookId","Value")],by="BookId",all.x = TRUE)
  remove(pivoted.prod, pivoted.pearson, pivoted.ratings)
  return(output)
}

remove(dfs.ratings)
remove(i)

old_dir <- getwd()
setwd('D:/rstuff/building_sample_data/')
dfs.ratings.full.details <- list()
dfs.ratings <- data.frame(BookId=numeric(0),Title=character(0),Author=character(0),Rating=double(0), Index = numeric(0))
ratings.files <- list.files(pattern="*.csv")
i<-0
N <- length(ratings.files)
for(i in 1:N)
{
  df <- read.csv(ratings.files[i], encoding = 'UTF-8')
  df$Index <- i
  if(colnames(df)[1] == "X.U.FEFF.BookId"){ #Lazy UTF encoding hack
    names(df)[1] <- "BookId"
  }
  dfs.ratings <- rbind(dfs.ratings, df)
}
dfs.ratings <- na.exclude(dfs.ratings)
j<-0
k<-0

R1 <- c()
R2 <- c()
Euclidean <- c()
Pearson <- c()
Sorensen_Dist <- c()
Sor_var <- c()

SINDHU <- -1

for(j in 1:(N-1)){
  if(!is.na(pmatch("sindhuja", ratings.files[j]))){
    SINDHU <- j
  }
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

if(!is.na(pmatch("sindhuja", ratings.files[N]))){
  SINDHU <- N
}

output.corrs <- data.frame(R1, R2, Euclidean, Pearson, Sorensen_Dist, Sor_var)
write.csv(output.corrs,"D:/output.csv")
setwd(old_dir)
remove(i,j,k,N,df, Pearson, Euclidean, R1, R2)
recommendations(dfs.ratings, SINDHU, output.corrs)