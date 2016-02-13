accumulate.ratings <- function(wd){
  dfs.ratings.full.details <- list()
  dfs.ratings <- data.frame(BookId=numeric(0),Title=character(0),Author=character(0),Rating=double(0), Index = numeric(0))
  #TODO: Integrate with goodreads API to get the ratings.
  ratings.files <- list.files(path=paste(wd,"all_ratings/",sep=""),pattern="*.csv", full.names = TRUE)
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
  return(list(ratings = dfs.ratings,total = N))
}

#example

#accumulate.ratings("~/playground/RPlayground/Goodreads/")