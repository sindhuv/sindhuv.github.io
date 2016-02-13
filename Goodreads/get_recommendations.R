recommendations <- function(dfs.ratings, person.index, output.corrs){ ## return recommendations of titleids!
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
