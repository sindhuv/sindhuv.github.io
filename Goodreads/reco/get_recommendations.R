#' Title
#'
#' @param dfs.ratings 
#' @param person.index 
#' @param output.corrs 
#' @param required.reco.count 
#' @param max.metric.reject 
#' @param min.metric.reject 
#' @param metic 
#'
#' @return
#' @export
#'
#' @examples
recommendations <- function(dfs.ratings, person.index, output.corrs, 
                            candidate.choice.count = FALSE,
                            required.reco.count = 5, 
                            max.metric.reject = 0.3, min.metric.reject = 0.7, 
                            metic = "Pearson"){ 
  ## return recommendations of titleids!
  reco.books <- dfs.ratings[dfs.ratings$Index != person.index, ]
  reco.books <- merge(reco.books, output.corrs, by.x =c("Index"), by.y = c("R1"),all.x=TRUE)
  reco.books <- reco.books[reco.books$R2==person.index,]
  
  pivoted.ratings <- cast(reco.books,BookId~Index, value="Rating")
  pivoted.pearson <- cast(reco.books,BookId~Index, value=metric)
  pivoted.prod <- pivoted.ratings*pivoted.pearson
  
  op <- c()
  for (i in 1:nrow(pivoted.prod)){ 
    op <- c(op, sum(pivoted.prod[i,2:9], na.rm=TRUE)/sum(pivoted.pearson[i,2:9], na.rm = TRUE))
  }
  pivoted.prod$Value <- op
  reco.book.details <- na.omit(dfs.ratings[,c("BookId","Title","Author")])
  reco.book.details <- unique(reco.book.details)
  output <- merge(reco.book.details,pivoted.prod[,c("BookId","Value")],by="BookId",all.x = TRUE)
  remove(pivoted.prod, pivoted.pearson, pivoted.ratings)
  return(list(couldreads = head(output,required.reco.count), maybelaters = tail(output, required.reco.count)))
}

## Example Usage:
#recommendations(dfs.ratings, YOU, output.corrs)