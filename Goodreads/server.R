library(shiny)

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  wd <-  "~/playground/RPlayground/Goodreads/"
  corr.vectors <- NULL
  dfs.ratings <- NULL
  N <- 0
  
  stp <- reactiveValues()
  observeEvent(input$user.load.recos.p, load.recommendations.p(input$user.number))
  observeEvent(input$user.load.recos.n, load.recommendations.n(input$user.number))
  observeEvent(input$user.under.hoods, load.corrs(input$user.number))
  
  # # Shows users's correlations with other friends, based on established metrics.
  # output$user.correlation <- renderDataTable({
  #   stp$user.corr.vectors
  # })
  # 
  # # Shows users's recommendations directly
  # output$user.recommendations <- renderDataTable({
  #   stp$user.recos
  # })
  
  output$user.output <- renderDataTable({
    stp$user.op
  })
  
  output$user.text <- renderText({
    stp$text
  })

  retrieve.ratings <- function(){
    #TODO: Integrate with Goodreads. Add a function to retrieve the list of friends & their book ratings.
    source(paste(wd,"reco/accumulate_ratings.R", sep=""))
    return(accumulate.ratings(wd))
  }
  
  load.corrs <- function(n){
    if(is.null(dfs.ratings)){
      fop <- retrieve.ratings()
      dfs.ratings <- fop$ratings
      N <- fop$total
    }
    if(is.null(corr.vectors)){
      source(paste(wd,"reco/build_correlation_vectors.R", sep=""))
      corr.vectors <- build.correlations(wd, dfs.ratings,N)
    }
    stp$user.op <- (corr.vectors[corr.vectors$R1 == n,])
    stp$text <- "Metrics used against your GoodReads friends"
    return(list(cv = corr.vectors, ratings = dfs.ratings))
  }
  
  
  load.recommendations <- function(n){
    if(is.null(corr.vectors)){
      corv <- load.corrs(n)
      dfs.ratings <- corv$ratings
      corr.vectors <- corv$cv
    }
    source(paste(wd,"reco/get_recommendations.R", sep=""))
    return(recommendations(dfs.ratings,n,corr.vectors))
  }
  
  load.recommendations.p <- function(n){
    if(is.null(corr.vectors)){
      recs <- load.recommendations(n)
      stp$user.op <- recs$couldreads[, c('Title', 'Author')]
      stp$text <- "Books you could read right away and probably like"
    }
  }
  
  load.recommendations.n <- function(n){
    if(is.null(corr.vectors)){
      recs <- load.recommendations(n)
      stp$user.op <- recs$maybelaters[, c('Title', 'Author')]
      stp$text <- "You may not like these books"
    }
  }
})