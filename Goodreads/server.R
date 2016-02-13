library(shiny)

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  wd <-  "~/playground/RPlayground/Goodreads/"
  corr.vectors <- NULL
  dfs.ratings <- NULL
  N <- 0
  
  stp <- reactiveValues()
  observeEvent(input$load.recos, load.recommendations(input$user.number))
  observeEvent(input$under.the.hoods, load.corrs(input$user.number))
  
  # Shows users's correlations with other friends, based on established metrics.
  output$user.correlation <- renderDataTable({
    stp$user.corr.vectors
  })
  
  # Shows users's recommendations directly
  output$user.recommendations <- renderDataTable({
    stp$user.recos
  })

  retrieve.ratings <- function(){
    #TODO: Integrate with Goodreads. Add a function to retrieve the list of friends & their book ratings.
    source(paste(wd,"reco/accumulate_ratings.R", sep=""))
    fop <- accumulate.ratings(wd)
    dfs.ratings <- fop$ratings
    N <- fop$total
  }
  
  load.corrs <- function(n){
    if(is.null(dfs.ratings)){
      retrieve.ratings()
    }
    if(is.null(corr.vectors)){
      source(paste(wd,"reco/build_correlation_vectors.R", sep=""))
      corr.vectors <- build.correlations(wd, dfs.ratings,N)
    }
    stp$user.corr.vectors <- (corr.vectors[corr.vectors$R1 == n,])
  }
  
  
  load.recommendations <- function(n){
    if(is.null(corr.vectors)){
      load.corrs(n)
    }
    source(paste(wd,"reco/get_recommendations.R", sep=""))
    stp$user.recos <- recommendations(dfs.ratings,n,corr.vectors)
  }
  
})