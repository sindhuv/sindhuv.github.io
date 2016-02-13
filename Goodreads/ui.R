library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  # Application title
  titlePanel("GoodReads Compatibility"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      selectInput("user.number", "Choose a user:", 
                  choices = c(1,2,3,4,5,6,7,8,9)),
      actionButton("load.recos", "Recos", width='100px'),
      actionButton("under.the.hoods", "Under the hood", width = '100px'),
      hr()
    ),
    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
      dataTableOutput('user.correlation')
    )
  )
))
