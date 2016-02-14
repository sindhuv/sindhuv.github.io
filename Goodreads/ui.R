library(shiny)
library(shinyBS)

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
      hr(),
      actionButton("user.load.recos.p", "Recos", width='100px'),
      actionButton("user.load.recos.n", "Non Recos", width='100px'),
      br(),
      br(),
      actionButton("user.under.hoods", "Under the hood", width = '200px')
    ),
    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
      verbatimTextOutput('user.text'),
      dataTableOutput('user.output')
    )
  )
))
