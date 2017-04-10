#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("IDEX data subsetting"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(
           inputId = "groupLabel",
           label = "Group by:"
         ), width = 1
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput('mytable')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  orghistory = read_csv("Total Organization History Data.csv")
  donations = read_csv("Total Donations.csv")
  camps = read_csv("Total Campaigns.csv")
  merge1 = merge(donations, camps, by.x = "Campaign", by.y = "Campaign Name")
  merge2 = merge(merge1, orghistory, by.x = "Organization Name", by.y = "Organization Name")
  
  output$mytable = renderDataTable({
    merge2
    # read_csv("C://Users/Erik Cheng/IDEX/ugly.csv")
    #mtcars
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

