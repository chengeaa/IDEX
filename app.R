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

orghistory = read_csv("Total Organization History Data.csv")
donations = read_csv("Total Donations.csv")
camps = read_csv("Total Campaigns.csv")
merge1 = merge(donations, camps, by.x = "Campaign", by.y = "Campaign Name")
merge2 = merge(merge1, orghistory, by.x = "Organization Name", by.y = "Organization Name")

aggfuncs = vector(mode = "list", length = 4)
names(aggfuncs) = c("Mean", "Max", "Min", "Sum")
aggfuncs[[1]] = mean
aggfuncs[[2]] = max
aggfuncs[[3]] = min
aggfuncs[[4]] = sum

ui <- fluidPage(
  
   titlePanel("IDEX data subsetting"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput(
           inputId = "groupLabel",
           label = "Group by:",
           choices = c("all", names(merge2)),
           selected = "all"
         ),
         selectInput(
           inputId = "agg",
           label = "Aggregate:",
           choices = c("None", names(aggfuncs)),
           selected = "None"
         )
         , width = 2
      ),
      mainPanel(
        dataTableOutput('mytable')
      )
   )
)

server <- function(input, output) {
  observeEvent(c(input$groupLabel, input$agg),{
    if(input$groupLabel != "all"){
      index = match(input$groupLabel, names(merge2))
      df <- data.frame(merge2[,index])
      names(df) <- input$groupLabel
      
        if(input$agg != "None"){
          aggregator = match(input$agg, names(aggfuncs))
          agglbl = paste(input$agg, "of Donations")
          
          
          df = aggregate(x = merge2$Amount, 
                         by = list(merge2[,index]), 
                         FUN = aggfuncs[[aggregator]])
          names(df)[names(df) == "x"] = agglbl
          
        } else {
          df <- data.frame(unique(merge2[,index]))
          names(df) <- input$groupLabel
        }
      
      output$mytable = renderDataTable(df)
    } else {
      output$mytable = renderDataTable(merge2)
      
    }
    
    
  })

}

shinyApp(ui = ui, server = server)

