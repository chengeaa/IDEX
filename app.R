#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

install.packages('shiny')
install.packages('readr')
install.packages('lubridate') #run these 3 lines when you are using this app for the first time
#Use this hashtag to comment out the lines above once you have installed the packages

library(shiny)
require(readr)
library(lubridate)

contacts = read_csv("contacts_cleaned copy.csv")
contacts <- mutate(contacts, name = paste(First_Name, Last_Name)) 
contacts1 <- contacts[, c(11,4:10)]


merge1 = merge(donations, camps, by.x = "Campaign", by.y = "Campaign Name")
merge2 = merge(merge1, orghistory, by.x = "Organization Name", by.y = "Organization Name")
merge2 = merge2[1:32161,]
merge2$`Close Date` = as.Date(merge2$`Close Date`, format = "%m/%d/%Y") + years(2000)
merge2 = left_join(merge2, contacts1, by = c("Organization Name" = "name"))



uniquedates = (unique(merge2$`Close Date`))
ordereddates = uniquedates[order(uniquedates)]

contactinfo = names(orghistory)
aggfuncs = vector(mode = "list", length = 4)
names(aggfuncs) = c("Mean", "Max", "Min", "Sum")
aggfuncs[[1]] = mean
aggfuncs[[2]] = max
aggfuncs[[3]] = min
aggfuncs[[4]] = sum

ui = navbarPage(
  "IDEX Data Cruncher",
tabPanel(
  "Table page",
  titlePanel("IDEX data subsetting"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "startDate",
        label = "Select a start date",
        choices = ordereddates,
        selected = ordereddates[1]
      ),
      selectInput(
        inputId = "endDate",
        label = "Select an end date",
        choices = ordereddates,
        selected = ordereddates[length(ordereddates)]
      ),
      selectInput(
        inputId = "columns",
        label = "Select columns to display:",
        choices = c("all", names(merge2)),
        selected = "all",
        multiple = TRUE
      ),
      selectInput(
        inputId = "groupLabel",
        label = "Aggregate by:",
        choices = c("all", names(merge2)),
        selected = "all"
      ),
      selectInput(
        inputId = "agg",
        label = "Aggregate function:",
        choices = c("None", names(aggfuncs)),
        selected = "None"
      ),
      h3("Export CSV"),
      h5("Enter a valid file name, then click export"),
      textInput(
        inputId = "filename",
        label = "File name:"
      ),
      actionButton(
        inputId = "generate",
        label = "Export"
      )
      , width = 2
    ),
    mainPanel(
      dataTableOutput('mytable')
    ),
    fluid = TRUE
  )
  
),
tabPanel( "Vis page"
  ##visualization goes here.
  
)
   
)


server <- function(input, output) {
  newtable = reactiveValues()
  
  df= merge2
  newtable$table = df
  
  observeEvent(c(input$endDate, input$startDate),
               {
                 
                 start = as.Date(input$startDate, format = "%Y-%m-%d")
                 end = as.Date(input$endDate, format = "%Y-%m-%d")
                 print(start)
                 print(end)
                 df = newtable$table[newtable$table$`Close Date` > start & newtable$table$`Close Date` < end,]
                 output$mytable = renderDataTable(df)
                 newtable$table = df
               })
  
  observeEvent(input$columns, {
    if("all" %in% input$columns && length(input$columns) == 1){
      df = merge2
    } else {
      incols = input$columns
      present = names(merge2) %in% incols
      cols = which(present)
      df = data.frame(merge2[,cols])
      
    }
    output$mytable = renderDataTable(df)
    newtable$table = df
  })
  
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
      output$mytable = renderDataTable(df)
      
    }
    
  output$mytable = renderDataTable(df)
  newtable$table = df
  })
  
  
  observeEvent(input$generate, {
    if(input$filename == "" || grepl("[/\\:*?\"<>|]", input$filename)){
      print(input$filename)
      print(grepl("/\\:*?\"<>|", input$filename))
      showModal(
        modalDialog(
          title = "Invalid file name",
          "Provide a file name; the following characters may not be used: /\\:*?\"<>|",
          easyClose = TRUE
        )
      )
    } else {
      
      tryCatch(
        {
          write.csv(newtable$table, file = paste(sub("\\s+$", "", input$filename), ".csv",sep = ""))
          showModal(
            modalDialog(
              title = "Success",
              paste("Table saved to ", sub("\\s+$", "", input$filename), ".csv"),
              easyClose = TRUE
            )
          )
        },
        error = function(cond){
          showModal(
            modalDialog(
              title = "Oops",
              "Save unsuccessful",
              easyClose = TRUE
            )
          )
        }
      )
    }
  }
  )
}

shinyApp(ui = ui, server = server)

