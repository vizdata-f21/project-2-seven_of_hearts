#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
stat_data <-  read_csv(here::here("data/stat_data.csv"))
cs_data <- read_csv(here::here("data/CS_data.csv"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               "Computer Science" = cs_data%>%select("Subject", "Class Number", "Start Time",
                                                     "End Time"),
               "Statistics" = stat_data%>%select("Subject", "Class Number", "Start Time",
                                                 "End Time"),
               "Economics" = cars)
    })


    Dataframe2 <- reactive({
        mtcars[,input$Columns]
    })
    output$dfStr <- renderPrint({
        str(Dataframe2())
    })
    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })

})
