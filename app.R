library(readxl)
library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(ggplot2)
library(markdown)

#setwd("d:/")  ##JF WD Was missing
spareparts <- read_excel("spareparts.xlsx",
                         
                         col_types = c("numeric", "date", "numeric",
                                       
                                       "numeric", "numeric", "text", "text",
                                       
                                       "text", "text", "text"))



# Define UI for application that draws a histogram
ui <- navbarPage("Parts & Material Overview",
  # Application title and theme
  theme = shinytheme("superhero"),
  
  tabPanel("Overview", icon = icon("chart-bar"),
           sidebarLayout(
             sidebarPanel(
               
               #SELECT PRICE RANGE
               sliderInput(
                 
                 "price_range1", "Price",
                 min(spareparts$total_eur), max(spareparts$total_eur),
                 c(1, 2500), pre = "EUR"),
               
               #SELECT DATE RANGE
               dateRangeInput(
                 
                 "date_range1",
                 label = 'Date',
                 start = Sys.Date() - 90, end = Sys.Date(),
                 
               ),
             ),
             mainPanel(
               plotOutput("overview")
             )
           )
  ),
  
  tabPanel("Spare Parts Order", icon = icon("table"),
           sidebarLayout(
             sidebarPanel(
               #SELECT TAIL
               selectInput(
                 
                 "tail_select",
                 "Select an Aircraft Tail",
                 choices = c(spareparts$aircraft_tail)
                 
               ),
               
               #SELECT PRICE RANGE
               sliderInput(
                 
                 "price_range2", "Price",
                 min(spareparts$total_eur), max(spareparts$total_eur),
                 c(1, 2500), pre = "EUR"),
               
               
               
               
               #SELECT DATE RANGE
               dateRangeInput(
                 
                 "date_range2",
                 label = 'Date',
                 start = Sys.Date() - 90, end = Sys.Date(),
                 
               ),
             ),
             mainPanel(
               ("table"), DT::dataTableOutput("table")
             )
           )
  ),
  
  tabPanel("Aircraft plot", icon = icon("chart-bar"),
           sidebarLayout(
             sidebarPanel(
              
      #SELECT PRICE RANGE
      sliderInput(
       
        "price_range3", "Price",
        min(spareparts$total_eur), max(spareparts$total_eur),
        c(1, 2500), pre = "EUR"),
     
      
      
      
      #SELECT DATE RANGE
      dateRangeInput(
       
        "date_range3",
        label = 'Date',
        start = Sys.Date() - 90, end = Sys.Date(),
       
      ),
             ),
             mainPanel(
               plotOutput("price_orders")
             )
           )
  )
  
)     
# Define server logic required to draw a histogram
server <- function(input, output, session){
  filter1 <- reactive(
    filter1 <- subset(spareparts,
                      #aircraft_tail == input$tail_select &
                        total_eur > input$price_range1[1]
                      & total_eur < input$price_range1[2]
                      & as.Date(date) > as.Date(input$date_range1[1])
                      & as.Date(date) < as.Date(input$date_range1[2])
    ) #JF parenthesis was missing
  ) #reactive environment needed, as it is not in rendering environemnt; see here: https://stackoverflow.com/questions/17002160/shiny-tutorial-error-in-r
  output$overview <- renderPlot({
    ggplot( data = filter1(), aes(x = aircraft_tail, y = total_eur)) + geom_bar(stat="identity", fill="steelblue")
    
  })
  
  # table -- DATE FORMAT
  output$table <- DT::renderDataTable({
    
    subset(spareparts,
           aircraft_tail == input$tail_select
           & total_eur > input$price_range2[1]
           & total_eur < input$price_range2[2]
           & as.Date(date) > as.Date(input$date_range2[1])
           & as.Date(date) < as.Date(input$date_range2[2])
           
           
    )}) ##JF Parenthesis missing
  # graph price_orders
  
  filter2 <- reactive(
    filter2 <- subset(spareparts,
                      #aircraft_tail == input$tail_select &
                        total_eur > input$price_range3[1]
                      & total_eur < input$price_range3[2]
                      & as.Date(date) > as.Date(input$date_range3[1])
                      & as.Date(date) < as.Date(input$date_range3[2])
    ) #JF Parenthesis was missing
  )### See above
  output$price_orders <- renderPlot({
    
    ggplot(data = filter2() , aes(x = offer_num, y = total_eur)) + geom_bar(stat="identity", fill="steelblue")
    
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)