source("//Users//ashwinichowdhary//Documents//Fall2016//BA//percentMap.R")

restuarantData <- read.csv("//Users//ashwinichowdhary//Documents//Fall2016//BA//RestaurantData.csv")

library(shiny)
library(shinydashboard)
require(maps)
library(shinythemes)
library(rpart)
library(rpart.plot)

#User Interface implementation

ui <-dashboardPage(skin = "purple",

 dashboardHeader(title="Interactive Visualization"),
  dashboardSidebar(),
 dashboardBody(
  sidebarLayout(
    sidebarPanel(
      helpText("Allocation of Drivers on Map"),
      
      selectInput("var", 
                  label = "Choose a Driver to view on Map",
                  choices = c("Discount Driver", "Healthy Driver",
                              "Taste Driver", "Convenient Driver", "Price Driver"),
                  selected = "Price Share"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      
      fluidRow(
        fluidPage(
        
        column(12, offset = 1,
               selectInput("var1", 
                           label = "Choose a variable to view on the Decision Tree",
                           choices = c("Discount Share", "Healthy Share",
                                       "Taste Share", "Convenient Share", "Price Share"),
                           selected = "Price Share")
               
        )

     ))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Walkthrough", dataTableOutput("table")),
        tabPanel("Percent Share Maps", plotOutput("map")),
        tabPanel("Decision Tree", plotOutput("plot"), click = "plot_click")
        
      )
      
    )
  )
  ))


percent_map <- function(var, color, legend.title, min = 0, max = 100) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}

#Server side Implementation

server <- function(input, output) {
  
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Discount Driver" = list(restuarantData$DriverShare_discounts, "darkgreen", "% Discount"),
                   "Healthy Driver" = list(restuarantData$DriverShare_healthy, "black", "% Healthy"),
                   "Taste Driver" = list(restuarantData$DriverShare_taste, "darkorange", "% Taste"),
                   "Convenient Driver" = list(restuarantData$DriverShare_convenient, "darkviolet", "% Convenient"),
                   "Price Driver" = list(restuarantData$DriverShare_price,"blue","% Price"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
  output$table <-renderDataTable({
    restuarantData 
  }, options = list(scrollX = TRUE))
  
  output$plot <- renderPlot({
    
    args1 <- switch(input$var1,
                    "Discount Share" = restuarantData$DriverShare_discounts,
                    "Healthy Share" = restuarantData$DriverShare_healthy,
                    "Taste Share" = restuarantData$DriverShare_taste,
                    "Convenient Share" = restuarantData$DriverShare_convenient,
                    "Price Share" = restuarantData$DriverShare_price)
 
   d <-rpart(args1~DMA_Total_Brand_Revenue+DMA_Total_Restaurant_Market+Visits+Satisfaction_MeetExpectations_Percentile_Rank +Total_Current0Customer+
             HHI_200000 +HHI_150000_199999+HHI_less10000 +HHI_35000_49999+DMA_AUV+DMA_Revenue_Per_Capita+Market_Share,method="class",data=restuarantData)
  rpart.plot(d,type = 1, extra=2,box.palette=0,box.col="cyan2",shadow.col = "darkgray")

    
  })

}

shinyApp(ui = ui, server = server)
