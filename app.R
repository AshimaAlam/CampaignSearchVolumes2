library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

data <- read.csv("data.csv")

ui <- fluidPage(
  titlePanel("Campaign Search Volumes"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "RetentionFactor", label = "Retention Factor ", min = 1,max = 10, value= 1,step = 0.1),
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput(outputId ="stats"),
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    data.df <- data
    ggplot(data=data.df,
           aes(x=as.factor(weekID), y=Media.Spend, 
               group=MediaCampaign, 
               color=as.factor(MediaCampaign))) + 
      geom_line() + xlab("Weeks") + ylab("Adstock") + 
      theme_bw() + scale_color_manual(values=c("red", "blue","green"))
  }) 
  
  
}

shinyApp(ui = ui, server = server)