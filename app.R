library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(plotly)

  data <- read.csv("data.csv")
  
  data$index <- 1:nrow(data)
  data
  
  ui <- fluidPage(
    titlePanel("Campaign Search Volumes"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "RetentionFactor", label = "Retention Factor ", min = 0,max = 1, value= 0,step = 0.1),
      ),
      
      mainPanel(
        plotlyOutput(outputId = "distPlot"),
        tableOutput("TbEfficiencies"),
      )
    )
  )
  
  server <- function(input, output) {
    
    MC1 <- reactive({
      data %>% filter(data$MediaCampaign=="1")
    })
    MC2 <- reactive({
      data %>% filter(data$MediaCampaign=="2")
    })    
    MC3 <- reactive({
      data %>% filter(data$MediaCampaign=="3")
    })
    
    output$distPlot <- renderPlotly({
      myAdstock <- 0.0;
      AdstockIterative <- function(MSpend,RF,Week)
      {
        myAdstock <- MSpend + RF * myAdstock
        return(myAdstock)
      }
      
      AdstockRecursive  <- function(MSpend,RF,week)
      {
        #return(MSpend + (RF * AdstockIterative(MSpend,RF, week-1)))
        return(MSpend + (RF * lag(AdstockIterative(MSpend,RF, week))))
      }  
      
      data$Adstock = AdstockRecursive(data$Media.Spend,input$RetentionFactor,data$index)
      Weeks<- as.factor(data$weekID)
      Campaign <- as.factor(data$MediaCampaign)
      ggplot(data,aes(x=Weeks, y=Adstock, group=MediaCampaign, color=Campaign)) + 
      geom_line() + xlab("Week number per Campaign") + ylab("Adstock") +
      theme_bw() + scale_color_manual(values=c("red", "blue","green"))
    })
    
    Values <- reactive({
      
      data.frame(
        Name = c("Highest Search Volume",
                 "Lowest Search Volume",
                 "Average Search Volume"),
        Campaign1 = as.character(c(paste(max(MC1()$Media.Spend)),
                                   paste(min(MC1()$Media.Spend)),
                                   paste(round(mean(MC1()$Media.Spend,digits=2)))
        )),
        Campaign2 = as.character(c(paste(max(MC2()$Media.Spend)),
                                   paste(min(MC2()$Media.Spend)),
                                   paste(round(mean(MC2()$Media.Spend,digits=2)))
        )),
        Campaign3 = as.character(c(paste(max(MC3()$Media.Spend)),
                                   paste(min(MC3()$Media.Spend)),
                                   paste(round(mean(MC3()$Media.Spend,digits=2)))
        )),        
        stringsAsFactors = FALSE)
      
    })
    
    output$TbEfficiencies <- renderTable({
      Values()
    })    
  }
  
  shinyApp(ui = ui, server = server)