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
    
    output$distPlot <- renderPlot({
      
      #RF = data()$Media.Spend + (input$RetentionFactor * data()$Media.Spend)
      #cbind(data, RF)
      
      data.df <- data
      ggplot(data=data.df,
             aes(x=as.factor(weekID), y=Media.Spend, 
                 group=MediaCampaign, 
                 color=as.factor(MediaCampaign))) + 
        geom_line() + xlab("Weeks") + ylab("Adstock") + 
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