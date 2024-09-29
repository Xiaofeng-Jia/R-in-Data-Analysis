

# STEPS:
#   1 create outputs, 1 for the plot and 1 for a dataTable
#   2 diplay those two outputs
#   3 add the inputs (see final app to see them all)
#   4 format using sidebarLayout()
#   5 EASY: update border and fill colors in plot based on user input
#   6 MEDIUM: update sales metric being plotted based on user input
#   7 HARD: filter for genre, publisher and rating selected, including an "all" option 

library(shiny)
library(ggplot2)
library(dplyr)

# final app here: https://lehmannd.shinyapps.io/games/

# read in and prep data
d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/VideoGameSales2016.csv') %>%
  mutate(Rating = ifelse(Rating=='', 'UNK', Rating),
         Year = as.numeric(Year_of_Release)
         ) %>%
  filter(
    Developer != '',
    !is.na(Year)) %>%
  select(
    Year, Name, Rating, Platform, Genre, Publisher, 
    NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales
  )




ui = fluidPage(
  titlePanel('SOME TITLE HERE'),
  
  sidebarLayout(
    sidebarPanel(
      #genre, publisher, rating, sales   
        selectInput('Genre', 'Select a Genre:', choices = c('ALL', unique(d$Genre))),
        selectInput('Publisher', 'Select a Publisher', choices = c('ALL', unique(d$Publisher))),
        selectInput('Rating', 'Select rating', choices = c('ALL', unique(d$Rating))),
        selectInput('Sales', 'Sales Region:', choices = c('Global' = 'Global_Sales',
                                                          'North America'='NA_Sales',
                                                          'Europe'='EU_Sales',
                                                          'Japan'='JP_Sales',
                                                          'Others'='Other_Sales'))
      ),
        
      mainPanel(
        tabsetPanel(
          
          tabPanel('Sales', plotOutput('sales_bar')),
          tabPanel('Source Data', dataTableOutput('data'))
        )#tabsetPanel
        
    ) #sidebarPanel
    
  ) #sidebarlayout
) #ui

  

server = function(input, output){
  
  #barplot of sales by year
  output$sales_bar = renderPlot({
    
    rows = TRUE
    if(input$Genre != 'ALL') rows = rows & d$Genre == input$Genre
    if(input$Publisher != 'ALL') rows = rows & d$Publisher == input$Publisher
    if(input$Rating != 'ALL') rows = rows & d$Rating == input$Rating
    
    d[rows, ] %>%
      
      
      group_by(Year) %>%
      summarise(
        NA_Sales = sum(NA_Sales),
        EU_Sales = sum(EU_Sales),
        JP_Sales = sum(JP_Sales),
        Other_Sales = sum(Other_Sales),
        Global_Sales = sum(Global_Sales)
      ) %>%
      ggplot( aes_string (x='Year', y=input$Sales) ) +  #important step!!!
      
      geom_col(fill='darkorange', col='darkgreen') +
      scale_x_continuous(limits=c(1985, 2016), breaks=1985:2016) +
      scale_y_continuous(limits=c(0, 650)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x='Release Year', y='Sales (million of units)')
    
  })
  
  
  
  output$source_data = renderDataTable(d)
  
}

shinyApp(ui, server)

