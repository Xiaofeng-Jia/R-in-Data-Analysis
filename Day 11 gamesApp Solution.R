

# STEPS:
#   X create outputs, 1 for the plot and 1 for a dataTable
#   X diplay those two outputs
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
  sidebarLayout(
    sidebarPanel(
      selectInput('Genre', 'Select a Genre:', choices = c('All', unique(d$Genre)) ),
      selectInput('Publisher', 'Select a Publisher:', choices = c('All', unique(d$Publisher)) ),
      selectInput('Rating', 'Select a Rating:', choices = c('All', unique(d$Rating)) ),
      selectInput('Sales', 'Sales Region:', 
                  choices = c('Global Sales'='Global_Sales',
                              'North America'='NA_Sales',
                              'Europe'='EU_Sales',
                              'Japan'='JP_Sales',
                              'Other'='Other_Sales'
                              )
      ),
      selectInput('Color', 'Color?', choices=colors(), selected='darkorange')
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel('Sales by Year', plotOutput('sales_by_year')),
        tabPanel('Data', dataTableOutput('data'))
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage


server = function(input, output){
  

  # output 1: sales by time ggplot
  output$sales_by_year = renderPlot({
    
    rows = TRUE
    if(input$Genre != 'All') rows = rows & d$Genre == input$Genre
    if(input$Publisher != 'All') rows = rows & d$Publisher == input$Publisher
    if(input$Rating != 'All') rows = rows & d$Rating == input$Rating
    
    d[rows, ] %>%
      group_by(Year) %>%
      summarise(
        Global_Sales = sum(Global_Sales),
        NA_Sales = sum(NA_Sales),
        EU_Sales = sum(EU_Sales),
        JP_Sales = sum(JP_Sales),
        Other_Sales = sum(Other_Sales)
      ) %>%
      ggplot(aes_string(x='Year', y=input$Sales) ) + 
        geom_col(fill=input$Color, col='darkgreen') +
        scale_x_continuous(limits=c(1985, 2016), breaks=1985:2016) +
        scale_y_continuous(limits=c(0, 650)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(x='Release Year', y='Sales (million of units)')
  })
  
  
  
  # output 2: full data
  output$data = renderDataTable(d)
}

shinyApp(ui, server)

