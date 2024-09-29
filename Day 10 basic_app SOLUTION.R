

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)

d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/superstore.csv') %>%
  mutate(
    Date = mdy(Date),
    Year = year(Date)
  )


###############################################
# USER INTERFACE (ALL THE INPUT/OUTPUT STUFF) #
###############################################

ui = fluidPage(
  
  selectInput('sales_year', 'Sales Year', choices=c('All', 2016, 2017, 2018, 2019)),
  selectInput('bar_color', 'Bar Color', choices=colors(), selected='darkorange'),
  selectInput('bar_border', 'Border Color', choices=colors(), selected='black'),
  plotOutput('sales_bar'),
  
  selectInput('xvar', 'X-Axis Variable', choices=c('Sales', 'Profit', 'Discount', 'Quantity'), selected='Sales'),
  selectInput('yvar', 'Y-Axis Variable', choices=c('Sales', 'Profit', 'Discount', 'Quantity'), selected='Profit'),
  selectInput('scatter_color', 'Color By', choices=c('Segment', 'Category', 'Region')),
  plotOutput('scatter'),
  
  dataTableOutput('source_data')
  
)


##############################
# SERVER (ALL THE RENDERING) #
##############################

server = function(input, output){
  
  
  # Bar plot of sales by state
  output$sales_bar = renderPlot({
    rows_i_want = TRUE
    if(input$sales_year != 'All'){rows_i_want = rows_i_want & d$Year == input$sales_year}
    d[rows_i_want, ] %>%
      group_by(State) %>%
      summarise(Sales = sum(Sales)) %>%
      top_n(10) %>%
      ggplot(aes(y=reorder(State, Sales), x=Sales)) +
      geom_col(
        fill = input$bar_color, 
        color=input$bar_border
        ) +
      labs(y='') + 
      scale_x_continuous('Total Sales', labels=dollar) +
      theme(
        axis.text = element_text(face='bold', size=18),
        text = element_text(face='bold', size=18)
      )
  })
  
  # user defined scatter plot
  output$scatter = renderPlot(
    ggplot(d, aes_string(x=input$xvar, y=input$yvar, color=input$scatter_color)) +
      geom_jitter()
  )
  
  # output source data
  output$source_data = renderDataTable(d)
  
}

shinyApp(ui, server)

