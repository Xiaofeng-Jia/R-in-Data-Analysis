

# TODAY:
#   3 levels of interactivity
#     - 1) Changing aesthetics
#     - 2) Changing columns selected
#     - 3) Filtering rows (maybe, this one is tough, we might save for Day 11)
#
#   Basic formatting
#     - sidebarlayout() today, I'll provide templates for some other stuff



#library(stringr)
#str_subset(ls('package:shiny'), 'render')
#str_subset(ls('package:shiny'), 'Output')
#str_subset(ls('package:shiny'), 'Input')


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
#convert date to date and add a year variable


# plot 1 - bar plot of sales by state (level 1: let user set colors, level 3: filter for a particular year)
d %>%
  group_by(State) %>%
  summarise(Sales = sum(Sales)) %>%
  top_n(10) %>%
  ggplot(aes(y=reorder(State, Sales), x=Sales)) +
  geom_col() +
  labs(y='') + 
  scale_x_continuous('Total Sales', labels=dollar) + 
  theme(
    axis.text = element_text(face='bold', size=18),
    text = element_text(face='bold', size=18)
  )

# plot 2 - scatterplot of profit by sales, color by region (level 2: let user change x, y, color)
# maybe let them update point size as well?
ggplot(d, aes(x=Sales, y=Profit, color=Region)) +
  geom_point()







#ui
#server
#shinyApp




#ui (all the staff we see on the app)

ui = fluidPage(
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput('bar_fill', 'Bar Color', choices = colors(), selected = 'darkorange' ),
      selectInput('barcolor', 'Border Color', choices = colors(), selected = 'black' ),
      selectInput('xvar', 'X-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity')),
      selectInput('yvar', 'Y-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity'), selected = 'Profit'),
      selectInput('color_var',  'Color By?', choices = c('Segment', 'Region', 'Catagory')),
    ),#input,
    mainPanel(
    plotOutput('sales_bar'), 
    plotOutput('scatter'),
    dataTableOutput('source_data')
  )#close main panel
    
  ) #closes siderbarlayout
  )# close fluide page
  
  
  
  #3) add inputs for the user to interact with (using *Inputs functions)
  selectInput('bar_fill', 'Bar Color', choices = colors(), selected = 'darkorange' ),
  selectInput('barcolor', 'Border Color', choices = colors(), selected = 'black' ),
  #2) display whatever you build (using *Outputs functions) (see the cheatsheet)  
  plotOutput('sales_bar'), 
  
  
  selectInput('xvar', 'X-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity')),
  selectInput('yvar', 'Y-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity'), selected = 'Profit'),
  selectInput('color_var',  'Color By?', choices = c('Segment', 'Region', 'Catagory')),
  
  plotOutput('scatter'),
  dataTableOutput('source_data'),
  
  
  
  radioButtons('sales_year', 'Sales Year',  = c(2016, 2017, 2018, 2019, all))
)


#server: build all the things we want to see on the app
#1) build something to display (using render*)
#sales barplot
server = function(input, output){
  
  output$sales_bar = renderPlot({
    
    
    
    rows_i_want = TRUE
    if(input$sales_year != 'All'){ rows_i_want = rows_i_want & d$Year == input$sales_year} %>%
    d[rows_i_want , ] %>%
      #filter(Year == input$sales_year) %>%
      
      
      group_by(State) %>%
      summarise(Sales = sum(Sales)) %>%
      top_n(10) %>%
      
      ggplot(aes(y=reorder(State, Sales), x=Sales)) +
      geom_col(fill = input$bar_fill, color = input$barcolor) +
      labs(y='') + 
      scale_x_continuous('Total Sales', labels=dollar) + 
      theme(
        axis.text = element_text(face='bold', size=18),
        text = element_text(face='bold', size=18)
        )
  })
  
  
  #user defined scatter. plot
  output$scatter = renderPlot(
    ggplot(d, aes_string(x=input$xvar, y= input$yvar, color=input$color_var )) +
      geom_point()
    )
  
  
  #output source data
  output$source_data = renderDataTable(d)

}

#shinyApp
shinyApp(ui, server)


d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/superstore.csv') %>%
  mutate(
    Date = mdy(Date),
    Year = year(Date)
  )









